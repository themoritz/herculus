{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Rest where

import           Control.Monad                  (unless, void)

import           Data.List                      (union)
import qualified Data.Map                       as Map
import           Data.Maybe                     (mapMaybe)
import           Data.Monoid
import           Data.Text                      (Text, pack)
import           Data.Traversable

import           Servant

import           Database.MongoDB               ((=:))

import           Lib.Api.Rest
import           Lib.Api.WebSocket
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Types
import           Lib.Types
import           Lib.Compiler
import           Lib.Compiler.Typechecker.Types

import           Monads
import           Propagate

handle :: MonadHexl m => ServerT Routes m
handle =
       handleProject
  :<|> handleTable
  :<|> handleColumn
  :<|> handleRecord
  :<|> handleCell

--

handleProject :: MonadHexl m => ServerT ProjectRoutes m
handleProject =
       handleProjectCreate
  :<|> handleProjectList

handleProjectCreate :: MonadHexl m => Project -> m (Id Project)
handleProjectCreate = create

handleProjectList :: MonadHexl m => m [Entity Project]
handleProjectList = listAll

--

handleTable :: MonadHexl m => ServerT TableRoutes m
handleTable =
       handleTableCreate
  :<|> handleTableList
  :<|> handleTableData

handleTableCreate :: MonadHexl m => Table -> m (Id Table)
handleTableCreate = create

handleTableList :: MonadHexl m => Id Project -> m [Entity Table]
handleTableList projId = listByQuery [ "projectId" =: toObjectId projId ]

handleTableData :: MonadHexl m => Id Table -> m [(Id Column, Id Record, CellContent)]
handleTableData tblId = do
  cells <- listByQuery [ "aspects.tableId" =: toObjectId tblId ]
  let go (Cell v (Aspects _ c r)) = (c, r, v)
  pure $ map (go . entityVal) cells

--

handleColumn :: MonadHexl m => ServerT ColumnRoutes m
handleColumn =
       handleColumnCreate
  :<|> handleColumnDelete
  :<|> handleColumnSetName
  :<|> handleColumnSetDataType
  :<|> handleColumnSetInput
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column, [Entity Cell])
handleColumnCreate t = do
  c <- create $ Column t "" DataString ColumnInput "" CompileResultNone
  rs <- listByQuery [ "tableId" =: toObjectId t ]
  cells <- for rs $ \e -> do
    let cell = emptyCell t c (entityId e)
    i <- create cell
    pure $ Entity i cell
  pure (c, cells)

handleColumnDelete :: MonadHexl m => Id Column -> m ()
handleColumnDelete colId = do
  delete colId
  deleteByQuery (Proxy :: Proxy Cell)
    [ "aspects.columnId" =: toObjectId colId
    ]
  newChilds <- compileColumnChildren colId
  void $ modifyDependencies colId []
  sendWS $ WsDownColumnsChanged newChilds
  propagate $ RootWholeColumns $ map entityId newChilds

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName c name = do
  update c $ \col -> col { columnName = name }
  newChilds <- compileColumnChildren c
  sendWS $ WsDownColumnsChanged newChilds
  propagate $ RootWholeColumns $ map entityId newChilds

handleColumnSetDataType :: MonadHexl m => Id Column -> DataType -> m ()
handleColumnSetDataType c typ = do
  oldCol <- getById' c
  update c $ \col -> col { columnDataType = typ }
  unless (columnDataType oldCol == typ) $ do
    newChilds <- compileColumnChildren c
    toUpdate <- case columnInputType oldCol of
      ColumnInput   -> do invalidateCells c
                          pure newChilds
      ColumnDerived -> do newC <- compileColumn c
                          pure $ newC:newChilds
    sendWS $ WsDownColumnsChanged toUpdate
    propagate $ RootWholeColumns $ map entityId toUpdate

handleColumnSetInput :: MonadHexl m => Id Column -> (InputType, Text) -> m ()
handleColumnSetInput c (typ, code) = do
  update c $ \col -> col { columnInputType = typ
                         , columnSourceCode = code }
  case typ of
    ColumnDerived -> do newCol <- compileColumn c
                        sendWS $ WsDownColumnsChanged [newCol]
                        propagate $ RootWholeColumns [c]
    _             -> pure ()

handleColumnList :: MonadHexl m => Id Table -> m [Entity Column]
handleColumnList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleRecord :: MonadHexl m => ServerT RecordRoutes m
handleRecord =
       handleRecordCreate
  :<|> handleRecordDelete
  :<|> handleRecordList

handleRecordCreate :: MonadHexl m => Id Table -> m (Id Record, [Entity Cell])
handleRecordCreate t = do
  r <- create $ Record t
  cs <- listByQuery [ "tableId" =: toObjectId t ]
  maybes <- for cs $ \e -> do
    let cell = emptyCell t (entityId e) r
    i <- create cell
    pure $ case columnInputType $ entityVal e of
      ColumnInput -> Just $ Entity i cell
      ColumnDerived -> Nothing
  let cells = mapMaybe id maybes
  propagate $ RootCellChanges $ map (\e -> (aspectsColumnId $ cellAspects $ entityVal e, r)) cells
  pure (r, cells)

handleRecordDelete :: MonadHexl m => Id Record -> m ()
handleRecordDelete recId = do
  record <- getById' recId
  delete recId
  deleteByQuery (Proxy :: Proxy Cell)
    [ "aspects.recordId" =: toObjectId recId
    ]
  graph <- getDependencies
  cs <- listByQuery [ "tableId" =: toObjectId (recordTableId record) ]
  let allChildren = foldr union [] $
        map (\e -> map fst .
                   filter (\(_, typ) -> typ == OneToAll) .
                   getChildren (entityId e) $ graph) cs
  propagate $ RootWholeColumns allChildren

handleRecordList :: MonadHexl m => Id Table -> m [Entity Record]
handleRecordList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleCell :: MonadHexl m => ServerT CellRoutes m
handleCell =
       handleCellSet

handleCellSet :: MonadHexl m => Id Column -> Id Record -> Value -> m ()
handleCellSet c r val = do
  let query =
        [ "aspects.columnId" =: toObjectId c
        , "aspects.recordId" =: toObjectId r
        ]
  updateByQuery' query $ \cell -> cell { cellContent = CellValue val }
  -- TODO: fork this
  propagate $ RootCellChanges [(c, r)]

-- Helper ----------------------------

compileColumn :: MonadHexl m => Id Column -> m (Entity Column)
compileColumn c = do
  col <- getById' c
  let abort msg = do
        void $ modifyDependencies c []
        pure $ CompileResultError msg
  res <- compile (columnSourceCode col) (mkTypecheckEnv $ columnTableId col)
  compileResult <- case res of
    Left msg -> abort msg
    Right (expr ::: typ) -> do
      if typ /= typeOfDataType (columnDataType col)
        then abort $ pack $
               "Inferred type `" <> show typ <>
               "` does not match column type `" <>
               show (columnDataType col) <> "`"
        else do
          let deps = collectDependencies expr
          cycles <- modifyDependencies c deps
          if cycles then abort "Dependency graph has cycles"
                    else pure $ CompileResultCode expr
  update c $ \col' -> col' { columnCompileResult = compileResult }
  pure $ Entity c col { columnCompileResult = compileResult }

compileColumnChildren :: MonadHexl m => Id Column -> m [Entity Column]
compileColumnChildren c = do
  graph <- getDependencies
  for (getChildren c graph) $ \(child, _) -> do
    void $ compileColumn child
    childCol <- getById' child
    pure $ Entity child childCol

invalidateCells :: MonadHexl m => Id Column -> m ()
invalidateCells c = do
  cells <- listByQuery [ "aspects.columnId" =: toObjectId c]
  changes <- for cells $ \e -> do
    update (entityId e) $ \cell -> cell { cellContent = CellNothing }
    let aspects = cellAspects $ entityVal e
    pure (aspectsColumnId aspects, aspectsRecordId aspects, CellNothing)
  sendWS $ WsDownCellsChanged changes

--

mkTypecheckEnv :: MonadHexl m => Id Table -> TypecheckEnv m
mkTypecheckEnv ownTblId = TypecheckEnv
    { envResolveColumnRef = resolveColumnRef ownTblId

    , envResolveColumnOfTableRef = \tblName colName -> do
        tableRes <- getOneByQuery [ "name" =: tblName ]
        case tableRes of
          Left _ -> pure Nothing
          Right (Entity i _) -> (fmap.fmap) (i,) $ resolveColumnRef i colName

    , envResolveTableRef = \tblName -> do
        tableRes <- getOneByQuery [ "name" =: tblName ]
        case tableRes of
          Left _ -> pure Nothing
          Right (Entity tblId _) -> do
            cols <- listByQuery [ "tableId" =: toObjectId tblId ]
            let l = map (\(Entity _ c) ->
                           ( Ref $ columnName c
                           , typeOfDataType $ columnDataType c
                           )
                        ) cols
            pure $ Just (tblId, Map.fromList l)

    , envOwnTableId = ownTblId

    }
  where
    resolveColumnRef tbl colName = do
      let colQuery =
            [ "name" =: colName
            , "tableId" =: toObjectId tbl
            ]
      columnRes <- getOneByQuery colQuery
      case columnRes of
        Left _  -> pure Nothing
        Right e -> pure $ Just e

--
