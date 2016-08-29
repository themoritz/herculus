{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Handler.Rest where

import           Control.Monad                  (unless, void)

import           Data.List                      (union)
import           Data.Maybe                     (mapMaybe)
import           Data.Monoid
import           Data.Text                      (Text, pack)
import           Data.Traversable

import           Servant

import           Database.MongoDB               ((=:))

import           Lib.Api.Rest
import           Lib.Api.WebSocket
import           Lib.Compiler
import           Lib.Compiler.Typechecker.Types
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Types
import           Lib.Types

import           Monads
import           Propagate

handle :: MonadHexl m => ServerT Routes m
handle =
       handleProjectCreate
  :<|> handleProjectList

  :<|> handleTableCreate
  :<|> handleTableList
  :<|> handleTableListGlobal
  :<|> handleTableData
  :<|> handleTableGetWhole

  :<|> handleColumnCreate
  :<|> handleColumnDelete
  :<|> handleColumnSetName
  :<|> handleColumnSetDataType
  :<|> handleColumnSetInput
  :<|> handleColumnList

  :<|> handleRecordCreate
  :<|> handleRecordDelete
  :<|> handleRecordData
  :<|> handleRecordList
  :<|> handleRecordListWithData

  :<|> handleCellSet

--

handleProjectCreate :: MonadHexl m => Project -> m (Id Project)
handleProjectCreate = create

handleProjectList :: MonadHexl m => m [Entity Project]
handleProjectList = listAll

--

handleTableCreate :: MonadHexl m => Table -> m (Id Table)
handleTableCreate = create

handleTableList :: MonadHexl m => Id Project -> m [Entity Table]
handleTableList projId = listByQuery [ "projectId" =: toObjectId projId ]

handleTableListGlobal :: MonadHexl m => m [Entity Table]
handleTableListGlobal = listByQuery [ ]

handleTableData :: MonadHexl m => Id Table -> m [(Id Column, Id Record, CellContent)]
handleTableData tblId = do
  cells <- listByQuery [ "aspects.tableId" =: toObjectId tblId ]
  let go (Cell v (Aspects _ c r)) = (c, r, v)
  pure $ map (go . entityVal) cells

handleTableGetWhole :: MonadHexl m => Id Table
                    -> m ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])
handleTableGetWhole tblId = do
  (,,) <$> handleColumnList tblId
       <*> handleRecordList tblId
       <*> handleTableData tblId

--

handleColumnCreate :: MonadHexl m => Id Table -> m (Entity Column, [Entity Cell])
handleColumnCreate t = do
  let newCol = Column t "" DataString ColumnInput "" CompileResultNone
  c <- create newCol
  rs <- listByQuery [ "tableId" =: toObjectId t ]
  cells <- for rs $ \e -> do
    let cell = newCell t c (entityId e) (defaultContent DataString)
    i <- create cell
    pure $ Entity i cell
  pure (Entity c newCol, cells)

handleColumnDelete :: MonadHexl m => Id Column -> m ()
handleColumnDelete colId = do
  delete colId
  deleteByQuery (Proxy :: Proxy Cell)
    [ "aspects.columnId" =: toObjectId colId
    ]
  newChilds <- compileColumnChildren colId
  void $ modifyDependencies colId []
  sendWS $ WsDownColumnsChanged newChilds
  propagate [RootWholeColumns $ map entityId newChilds]

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName c name = do
  update c $ \col -> col { columnName = name }
  newChilds <- compileColumnChildren c
  sendWS $ WsDownColumnsChanged newChilds
  propagate [RootWholeColumns $ map entityId newChilds]

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
    propagate [RootWholeColumns $ map entityId toUpdate]

handleColumnSetInput :: MonadHexl m => Id Column -> (InputType, Text) -> m ()
handleColumnSetInput c (typ, code) = do
  update c $ \col -> col { columnInputType = typ
                         , columnSourceCode = code }
  case typ of
    ColumnDerived -> do newCol <- compileColumn c
                        sendWS $ WsDownColumnsChanged [newCol]
                        propagate [RootWholeColumns [c]]
    _             -> pure ()

handleColumnList :: MonadHexl m => Id Table -> m [Entity Column]
handleColumnList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleRecordCreate :: MonadHexl m => Id Table -> m (Entity Record, [Entity Cell])
handleRecordCreate t = do
  let newRec = Record t
  r <- create newRec
  cs <- listByQuery [ "tableId" =: toObjectId t ]
  newCells <- for cs $ \e -> do
    let cell = newCell t (entityId e) r $ defaultContent $ columnDataType $ entityVal e
    i <- create cell
    pure $ Entity i cell
  propagate
    [ RootCellChanges $
        mapMaybe (\(Entity c col) -> case columnInputType col of
                     ColumnInput -> Just (c, r)
                     ColumnDerived -> Nothing
                 ) cs
    , RootSpecificCells $
        mapMaybe (\(Entity c col) -> case columnInputType col of
                     ColumnInput -> Nothing
                     ColumnDerived -> Just (c, r)
                 ) cs
    ]
  pure (Entity r newRec, newCells)

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
  propagate [RootWholeColumns allChildren]

handleRecordData :: MonadHexl m => Id Record -> m [(Entity Column, CellContent)]
handleRecordData recId = do
  cells <- listByQuery
    [ "aspects.recordId" =: toObjectId recId ]
  for cells $ \(Entity _ cell) -> do
    let i = aspectsColumnId $ cellAspects cell
    col <- getById' i
    pure (Entity i col, cellContent cell)

handleRecordList :: MonadHexl m => Id Table -> m [Entity Record]
handleRecordList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

handleRecordListWithData :: MonadHexl m => Id Table -> m [(Id Record, [(Text, CellContent)])]
handleRecordListWithData tblId = do
  recs <- listByQuery [ "tableId" =: toObjectId tblId ]
  for recs $ \r -> do
    dat <- handleRecordData (entityId r)
    let entries = map (\(col, content) -> (columnName $ entityVal col, content)) dat
    pure (entityId r, entries)

--

handleCellSet :: MonadHexl m => Id Column -> Id Record -> Value -> m ()
handleCellSet c r val = do
  let query =
        [ "aspects.columnId" =: toObjectId c
        , "aspects.recordId" =: toObjectId r
        ]
  updateByQuery' query $ \cell -> cell { cellContent = CellValue val }
  -- TODO: fork this
  propagate [RootCellChanges [(c, r)]]

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
      colTyp <- typeOfDataType getTableRows (columnDataType col)
      if typ /= colTyp
        then abort $ pack $
               "Inferred type `" <> show typ <>
               "` does not match column type `" <>
               show colTyp <> "`."
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
  col <- getById' c
  changes <- for cells $ \e -> do
    let defContent = defaultContent (columnDataType col)
    update (entityId e) $ \cell -> cell { cellContent = defContent }
    let aspects = cellAspects $ entityVal e
    pure (aspectsColumnId aspects, aspectsRecordId aspects, defContent)
  sendWS $ WsDownCellsChanged changes

--

mkTypecheckEnv :: forall m. MonadHexl m => Id Table -> TypecheckEnv m
mkTypecheckEnv ownTblId = TypecheckEnv
    { envResolveColumnRef = resolveColumnRef ownTblId

    , envResolveColumnOfTableRef = \tblName colName -> do
        tableRes <- getOneByQuery [ "name" =: tblName ]
        case tableRes of
          Left _ -> pure Nothing
          Right (Entity i _) -> resolveColumnRef i colName

    , envResolveTableRef = \tblName -> do
        tableRes <- getOneByQuery [ "name" =: tblName ]
        case tableRes of
          Left _ -> pure Nothing
          Right (Entity tblId _) -> do
            cols <- listByQuery [ "tableId" =: toObjectId tblId ]
            pure $ Just (tblId, cols)

    , envGetTableRows = getTableRows

    , envOwnTableId = ownTblId

    }
  where
    resolveColumnRef :: Id Table -> Ref Column -> m (Maybe (Entity Column))
    resolveColumnRef tbl colName = do
      let colQuery =
            [ "name" =: colName
            , "tableId" =: toObjectId tbl
            ]
      columnRes <- getOneByQuery colQuery
      case columnRes of
        Left _  -> pure Nothing
        Right e -> pure $ Just e

getTableRows :: MonadHexl m => Id Table -> m Type
getTableRows t = do
  cols <- listByQuery [ "tableId" =: toObjectId t ]
  let toRow [] = pure TyNoRow
      toRow ((Entity _ c):rest) = TyRow (Ref $ columnName c)
                                    <$> typeOfDataType getTableRows (columnDataType c)
                                    <*> toRow rest
  toRow cols

--
