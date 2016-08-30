{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Handler.Rest where

import           Control.Monad                  (unless, void)
import           Control.Arrow                  ((***))

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
import           Lib.Model.References
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
    defContent <- defaultContent newCol
    let cell = newCell t c (entityId e) defContent
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
  modifyReferences $ removeReference colId
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
                          updateReference c typ
                          pure newChilds
      ColumnDerived -> do newC <- compileColumn c
                          pure $ newC:newChilds
    sendWS $ WsDownColumnsChanged toUpdate
    propagate [RootWholeColumns $ map entityId toUpdate]

handleColumnSetInput :: MonadHexl m => Id Column -> (InputType, Text) -> m ()
handleColumnSetInput c (typ, code) = do
  oldCol <- getById' c
  update c $ const $ oldCol { columnInputType = typ
                            , columnSourceCode = code
                            }
  case typ of
    ColumnDerived -> do newCol <- compileColumn c
                        sendWS $ WsDownColumnsChanged [newCol]
                        propagate [RootWholeColumns [c]]
    ColumnInput   -> updateReference c (columnDataType oldCol)

handleColumnList :: MonadHexl m => Id Table -> m [Entity Column]
handleColumnList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

--

handleRecordCreate :: MonadHexl m => Id Table -> m (Entity Record, [Entity Cell])
handleRecordCreate t = do
  let newRec = Record t
  r <- create newRec
  cs <- listByQuery [ "tableId" =: toObjectId t ]
  newCells <- for cs $ \e@(Entity c col) -> do
    defContent <- defaultContent col
    let cell = newCell t c r defContent
    i <- create cell
    pure $ case columnInputType col of
      ColumnInput -> Just (e, Entity i cell)
      ColumnDerived -> Nothing
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
  sendWS $ WsDownRecordCreated t r $
    map (id *** cellContent . entityVal) $ mapMaybe id newCells
  pure (Entity r newRec, map snd $ mapMaybe id newCells)

handleRecordDelete :: MonadHexl m => Id Record -> m ()
handleRecordDelete recId = do
  record <- getById' recId
  delete recId
  sendWS $ WsDownRecordDeleted (recordTableId record) recId
  deleteByQuery (Proxy :: Proxy Cell)
    [ "aspects.recordId" =: toObjectId recId
    ]
  graph <- getDependencies
  cs <- listByQuery [ "tableId" =: toObjectId (recordTableId record) ]
  let allChildren = foldr union [] $
        map (\e -> map fst .
                   filter (\(_, typ) -> typ == OneToAll) .
                   getChildren (entityId e) $ graph) cs
  -- Invalidate references
  refGraph <- getReferences
  let refingCols = getReferringColumns (recordTableId record) refGraph
  mCellChanges <- for refingCols $ \c -> do
    cells <- listByQuery [ "aspects.columnId" =: toObjectId c ]
    for cells $ \(Entity i cell) -> case cellContent cell of
      CellError _ -> pure Nothing
      CellValue val -> case invalidateRecord recId val of
        Nothing -> pure Nothing
        Just newVal -> do
          let updatedCell = cell { cellContent = CellValue newVal }
          update i $ const updatedCell
          pure $ Just updatedCell
  let cellChanges = concat $ map (mapMaybe id) mCellChanges
  sendWS $ WsDownCellsChanged cellChanges
  propagate
    [ RootWholeColumns allChildren
    , RootCellChanges $ map (\(Cell _ (Aspects _ c r)) -> (c, r)) cellChanges
    ]

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

handleRecordListWithData :: MonadHexl m => Id Table -> m [(Id Record, [(Entity Column, CellContent)])]
handleRecordListWithData tblId = do
  recs <- listByQuery [ "tableId" =: toObjectId tblId ]
  for recs $ \r -> (entityId r,) <$> handleRecordData (entityId r)

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
  changes <- for cells $ \(Entity i cell) -> do
    defContent <- defaultContent col
    let invalidatedCell = Cell defContent (cellAspects cell)
    update i $ const invalidatedCell
    pure invalidatedCell
  sendWS $ WsDownCellsChanged changes

updateReference :: MonadHexl m => Id Column -> DataType -> m ()
updateReference c dataType = case getReference dataType of
  Nothing -> modifyReferences $ removeReference c
  Just t  -> modifyReferences $ setReference t c

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

-- TODO: configurable by user
defaultContent :: MonadHexl m => Column -> m CellContent
defaultContent col = case columnDataType col of
  DataBool     -> pure . CellValue $ VBool False
  DataString   -> pure . CellValue $ VString ""
  DataNumber   -> pure . CellValue $ VNumber 0
  DataTime     -> CellValue . VTime <$> getCurrentTime
  DataRecord t -> do
    res <- getOneByQuery [ "tableId" =: toObjectId t ]
    pure $ CellValue $ VRecord $ case res of
      Left _ -> Nothing
      Right (Entity i _) -> Just i
  DataList _   -> pure . CellValue $ VList []
  DataMaybe _  -> pure . CellValue $ VMaybe Nothing

--
