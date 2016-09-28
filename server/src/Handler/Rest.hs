{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Handler.Rest where

import           Control.Arrow                  (second)
import           Control.Lens
import           Control.Monad                  (unless, void, when)

import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as BL8
import           Data.List                      (union)
import           Data.Maybe                     (catMaybes, isNothing, mapMaybe)
import           Data.Monoid
import           Data.Text                      (Text, pack, unpack)
import           Data.Traversable

import qualified Text.Pandoc                    as Pandoc
import qualified Text.Pandoc.Error              as Pandoc

import           Servant

import           Database.MongoDB               ((=:))

import           Lib.Api.Rest
import           Lib.Api.WebSocket
import           Lib.Compiler
import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Project
import           Lib.Model.Record
import           Lib.Model.References
import           Lib.Model.Table
import           Lib.Template
import           Lib.Template.Interpreter
import           Lib.Template.Types
import           Lib.Types

import           Cache
import           Monads
import           Propagate

handle :: MonadHexl m => ServerT Routes m
handle =
       handleProjectCreate
  :<|> handleProjectList
  :<|> handleProjectSetName
  :<|> handleProjectDelete

  :<|> handleTableCreate
  :<|> handleTableList
  :<|> handleTableListGlobal
  :<|> handleTableData
  :<|> handleTableGetWhole
  :<|> handleTableSetName
  :<|> handleTableDelete

  :<|> handleColumnCreate
  :<|> handleColumnDelete
  :<|> handleColumnList
  :<|> handleColumnSetName

  :<|> handleDataColUpdate

  :<|> handleReportColUpdate

  :<|> handleRecordCreate
  :<|> handleRecordDelete
  :<|> handleRecordData
  :<|> handleRecordList
  :<|> handleRecordListWithData

  :<|> handleCellSet
  :<|> handleCellGetReportPDF
  :<|> handleCellGetReportHTML
  :<|> handleCellGetReportPlain

--

handleProjectCreate :: MonadHexl m => Project -> m (Id Project)
handleProjectCreate = create

handleProjectList :: MonadHexl m => m [Entity Project]
handleProjectList = listAll

handleProjectSetName :: MonadHexl m => Id Project -> Text -> m ()
handleProjectSetName projectId name = do
  project <- getById' projectId
  let updatedProject = project & projectName .~ name
  update projectId $ const updatedProject


handleProjectDelete :: MonadHexl m => Id Project -> m ()
handleProjectDelete projectId = do
    tables <- handleTableList projectId
    _ <- mapM (handleTableDelete . entityId) tables
    delete projectId

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
handleTableGetWhole tblId =
  (,,) <$> handleColumnList tblId
       <*> handleRecordList tblId
       <*> handleTableData tblId

handleTableSetName :: MonadHexl m => Id Table -> Text -> m ()
handleTableSetName tblId name = do
  table <- getById' tblId
  let table' = table & tableName .~ name
  update tblId $ const table'

handleTableDelete :: MonadHexl m => Id Table -> m ()
handleTableDelete tableId = do
    delete tableId
    deleteByQuery (Proxy::Proxy Column) query
    deleteByQuery (Proxy::Proxy Cell) query
    deleteByQuery (Proxy::Proxy Record) query
  where
    query = [ "tableId" =: toObjectId tableId]

--

handleColumnCreate :: MonadHexl m => Column -> m (Entity Column, [Entity Cell])
handleColumnCreate newCol = do
  c <- create newCol
  case newCol ^. columnKind of
    ColumnData dataCol -> do
      let t = newCol ^. columnTableId
      rs <- listByQuery [ "tableId" =: toObjectId t ]
      cells <- for rs $ \e -> do
        defContent <- defaultContent (dataCol ^. dataColType)
        let cell = newCell t c (entityId e) defContent
        i <- create cell
        pure $ Entity i cell
      pure (Entity c newCol, cells)
    ColumnReport _ -> pure (Entity c newCol, [])

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

handleColumnList :: MonadHexl m => Id Table -> m [Entity Column]
handleColumnList tblId = listByQuery [ "tableId" =: toObjectId tblId ]

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName c name = do
  update c $ columnName .~ name
  newChilds <- compileColumnChildren c
  sendWS $ WsDownColumnsChanged newChilds
  propagate [RootWholeColumns $ map entityId newChilds]

--

handleDataColUpdate :: MonadHexl m => Id Column -> (DataType, IsDerived, Text) -> m ()
handleDataColUpdate c (typ, derived, code) = do
  oldCol <- getById' c
  case oldCol ^? columnKind . _ColumnData of
    Nothing -> throwError $ ErrBug "Called dataColUpdate for column other than data"
    Just dataCol -> do
      let updatedCol = oldCol & columnKind . _ColumnData %~ (dataColType .~ typ)
                                           . (dataColIsDerived .~ derived)
                                           . (dataColSourceCode .~ code)
      update c $ const updatedCol

      updatedChilds <- if dataCol ^. dataColType == typ
        then pure []
        else compileColumnChildren c

      (newSelf, propSelf) <- case derived of
        Derived -> do
          newC <- compileColumn c
          pure ([newC], [c])
        NotDerived -> do
          renewErrorCells c
          unless (typ == dataCol ^. dataColType) $ do
            updateReference c typ
            invalidateCells c
          pure ([Entity c updatedCol], [])

      sendWS $ WsDownColumnsChanged $ newSelf <> updatedChilds
      propagate [RootWholeColumns $ propSelf <> map entityId updatedChilds]

handleReportColUpdate :: MonadHexl m => Id Column -> (Text, ReportFormat, Maybe ReportLanguage) -> m ()
handleReportColUpdate c (template, format, lang) = do
  oldCol <- getById' c
  when (isNothing (oldCol ^? columnKind . _ColumnReport)) $
    throwError $ ErrBug "Called ReportColUpdate for column other than report"
  update c $ columnKind . _ColumnReport %~ (reportColTemplate .~ template)
                                         . (reportColFormat .~ format)
                                         . (reportColLanguage .~ lang)
  newCol <- compileColumn c
  sendWS $ WsDownColumnsChanged [newCol]

--

handleRecordCreate :: MonadHexl m => Id Table -> m (Entity Record, [Entity Cell])
handleRecordCreate t = do
  let newRec = Record t
  r <- create newRec
  cols <- listByQuery [ "tableId" =: toObjectId t ]
  newCells <- for cols $ \e@(Entity c col) ->
    case col ^? columnKind . _ColumnData of
      Nothing -> pure Nothing
      Just dataCol -> do
        defContent <- defaultContent (dataCol ^. dataColType)
        let cell = newCell t c r defContent
        i <- create cell
        case dataCol ^. dataColIsDerived of
          NotDerived -> pure $ Just (e, Entity i cell)
          Derived    -> pure Nothing
  propagate
    [ RootCellChanges $
        mapMaybe (\(Entity c col) ->
                     case col ^? columnKind . _ColumnData . dataColIsDerived of
                       Just NotDerived -> Just (c, r)
                       _               -> Nothing
                 ) cols
    , RootSpecificCells $
        mapMaybe (\(Entity c col) ->
                      case col ^? columnKind . _ColumnData . dataColIsDerived of
                        Just Derived -> Just (c, r)
                        _            -> Nothing
                 ) cols
    ]
  sendWS $ WsDownRecordCreated t r $
    map (second $ cellContent . entityVal) $ catMaybes newCells
  pure (Entity r newRec, map snd $ catMaybes newCells)

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
  let allChildren = foldr (union . (\e ->
                                      map fst .
                                      filter (\(_, typ) -> typ == OneToAll) .
                                      getChildren (entityId e) $ graph)) [] cs
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
  let cellChanges = concatMap catMaybes mCellChanges
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

handleRecordListWithData :: MonadHexl m => Id Table
                         -> m [(Id Record, [(Entity Column, CellContent)])]
handleRecordListWithData tblId = do
  recs <- listByQuery [ "tableId" =: toObjectId tblId ]
  for recs $ \r -> (entityId r,) <$> handleRecordData (entityId r)

--

handleCellSet :: MonadHexl m => Id Column -> Id Record -> Value -> m ()
handleCellSet c r val = do
  Entity i cell <- getOneByQuery'
    [ "aspects.columnId" =: toObjectId c
    , "aspects.recordId" =: toObjectId r
    ]
  let changedCell = cell { cellContent = CellValue val }
  update i $ const changedCell
  sendWS $ WsDownCellsChanged [changedCell]
  -- TODO: fork this
  propagate [RootCellChanges [(c, r)]]

handleCellGetReportPDF :: MonadHexl m => Id Column -> Id Record -> m BL.ByteString
handleCellGetReportPDF c r = do
  (repCol, plain) <- evalReport c r
  case repCol ^. reportColLanguage of
    Nothing -> throwError $ ErrUser "Cannot generate PDF from plain text"
    Just lang -> case getPandocReader lang (repCol ^. reportColFormat) of
      Nothing -> do
        let options = Pandoc.def
        runLatex options (unpack plain) >>= \case
          Left e -> throwError $ ErrUser $
            "Error running pdflatex: " <> (pack . BL8.unpack) e <>
            "Source: " <> plain
          Right pdf -> pure pdf
      Just reader -> case reader Pandoc.def (unpack plain) of
        Left err -> throwError $ ErrUser $
          "Could not read generated code into pandoc document: " <> (pack . show) err
        Right pandoc -> do
          template <- getDefaultTemplate "latex" >>= \case
            Left msg -> throwError $ ErrBug $
              "Could not load latex template: " <> pack msg
            Right template -> pure template
          let options = Pandoc.def
                { Pandoc.writerStandalone = True
                , Pandoc.writerTemplate = template
                , Pandoc.writerVariables =
                  [ ("papersize", "A4")
                  , ("fontsize", "12pt")
                  , ("geometry", "margin=3cm")
                  , ("fontfamily", "lato")
                  , ("fontfamilyoptions", "default")
                  ]
                }
          makePDF options pandoc >>= \case
            Left e -> throwError $ ErrUser $
              "Error generating PDF: " <> (pack . BL8.unpack) e <>
              "Source: " <> (pack . show) (Pandoc.writeLaTeX options pandoc)
            Right pdf -> pure pdf

handleCellGetReportHTML :: MonadHexl m => Id Column -> Id Record -> m Text
handleCellGetReportHTML c r = do
  col <- getById' c
  (repCol, plain) <- evalReport c r
  case repCol ^. reportColLanguage of
    Nothing   -> pure plain
    Just lang -> case getPandocReader lang (repCol ^. reportColFormat) of
      Nothing -> pure plain
      Just reader -> case reader Pandoc.def (unpack plain) of
        Left err -> pure $ pack $ show err
        Right pandoc -> do
          template <- getDefaultTemplate "html5" >>= \case
            Left msg -> throwError $ ErrBug $ "Could not load html5 template: " <> pack msg
            Right template -> pure template
          let options = Pandoc.def
                { Pandoc.writerStandalone = True
                , Pandoc.writerTemplate = template
                , Pandoc.writerVariables =
                  [ ("pagetitle", unpack (col ^. columnName))
                  , ("title-prefix", "Report")
                  ]
                }
          pure $ pack $ Pandoc.writeHtmlString options pandoc

handleCellGetReportPlain :: MonadHexl m => Id Column -> Id Record -> m Text
handleCellGetReportPlain c r = snd <$> evalReport c r

getPandocReader :: ReportLanguage -> ReportFormat
  -> Maybe (Pandoc.ReaderOptions -> String -> Either Pandoc.PandocError Pandoc.Pandoc)
getPandocReader lang format = case lang of
  ReportLanguageMarkdown -> Just Pandoc.readMarkdown
  ReportLanguageLatex    -> case format of
    ReportFormatPDF -> Nothing
    _               -> Just Pandoc.readLaTeX
  ReportLanguageHTML     -> case format of
    ReportFormatHTML -> Nothing
    _                -> Just Pandoc.readHtml

-- Helper ----------------------------

evalReport :: MonadHexl m => Id Column -> Id Record -> m (ReportCol, Text)
evalReport c r = do
  col <- getById' c
  case col ^? columnKind . _ColumnReport of
    Nothing -> throwError $ ErrBug "Trying to get report for non report column"
    Just repCol -> case repCol ^. reportColCompiledTemplate of
      CompileResultOk ttpl -> fmap fst $ runCacheT $ do
        let env = EvalEnv
                    { envGetCellValue = flip getCellValue r
                    , envGetColumnValues = getColumnValues
                    , envGetTableRecords = getTableRecords
                    , envGetRecordValue = getRecordValue
                    }
        runEvalTemplate env ttpl >>= \case
          Left e -> pure (repCol, e)
          Right res -> pure (repCol, res)
      _ -> throwError $ ErrBug "Getting report for non compiled template."

-- | Compiles all kinds of columns
compileColumn :: forall m. MonadHexl m => Id Column -> m (Entity Column)
compileColumn c = do
  col <- getById' c
  let abort :: Text -> m (CompileResult a)
      abort msg = do
        void $ modifyDependencies c []
        pure $ CompileResultError msg
  newCol <- case col ^. columnKind of
    ColumnData dataCol -> do
      res <- compile (dataCol ^. dataColSourceCode) (mkTypecheckEnv $ col ^. columnTableId)
      compileResult <- case res of
        Left msg -> abort msg
        Right (expr ::: typ) -> do
          colTyp <- typeOfDataType getTableRows (dataCol ^. dataColType)
          if typ /= colTyp
            then abort $ pack $
                   "Inferred type `" <> show typ <>
                   "` does not match column type `" <>
                   show colTyp <> "`."
            else do
              let deps = collectDependencies expr
              cycles <- modifyDependencies c deps
              if cycles then abort "Dependency graph has cycles"
                        else pure $ CompileResultOk expr
      pure $ col & columnKind . _ColumnData . dataColCompileResult .~ compileResult
    ColumnReport repCol -> do
      res <- compileTemplate (repCol ^. reportColTemplate)
                             (mkTypecheckEnv $ col ^. columnTableId)
      compileResult <- case res of
        Left msg -> abort msg
        Right tTpl -> do
          let deps = collectTplDependencies tTpl
          cycles <- modifyDependencies c deps
          when cycles $ throwError $ ErrBug "Setting report dependencies generated cycle"
          pure $ CompileResultOk tTpl
      pure $ col & columnKind . _ColumnReport . reportColCompiledTemplate .~ compileResult
  update c $ const newCol
  pure $ Entity c newCol

compileColumnChildren :: MonadHexl m => Id Column -> m [Entity Column]
compileColumnChildren c = do
  graph <- getDependencies
  for (getChildren c graph) $ \(child, _) -> do
    void $ compileColumn child
    childCol <- getById' child
    pure $ Entity child childCol

invalidateCells :: MonadHexl m => Id Column -> m ()
invalidateCells c = do
  col <- getById' c
  dataCol <- case col ^? columnKind . _ColumnData of
    Nothing -> throwError $ ErrBug "Trying to invalidate cells of column other than data"
    Just dataCol -> pure dataCol
  cells <- listByQuery [ "aspects.columnId" =: toObjectId c]
  changes <- for cells $ \(Entity i cell) -> do
    defContent <- defaultContent (dataCol ^. dataColType)
    let invalidatedCell = Cell defContent (cellAspects cell)
    update i $ const invalidatedCell
    pure invalidatedCell
  sendWS $ WsDownCellsChanged changes

renewErrorCells :: MonadHexl m => Id Column -> m ()
renewErrorCells c = do
  col <- getById' c
  dataCol <- case col ^? columnKind . _ColumnData of
    Nothing -> throwError $ ErrBug "Trying to renewErrorCells of column other than data"
    Just dataCol -> pure dataCol
  cells <- listByQuery [ "aspects.columnId" =: toObjectId c]
  changes <- for cells $ \(Entity i cell) ->
    case cellContent cell of
      CellError _ -> do
        defContent <- defaultContent (dataCol ^. dataColType)
        let renewedCell = Cell defContent (cellAspects cell)
        update i $ const renewedCell
        pure $ Just renewedCell
      CellValue _ -> pure Nothing
  sendWS $ WsDownCellsChanged $ catMaybes changes
  propagate [ RootCellChanges $
                map (\(Cell _ (Aspects _ c' r')) -> (c', r')) $
                catMaybes changes
            ]

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
            let dataCols = flip mapMaybe cols $ \(Entity i col) ->
                  fmap (i,) (col ^? columnKind . _ColumnData)
            pure $ Just (tblId, dataCols)

    , envGetTableRows = getTableRows

    , envOwnTableId = ownTblId

    }
  where
    resolveColumnRef :: Id Table -> Ref Column -> m (Maybe (Id Column, DataCol))
    resolveColumnRef tbl colName = do
      let colQuery =
            [ "name" =: colName
            , "tableId" =: toObjectId tbl
            ]
      columnRes <- getOneByQuery colQuery
      pure $ case columnRes of
        Left _  -> Nothing
        Right (Entity i col) -> fmap (i,) (col ^? columnKind . _ColumnData)

getTableRows :: MonadHexl m => Id Table -> m Type
getTableRows t = do
  cols <- listByQuery [ "tableId" =: toObjectId t ]
  let toRow [] = pure TyNoRow
      toRow ((name, col):rest) = TyRow (Ref name)
                                    <$> typeOfDataType getTableRows (col ^. dataColType)
                                    <*> toRow rest
  toRow $ flip mapMaybe cols $ \(Entity _ col) ->
    fmap (col ^. columnName,) (col ^? columnKind . _ColumnData)

-- TODO: configurable by user
defaultContent :: MonadHexl m => DataType -> m CellContent
defaultContent = \case
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
