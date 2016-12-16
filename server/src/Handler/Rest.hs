{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Handler.Rest where

import           Control.Arrow                  (second)
import           Control.Lens
import           Control.Monad                  (unless, void, when)
import           Control.Monad.Except           (ExceptT (ExceptT), runExceptT)
import           Control.Monad.IO.Class         (MonadIO)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as BL8
import           Data.Foldable                  (for_, traverse_)
import           Data.Functor                   (($>))
import           Data.List                      (union)
import           Data.Maybe                     (catMaybes, isNothing, mapMaybe)
import           Data.Monoid
import           Data.Text                      (Text, pack, unpack)
import           Data.Traversable               (for)
import           Database.MongoDB               ((=:))
import           Servant
import qualified Text.Pandoc                    as Pandoc
import qualified Text.Pandoc.Error              as Pandoc

import           Lib.Api.Rest
import           Lib.Api.WebSocket
import           Lib.Compiler
import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Typechecker.Prim
import           Lib.Compiler.Typechecker.Types hiding (union)
import           Lib.Compiler.Types
import           Lib.Model
import           Lib.Model.Auth                 (GetUserInfoResponse (..),
                                                 LoginData (..),
                                                 LoginResponse (..), Session,
                                                 SessionKey, SignupData (..),
                                                 SignupResponse (..),
                                                 User (User),
                                                 UserInfo (UserInfo), mkPwHash,
                                                 sessionKey, sessionUserId,
                                                 userName, userPwHash,
                                                 verifyPassword)
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Template
import           Lib.Template.Interpreter
import           Lib.Template.Types
import           Lib.Types

import           Auth                           (mkSession)
import           Auth.Permission                (permissionColumn,
                                                 permissionProject,
                                                 permissionRow, permissionTable)
import           Engine
import           Monads

handle :: ServerT Routes (HexlT IO)
handle =
       handleAuthLogin
  :<|> handleAuthLogout
  :<|> handleAuthSignup
  :<|> handleAuthGetUserInfo

  :<|> handleProjectCreate
  :<|> handleProjectList
  :<|> handleProjectSetName
  :<|> handleProjectDelete
  :<|> handleProjectLoad

  :<|> handleTableCreate
  :<|> handleTableData
  :<|> handleTableGetWhole
  :<|> handleTableSetName
  :<|> handleTableDelete

  :<|> handleColumnDelete
  :<|> handleColumnList
  :<|> handleColumnSetName

  :<|> handleDataColCreate
  :<|> handleDataColUpdate

  :<|> handleReportColCreate
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

-- Auth ------------------------------------------------------------------------

handleAuthLogin :: (MonadIO m, MonadHexl m) => LoginData -> m LoginResponse
handleAuthLogin (LoginData uName pwd) =
    checkLogin >>= \case
      Left  err    -> pure $ LoginFailed err
      Right userId -> do
        session <- getSession userId
        pure $ LoginSuccess $ UserInfo userId uName $ session ^. sessionKey
  where
    checkLogin = runExceptT $ do
      Entity userId user <- getUser
      checkPassword_ user
      pure userId
    getUser = ExceptT $ over _Left (\_ -> "username unknown") <$>
      getOneByQuery [ "name" =: uName ]
    checkPassword_ user =
      if verifyPassword pwd (user ^. userPwHash)
        then pure ()
        else throwError "wrong password"

    getSession userId = do
      eSession <- getOneByQuery [ "userId" =: toObjectId userId ]
      for_ eSession $ \(Entity sessionId _) -> delete (sessionId :: Id Session)
      session <- mkSession userId
      create session $> session

handleAuthLogout :: MonadHexl m => SessionData -> m ()
handleAuthLogout (UserInfo userId _ _) =
  getOneByQuery [ "userId" =: toObjectId userId ] >>= \case
    Left  msg -> throwError $ ErrBug msg
    Right (Entity sessionId _) -> delete (sessionId :: Id Session)

handleAuthSignup :: (MonadIO m, MonadHexl m) => SignupData -> m SignupResponse
handleAuthSignup (SignupData uName pwd intention) =
  getOneByQuery [ "name" =: uName ] >>= \case
    Right (_ :: Entity User) -> pure $ SignupFailed "username already exists"
    Left _  -> do
      pwHash <- mkPwHash pwd
      _ <- create $ User uName pwHash intention
      handleAuthLogin (LoginData uName pwd) >>= \case
        LoginSuccess userInfo -> pure $ SignupSuccess userInfo
        LoginFailed  msg      -> throwError $ ErrBug $ "signed up, but login failed: " <> msg

handleAuthGetUserInfo :: (MonadIO m, MonadHexl m) => SessionKey -> m GetUserInfoResponse
handleAuthGetUserInfo sKey = do
  eUser <- runExceptT $ do
    Entity _ session <- ExceptT $ getOneByQuery [ "sessionKey" =: sKey ]
    let userId = session ^. sessionUserId
    user <- ExceptT $ getById userId
    pure (userId, user ^. userName)
  pure $ case eUser of
    Left err             -> GetUserInfoFailed err
    Right (userId, name) -> GetUserInfoSuccess $ UserInfo userId name sKey

-- Project ----------------------------------------------------------------------

handleProjectCreate :: MonadHexl m => SessionData -> Text -> m (Entity ProjectClient)
handleProjectCreate (UserInfo userId _ _) projName = do
  let project = Project projName userId emptyDependencyGraph
  projectId <- create project
  pure $ toClient $ Entity projectId project

handleProjectList :: MonadHexl m => SessionData -> m [Entity ProjectClient]
handleProjectList (UserInfo userId _ _) =
  map toClient <$> listByQuery [ "owner" =: toObjectId userId ]

handleProjectSetName :: MonadHexl m => SessionData -> Id Project -> Text -> m ()
handleProjectSetName (UserInfo userId _ _) projectId name = do
  permissionProject userId projectId
  update projectId $ projectName .~ name

handleProjectDelete :: MonadHexl m => SessionData -> Id Project -> m ()
handleProjectDelete sessionData@(UserInfo userId _ _) projectId = do
  permissionProject userId projectId
  tables <- listByQuery [ "projectId" =: toObjectId projectId ]
  traverse_ (handleTableDelete sessionData . entityId) tables
  delete projectId

handleProjectLoad :: MonadHexl m => SessionData -> Id Project -> m (ProjectClient, [Entity Table])
handleProjectLoad (UserInfo userId _ _) projId = do
  permissionProject userId projId
  tables <- listByQuery [ "projectId" =: toObjectId projId ]
  project <- getById' projId
  pure (toClient project, tables)

--------------------------------------------------------------------------------

handleTableCreate :: MonadHexl m => SessionData -> Table -> m ()
handleTableCreate (UserInfo userId _ _) (Table projectId name) = do
  permissionProject userId projectId
  runCommand $ CmdTableCreate projectId name

handleTableData :: MonadHexl m => SessionData -> Id Table -> m [(Id Column, Id Row, CellContent)]
handleTableData (UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  cells <- listByQuery [ "aspects.tableId" =: toObjectId tblId ]
  let go (Cell v _ c r) = (c, r, v)
  pure $ map (go . entityVal) cells

handleTableGetWhole :: MonadHexl m
                    => SessionData
                    -> Id Table
                    -> m ([Entity Column], [Entity Row], [(Id Column, Id Row, CellContent)])
handleTableGetWhole sessionData@(UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  (,,) <$> handleColumnList sessionData tblId
       <*> handleRecordList sessionData tblId
       <*> handleTableData sessionData tblId

handleTableSetName :: MonadHexl m => SessionData -> Id Table -> Text -> m ()
handleTableSetName (UserInfo userId _ _) tableId name = do
  permissionTable userId tableId
  runCommand $ CmdTableSetName tableId name

handleTableDelete :: MonadHexl m => SessionData -> Id Table -> m ()
handleTableDelete (UserInfo userId _ _) tableId = do
  permissionTable userId tableId
  runCommand $ CmdTableDelete tableId

--------------------------------------------------------------------------------

handleColumnDelete :: MonadHexl m => SessionData -> Id Column -> m ()
handleColumnDelete (UserInfo userId _ _) colId = do
  permissionColumn userId colId
  delete colId
  deleteByQuery (Proxy :: Proxy Cell)
    [ "aspects.columnId" =: toObjectId colId
    ]
  newChilds <- compileColumnChildren colId
  void $ modifyDependencies colId []
  modifyReferences $ removeReference colId
  sendWS $ WsDownColumnsChanged newChilds
  propagate [RootWholeColumns $ map entityId newChilds]

handleColumnList :: MonadHexl m => SessionData -> Id Table -> m [Entity Column]
handleColumnList (UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  listByQuery [ "tableId" =: toObjectId tblId ]

handleColumnSetName :: MonadHexl m => SessionData -> Id Column -> Text -> m ()
handleColumnSetName (UserInfo userId _ _) columnId name = do
  permissionColumn userId columnId
  update columnId $ columnName .~ name
  newChilds <- compileColumnChildren columnId
  sendWS $ WsDownColumnsChanged newChilds
  propagate [RootWholeColumns $ map entityId newChilds]

--

handleDataColCreate :: MonadHexl m => SessionData -> Id Table -> m ()
handleDataColCreate (UserInfo userId _ _) tableId = do
  permissionTable userId tableId
  runCommand $ CmdDataColCreate tableId

handleDataColUpdate :: MonadHexl m => SessionData -> Id Column -> (DataType, IsDerived, Text) -> m ()
handleDataColUpdate (UserInfo userId _ _) columnId (typ, derived, code) = do
  permissionColumn userId columnId
  oldCol <- getById' columnId
  case oldCol ^? columnKind . _ColumnData of
    Nothing -> throwError $ ErrBug "Called dataColUpdate for column other than data"
    Just dataCol -> do
      let updatedCol = oldCol & columnKind . _ColumnData %~ (dataColType .~ typ)
                                           . (dataColIsDerived .~ derived)
                                           . (dataColSourceCode .~ code)
      update columnId $ const updatedCol

      updatedChilds <- if dataCol ^. dataColType == typ
        then pure []
        else compileColumnChildren columnId

      (newSelf, propSelf) <- case derived of
        Derived -> do
          newC <- compileColumn columnId
          pure ([newC], [columnId])
        NotDerived -> do
          renewErrorCells columnId
          unless (typ == dataCol ^. dataColType) $ do
            updateReference columnId typ
            invalidateCells columnId
          pure ([Entity columnId updatedCol], [])

      sendWS $ WsDownColumnsChanged $ newSelf <> updatedChilds
      propagate [RootWholeColumns $ propSelf <> map entityId updatedChilds]

handleReportColCreate :: MonadHexl m => SessionData -> Id Table -> m ()
handleReportColCreate = undefined

handleReportColUpdate :: MonadHexl m => SessionData -> Id Column -> (Text, ReportFormat, Maybe ReportLanguage) -> m ()
handleReportColUpdate (UserInfo userId _ _) columnId (template, format, lang) = do
  permissionColumn userId columnId
  oldCol <- getById' columnId
  when (isNothing (oldCol ^? columnKind . _ColumnReport)) $
    throwError $ ErrBug "Called ReportColUpdate for column other than report"
  update columnId $ columnKind . _ColumnReport %~ (reportColTemplate .~ template)
                                                . (reportColFormat .~ format)
                                                . (reportColLanguage .~ lang)
  newCol <- compileColumn columnId
  sendWS $ WsDownColumnsChanged [newCol]

--

handleRecordCreate :: MonadHexl m => SessionData -> Id Table -> m (Entity Row, [Entity Cell])
handleRecordCreate (UserInfo userId _ _) tableId = do
  permissionTable userId tableId
  let newRec = Record tableId
  r <- create newRec
  cols <- listByQuery [ "tableId" =: toObjectId tableId ]
  newCells <- for cols $ \e@(Entity c col) ->
    case col ^? columnKind . _ColumnData of
      Nothing -> pure Nothing
      Just dataCol -> do
        defContent <- defaultContent (dataCol ^. dataColType)
        let cell = newCell tableId c r defContent
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
  sendWS $ WsDownRecordCreated tableId r $
    map (second $ cellContent . entityVal) $ catMaybes newCells
  pure (Entity r newRec, map snd $ catMaybes newCells)

handleRecordDelete :: MonadHexl m => SessionData -> Id Row -> m ()
handleRecordDelete (UserInfo userId _ _) recId = do
  permissionRecord userId recId
  record <- getById' recId
  delete recId
  sendWS $ WsDownRecordDeleted (recordTableId record) recId
  deleteByQuery (Proxy :: Proxy Cell)
    [ "aspects.recordId" =: toObjectId recId
    ]
  let tableId = recordTableId record
  graph <- getDependencies
  cols <- listByQuery [ "tableId" =: toObjectId tableId ]
  let allChildren = foldr (union . (\e ->
                                      map fst .
                                      filter (\(_, typ) -> typ == OneToAll) .
                                      getChildren (entityId e) $ graph)) [] cols
  -- Invalidate references of values in other cells to this record
  refGraph <- getReferences
  let refingCols = getReferringColumns tableId refGraph
  mCellChanges <- for refingCols $ \col -> do
    cells <- listByQuery [ "aspects.columnId" =: toObjectId col ]
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
    , RootCellChanges $ map (\(Cell _ _ c r) -> (c, r)) cellChanges
    ]

-- | Get all the data of a row
handleRecordData :: MonadHexl m => SessionData -> Id Row -> m [(Entity Column, CellContent)]
handleRecordData (UserInfo userId _ _) recId = do
  permissionRecord userId recId
  cells <- listByQuery
    [ "aspects.recordId" =: toObjectId recId ]
  for cells $ \(Entity _ cell) -> do
    let i = aspectsColumnId $ cellAspects cell
    col <- getById' i
    pure (Entity i col, cellContent cell)

handleRecordList :: MonadHexl m => SessionData -> Id Table -> m [Entity Row]
handleRecordList (UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  listByQuery [ "tableId" =: toObjectId tblId ]

handleRecordListWithData :: MonadHexl m => SessionData -> Id Table
                          -> m [(Id Row, [(Entity Column, CellContent)])]
handleRecordListWithData sessionData@(UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  recs <- listByQuery [ "tableId" =: toObjectId tblId ]
  for recs $ \r -> (entityId r,) <$> handleRecordData sessionData (entityId r)

--

handleCellSet :: MonadHexl m => SessionData -> Id Column -> Id Row -> Value -> m ()
handleCellSet (UserInfo userId _ _) columnId recordId val = do
  permissionColumn userId columnId
  Entity i cell <- getOneByQuery'
    [ "aspects.columnId" =: toObjectId columnId
    , "aspects.recordId" =: toObjectId recordId
    ]
  let changedCell = cell { cellContent = CellValue val }
  update i $ const changedCell
  sendWS $ WsDownCellsChanged [changedCell]
  -- TODO: fork this
  propagate [RootCellChanges [(columnId, recordId)]]

handleCellGetReportPDF :: MonadHexl m => SessionData -> Maybe SessionKey -> Id Column -> Id Row -> m BL.ByteString
handleCellGetReportPDF (UserInfo userId _ _) _ columnId recordId = do
  permissionColumn userId columnId
  (repCol, plain) <- evalReport columnId recordId
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

handleCellGetReportHTML :: MonadHexl m => SessionData -> Maybe SessionKey -> Id Column -> Id Row -> m Text
handleCellGetReportHTML (UserInfo userId _ _) _ columnId recordId = do
  permissionColumn userId columnId
  col <- getById' columnId
  (repCol, plain) <- evalReport columnId recordId
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

handleCellGetReportPlain :: MonadHexl m => SessionData -> Maybe SessionKey -> Id Column -> Id Row -> m Text
handleCellGetReportPlain (UserInfo userId _ _) _ columnId recordId = do
  permissionColumn userId columnId
  snd <$> evalReport columnId recordId

getPandocReader :: ReportLanguage
                -> ReportFormat
                -> Maybe (Pandoc.ReaderOptions
                -> String
                -> Either Pandoc.PandocError Pandoc.Pandoc)
getPandocReader lang format = case lang of
  ReportLanguageMarkdown -> Just Pandoc.readMarkdown
  ReportLanguageLatex    -> case format of
    ReportFormatPDF -> Nothing
    _               -> Just Pandoc.readLaTeX
  ReportLanguageHTML     -> case format of
    ReportFormatHTML -> Nothing
    _                -> Just Pandoc.readHtml

-- Helper ----------------------------

evalReport :: MonadHexl m => Id Column -> Id Row -> m (ReportCol, Text)
evalReport columnId recordId = do
  col <- getById' columnId
  case col ^? columnKind . _ColumnReport of
    Nothing -> throwError $ ErrBug "Trying to get report for non report column"
    Just repCol -> case repCol ^. reportColCompiledTemplate of
      CompileResultOk ttpl -> fmap fst $ runCacheT $ do
        let env = EvalEnv
                    { envGetCellValue = flip getCellValue recordId
                    , envGetColumnValues = getColumnValues
                    , envGetTableRows = getTableRecords
                    , envGetRowField = getRecordValue
                    }
        runEvalTemplate env ttpl >>= \case
          Left e -> pure (repCol, e)
          Right res -> pure (repCol, res)
      _ -> throwError $ ErrBug "Getting report for non compiled template."

-- | Put default content in every cell of the column. Don't propagate.
invalidateCells :: MonadHexl m => Id Column -> m ()
invalidateCells columnId = do
  col <- getById' columnId
  dataCol <- case col ^? columnKind . _ColumnData of
    Nothing -> throwError $ ErrBug "Trying to invalidate cells of column other than data"
    Just dataCol -> pure dataCol
  cells <- listByQuery [ "aspects.columnId" =: toObjectId columnId ]
  changes <- for cells $ \(Entity i cell) -> do
    defContent <- defaultContent (dataCol ^. dataColType)
    let invalidatedCell = Cell defContent (cellAspects cell)
    update i $ const invalidatedCell
    pure invalidatedCell
  sendWS $ WsDownCellsChanged changes

-- | Insert default content for error cells and propagate.
renewErrorCells :: MonadHexl m => Id Column -> m ()
renewErrorCells columnId = do
  col <- getById' columnId
  dataCol <- case col ^? columnKind . _ColumnData of
    Nothing -> throwError $ ErrBug "Trying to renewErrorCells of column other than data"
    Just dataCol -> pure dataCol
  cells <- listByQuery [ "aspects.columnId" =: toObjectId columnId ]
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
                map (\(Cell _ _ columnId' r') -> (columnId', r')) $
                catMaybes changes
            ]

-- | If the datatype of the column contains a reference to a table, add it to
-- the reference graph. Otherwise remove all references.
updateReference :: MonadHexl m => Id Column -> DataType -> m ()
updateReference columnId dataType = case getReference dataType of
  Nothing -> modifyReferences $ removeReference columnId
  Just t  -> modifyReferences $ setReference t columnId

--
