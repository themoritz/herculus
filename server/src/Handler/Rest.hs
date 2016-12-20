{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Handler.Rest where

import           Prelude                        hiding (unlines)

import           Control.Lens
import           Control.Monad.Except           (ExceptT (ExceptT), runExceptT)
import           Control.Monad.IO.Class         (MonadIO)
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as BL8
import           Data.Foldable                  (for_, traverse_)
import           Data.Functor                   (($>))
import           Data.Monoid
import           Data.Text                      (Text, pack, unlines, unpack)
import           Data.Traversable               (for)
import           Database.MongoDB               ((=:))
import           Servant
import qualified Text.Pandoc                    as Pandoc
import qualified Text.Pandoc.Error              as Pandoc

import           Lib.Api.Rest
import           Lib.Compiler.Interpreter.Types
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
import           Lib.Template.Interpreter
import           Lib.Types

import           Auth                           (mkSession)
import           Auth.Permission                (permissionColumn,
                                                 permissionProject,
                                                 permissionRow, permissionTable)
import           Engine
import           Engine.Monad
import           Engine.Util
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

  :<|> handleRowCreate
  :<|> handleRowDelete
  :<|> handleRowData
  :<|> handleRowList
  :<|> handleRowListWithData

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
       <*> handleRowList sessionData tblId
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
  runCommand $ CmdColumnDelete colId

handleColumnList :: MonadHexl m => SessionData -> Id Table -> m [Entity Column]
handleColumnList (UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  listByQuery [ "tableId" =: toObjectId tblId ]

handleColumnSetName :: MonadHexl m => SessionData -> Id Column -> Text -> m ()
handleColumnSetName (UserInfo userId _ _) columnId name = do
  permissionColumn userId columnId
  runCommand $ CmdColumnSetName columnId name

--

handleDataColCreate :: MonadHexl m => SessionData -> Id Table -> m ()
handleDataColCreate (UserInfo userId _ _) tableId = do
  permissionTable userId tableId
  runCommand $ CmdDataColCreate tableId

handleDataColUpdate :: MonadHexl m => SessionData -> Id Column -> (DataType, IsDerived, Text) -> m ()
handleDataColUpdate (UserInfo userId _ _) columnId (typ, derived, code) = do
  permissionColumn userId columnId
  runCommand $ CmdDataColUpdate columnId typ derived code

handleReportColCreate :: MonadHexl m => SessionData -> Id Table -> m ()
handleReportColCreate (UserInfo userId _ _) tableId = do
  permissionTable userId tableId
  runCommand $ CmdReportColCreate tableId

handleReportColUpdate :: MonadHexl m => SessionData
                      -> Id Column -> (Text, ReportFormat, Maybe ReportLanguage)
                      -> m ()
handleReportColUpdate (UserInfo userId _ _) columnId (template, format, language) = do
  permissionColumn userId columnId
  runCommand $ CmdReportColUpdate columnId template format language

--

handleRowCreate :: MonadHexl m => SessionData -> Id Table -> m ()
handleRowCreate (UserInfo userId _ _) tableId = do
  permissionTable userId tableId
  runCommand $ CmdRowCreate tableId

handleRowDelete :: MonadHexl m => SessionData -> Id Row -> m ()
handleRowDelete (UserInfo userId _ _) rowId = do
  permissionRow userId rowId
  runCommand $ CmdRowDelete rowId

-- | Get all the data of a row
handleRowData :: MonadHexl m => SessionData
              -> Id Row -> m [(Entity Column, CellContent)]
handleRowData (UserInfo userId _ _) recId = do
  permissionRow userId recId
  cells <- listByQuery
    [ "aspects.recordId" =: toObjectId recId ]
  for cells $ \(Entity _ cell) -> do
    let i = cell ^. cellColumnId
    col <- getById' i
    pure (Entity i col, cell ^. cellContent)

handleRowList :: MonadHexl m => SessionData -> Id Table -> m [Entity Row]
handleRowList (UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  listByQuery [ "tableId" =: toObjectId tblId ]

handleRowListWithData :: MonadHexl m => SessionData
                      -> Id Table -> m [(Id Row, [(Entity Column, CellContent)])]
handleRowListWithData sessionData@(UserInfo userId _ _) tblId = do
  permissionTable userId tblId
  recs <- listByQuery [ "tableId" =: toObjectId tblId ]
  for recs $ \r -> (entityId r,) <$> handleRowData sessionData (entityId r)

--------------------------------------------------------------------------------

handleCellSet :: MonadHexl m => SessionData
              -> Id Column -> Id Row -> Value -> m ()
handleCellSet (UserInfo userId _ _) columnId rowId value = do
  permissionColumn userId columnId
  runCommand $ CmdCellSet columnId rowId value

--------------------------------------------------------------------------------

handleCellGetReportPDF :: MonadHexl m => SessionData
                       -> Maybe SessionKey -> Id Column -> Id Row
                       -> m BL.ByteString
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
        Left err -> throwError $
          ErrUser $ unlines
            [ "Could not read generated code into pandoc document: "
            , (pack . show) err ]
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

handleCellGetReportHTML :: MonadHexl m => SessionData
                        -> Maybe SessionKey -> Id Column -> Id Row -> m Text
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
            Left msg -> throwError $
              ErrBug $ "Could not load html5 template: " <> pack msg
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

handleCellGetReportPlain :: MonadHexl m => SessionData
                         -> Maybe SessionKey -> Id Column -> Id Row -> m Text
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

-- Helper ----------------------------------------------------------------------

evalReport :: MonadHexl m => Id Column -> Id Row -> m (ReportCol, Text)
evalReport columnId recordId = do
  col <- getById' columnId
  withReportCol col $ \repCol -> case repCol ^. reportColCompiledTemplate of
    CompileResultOk ttpl -> fmap fst $ runEngineT emptyDependencyGraph $ do
      let env = EvalEnv
                  { envGetCellValue = flip getCellValue recordId
                  , envGetColumnValues = getColumnValues
                  , envGetTableRows = getTableRows
                  , envGetRowField = getRowField
                  }
      runEvalTemplate env ttpl >>= \case
        Left e -> pure (repCol, e)
        Right res -> pure (repCol, res)
    _ -> throwError $ ErrBug "Getting report for non compiled template."
