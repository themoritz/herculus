{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Handler.Rest where

import           Lib.Prelude
import           Prelude                    (String)

import           Control.Lens
import           Control.Monad.Except       (ExceptT (..))

import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as Text (decodeUtf8)
import qualified Data.Text.Lazy             as TL

import           Database.MongoDB           ((=:))
import qualified Network.Mail.Mime          as Mail
import           Servant
import           System.Entropy             (getEntropy)
import qualified Text.Pandoc                as Pandoc

import           Lib.Api.Rest
import qualified Lib.Api.Schema.Auth        as Api
import qualified Lib.Api.Schema.Column      as Api
import qualified Lib.Api.Schema.Project     as Api
import           Lib.Compiler
import           Lib.Model
import           Lib.Model.Auth
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Model.ViewState
import           Lib.Template
import           Lib.Types

import           Auth                       (getUserInfo, lookUpSession,
                                             mkSession)
import           Auth.Permission            (permissionColumn,
                                             permissionProject, permissionTable)
import           Engine
import           Engine.Monad
import           Engine.Util
import           Monads

type RoutesHandler = HexlT IO

handle :: ServerT Routes RoutesHandler
handle =
       handleAuth
  :<|> getProjectHandler
  :<|> getReportCellHandler

getProjectHandler :: Maybe SessionKey -> ServerT ProjectRoutes RoutesHandler
getProjectHandler mSessKey = enter (authenticate mSessKey) handleProject

getReportCellHandler :: Maybe SessionKey -> ServerT ReportCellRoutes RoutesHandler
getReportCellHandler mSessKey = enter (authenticate mSessKey) handleReportCell

authenticate :: Maybe SessionKey -> ProjectHandler :~> RoutesHandler
authenticate mSessKey = Nat $ \handler -> do
  userInfo <- getUserInfo mSessKey
  env <- askHexlEnv
  result <- lift $ runReaderT (runHexlT env handler) userInfo
  either throwError pure result

-- Auth ------------------------------------------------------------------------

type AuthHandler = HexlT IO

handleAuth :: ServerT AuthRoutes AuthHandler
handleAuth =
       handleAuthLogin
  :<|> handleAuthLogout
  :<|> handleAuthSignup
  :<|> handleAuthGetUserInfo
  :<|> handleAuthChangePwd
  :<|> handleAuthSendResetLink
  :<|> handleAuthResetPassword

handleAuthLogin
  :: (MonadIO m, MonadHexl m)
  => Api.LoginData -> m Api.LoginResponse
handleAuthLogin (Api.LoginData email pwd) =
  getOneByQuery [ "email" =: email ] >>= \case
    Left _ -> pure failMessage
    Right (Entity userId user) -> do
      -- Delete expired sessions of that user
      now <- getCurrentTime
      sessions <- listByQuery [ "userId" =: toObjectId userId ]
      for_ sessions $ \(Entity sessionId session) ->
        when (now > session ^. sessionExpDate) $
          delete sessionId
      -- Check password and login
      if verifyPassword pwd (user ^. userPwHash)
        then do
          session <- getSession userId
          pure $ Api.LoginSuccess $ Api.UserInfo userId
                                         (user ^. userName)
                                         (user ^. userEmail)
                                         (session ^. sessionKey)
        else pure failMessage
  where
    failMessage = Api.LoginFailed "Wrong email or password."
    getSession userId = do
      session <- mkSession userId
      create session $> session

handleAuthLogout :: (MonadIO m, MonadHexl m) => Maybe SessionKey -> m ()
handleAuthLogout sKey = do
  userInfo <- getUserInfo sKey
  getOneByQuery [ "userId" =: toObjectId (userInfo ^. Api.uiUserId) ] >>= \case
    Left  msg -> throwError $ ErrBug msg
    Right (Entity sessionId _) -> delete (sessionId :: Id Session)

handleAuthSignup :: (MonadIO m, MonadHexl m) => Api.SignupData -> m Api.SignupResponse
handleAuthSignup (Api.SignupData uName email pwd intention) =
  getOneByQuery [ "email" =: email ] >>= \case
    Right (_ :: Entity User) -> pure $
      Api.SignupFailed "A user with that email address already exists."
    Left _  -> do
      if verifyEmail email
        then do
          pwHash <- mkPwHash pwd
          Time signupDate <- getCurrentTime
          _ <- create $ User uName email pwHash signupDate intention
          handleAuthLogin (Api.LoginData email pwd) >>= \case
            Api.LoginSuccess userInfo -> pure $ Api.SignupSuccess userInfo
            Api.LoginFailed  msg      -> throwError $
              ErrBug $ "Signed up, but login failed: " <> msg
        else pure $ Api.SignupFailed "Please enter a valid email address."

handleAuthGetUserInfo
  :: (MonadHexl m, MonadIO m)
  => Maybe SessionKey -> m Api.GetUserInfoResponse
handleAuthGetUserInfo mSessKey = do
  eUser <- runExceptT $ case mSessKey of
    Nothing -> throwError "No auth header found"
    Just s  -> ExceptT $ lookUpSession s
  pure $ case eUser of
    Left err       -> Api.GetUserInfoFailed err
    Right userInfo -> Api.GetUserInfoSuccess userInfo

handleAuthChangePwd
  :: (MonadIO m, MonadHexl m)
  => Maybe SessionKey -> Api.ChangePwdData -> m Api.ChangePwdResponse
handleAuthChangePwd sKey (Api.ChangePwdData oldPwd newPwd) = do
  userInfo <- getUserInfo sKey
  let userId = userInfo ^. Api.uiUserId
  user <- getById' userId
  if verifyPassword oldPwd (user ^. userPwHash)
    then do
      pwHash <- mkPwHash newPwd
      update userId (userPwHash .~ pwHash)
      pure Api.ChangePwdSuccess
    else pure $ Api.ChangePwdFailure "Wrong password."

handleAuthSendResetLink :: (MonadIO m, MonadHexl m) => Text -> m ()
handleAuthSendResetLink email =
  getOneByQuery [ "email" =: email ] >>= \case
    Left _ -> pure ()
    Right (Entity userId user) -> do
      -- We're using the session mechanism here to generate unique reset tokens
      session <- mkSession userId
      _ <- create session
      let path = safeLink (Proxy @AuthRoutes) (Proxy @AuthResetPassword)
                          (session ^. sessionKey)
          lnk = "https://app.herculus.io/api/auth/" <> show path
      let recipient = Mail.Address (Just (user ^. userName))
                                   (unEmail $ user ^. userEmail)
          sender = Mail.Address (Just "Herculus Admin") "admin@herculus.io"
          subject = "Password Reset"
          body = TL.unlines
            [ "Hi " <> TL.fromStrict (user ^. userName) <> ","
            , ""
            , "Someone requested to reset your Herculus password. If that "
            , "person was you, please go to the following address:"
            , ""
            , TL.pack lnk
            , ""
            , "If you did not request this, you can just ignore this email."
            ]
          mail = Mail.simpleMail' recipient sender subject body
      liftIO $ void $ forkIO $
        Mail.renderSendMailCustom "sendmail" ["-t"] mail

handleAuthResetPassword :: (MonadIO m, MonadHexl m) => SessionKey -> m Text
handleAuthResetPassword sKey =
  lookUpSession sKey >>= \case
    Left _ ->
      pure "This reset link is not valid or has expired."
    Right Api.UserInfo{..} -> do
      newPassword <- Text.decodeUtf8 . Base64URL.encode <$> liftIO (getEntropy 8)
      newPwHash <- mkPwHash newPassword
      update _uiUserId (userPwHash .~ newPwHash)
      deleteByQuery (Proxy @Session) [ "sessionKey" =: _uiSessionKey ]
      pure $ T.unlines
        [ "The Herculus login password for the email address " <>
          unEmail _uiUserEmail <>
          " has been changed to: "
        , ""
        , newPassword
        , ""
        , "Please log in with this new password and then use the"
        , "\"Change Password\" functionality to choose a new password."
        ]

-- Project ----------------------------------------------------------------------

type ProjectHandler = HexlT (ReaderT Api.UserInfo IO)

handleProject :: ServerT ProjectRoutes ProjectHandler
handleProject =
       handleProjectCreate
  :<|> handleProjectList
  :<|> handleProjectSetName
  :<|> handleProjectDelete
  :<|> handleProjectColSetWidth
  :<|> handleProjectReorderCols
  :<|> handleProjectLoad
  :<|> handleProjectRunCommands

handleProjectCreate
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Text -> m Api.Project
handleProjectCreate projName = do
  userId <- asks Api._uiUserId
  let project = Project projName userId emptyDependencyGraph
  projectId <- create project
  pure $ Api.projectFromEntity $ Entity projectId project

handleProjectList
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => m [Api.Project]
handleProjectList = do
  userId <- asks Api._uiUserId
  map Api.projectFromEntity <$> listByQuery [ "owner" =: toObjectId userId ]

handleProjectSetName
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Project -> Text -> m ()
handleProjectSetName projectId name = do
  userId <- asks Api._uiUserId
  permissionProject userId projectId
  update projectId $ projectName .~ name

handleProjectDelete
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Project -> m ()
handleProjectDelete projectId = do
  userId <- asks Api._uiUserId
  permissionProject userId projectId
  tables <- listByQuery [ "projectId" =: toObjectId projectId ]
  handleProjectRunCommands projectId $
    map (CmdTableDelete . entityId) tables
  delete projectId

handleProjectColSetWidth
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Column -> Int -> m ()
handleProjectColSetWidth columnId width = do
  userId <- asks Api._uiUserId
  permissionColumn userId columnId
  column <- getById' columnId
  table <- getById' (column ^. columnTableId)
  void $ upsert [ "columnId" =: toObjectId columnId ]
                (ColumnWidth (table ^. tableProjectId) columnId width)
                (cwWidth .~ width)

handleProjectReorderCols
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Table -> [Id Column] -> m ()
handleProjectReorderCols tableId order = do
  userId <- asks Api._uiUserId
  permissionTable userId tableId
  void $ upsert [ "tableId" =: toObjectId tableId ]
                (ColumnOrder tableId order)
                (coOrder .~ order)

handleProjectLoad
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Project
  -> m Api.ProjectData
handleProjectLoad projectId = do
  userId <- asks Api._uiUserId
  permissionProject userId projectId
  project <- getById' projectId
  tables <- listByQuery [ "projectId" =: toObjectId projectId ]
  dat <- for tables $ \(Entity tableId _) -> do
    columns <- listByQuery [ "tableId" =: toObjectId tableId ]
    rows <- listByQuery [ "tableId" =: toObjectId tableId ]
    cells <- listByQuery [ "tableId" =: toObjectId tableId ]
    pure (columns, rows, cells)
  let (columns, rows, cells) = mconcat dat
  colOrders <- for tables $ \(Entity tableId _) -> do
    order <- either (const []) (_coOrder . entityVal) <$>
      getOneByQuery [ "tableId" =: toObjectId tableId ]
    pure (tableId, order)
  colSizes <- listByQuery [ "projectId" =: toObjectId projectId ]
  pure $ Api.ProjectData
           (Api.projectFromEntity (Entity projectId project))
           tables
           (map Api.columnFromEntity columns)
           rows
           cells
           (map (\(Entity _ cw) -> (_cwColumnId cw, _cwWidth cw)) colSizes)
           colOrders

handleProjectRunCommands
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Project -> [Command] -> m ()
handleProjectRunCommands projectId cmds = do
  userId <- asks Api._uiUserId
  permissionProject userId projectId
  -- Check that project id implied by the command matches `projectId`.
  let
    ofTable t = getById' t >>= \table -> if _tableProjectId table == projectId
      then pure ()
      else throwError $ ErrBug $ "Given project id does not match project id "
                              <> "implied by the command."
    ofColumn c = _columnTableId <$> getById' c >>= ofTable
    ofRow r = _rowTableId <$> getById' r >>= ofTable
    checkCmd = \case
      CmdTableCreate _           -> pure ()
      CmdTableSetName t _        -> ofTable t
      CmdTableDelete t           -> ofTable t
      CmdDataColCreate t         -> ofTable t
      CmdDataColUpdate c _ _ _   -> ofColumn c
      CmdReportColCreate t       -> ofTable t
      CmdReportColUpdate c _ _ _ -> ofColumn c
      CmdColumnSetName c _       -> ofColumn c
      CmdColumnDelete c          -> ofColumn c
      CmdRowCreate t             -> ofTable t
      CmdRowDelete r             -> ofRow r
      CmdCellSet c _ _           -> ofColumn c
  mapM_ checkCmd cmds
  runCommands projectId cmds

--------------------------------------------------------------------------------

type ReportCellHandler = HexlT (ReaderT Api.UserInfo IO)

handleReportCell :: ServerT ReportCellRoutes ReportCellHandler
handleReportCell =
       handleCellGetReportPDF
  :<|> handleCellGetReportHTML
  :<|> handleCellGetReportPlain

handleCellGetReportPDF
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Column -> Id Row -> m BL.ByteString
handleCellGetReportPDF columnId rowId = do
  userId <- asks Api._uiUserId
  permissionColumn userId columnId
  (repCol, plain) <- evalReport columnId rowId
  case repCol ^. reportColLanguage of
    Nothing -> throwError $ ErrUser "Cannot generate PDF from plain text"
    Just lang -> case getPandocReader lang (repCol ^. reportColFormat) of
      Nothing -> do
        let options = Pandoc.def
        runLatex options plain >>= \case
          Left e -> throwError $ ErrUser $
            "Error running pdflatex: " <> (T.pack . BL8.unpack) e <>
            "Source: " <> plain
          Right pdf -> pure pdf
      Just r -> case r Pandoc.def (T.unpack plain) of
        Left err -> throwError $
          ErrUser $ T.unlines
            [ "Could not read generated code into pandoc document: "
            , show err ]
        Right pandoc -> do
          template <- getDefaultTemplate "latex" >>= \case
            Left msg -> throwError $ ErrBug $
              "Could not load latex template: " <> msg
            Right template -> pure template
          let options = Pandoc.def
                { Pandoc.writerTemplate = Just $ T.unpack template
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
              "Error generating PDF: " <> (T.pack . BL8.unpack) e <>
              "Source: " <> show (Pandoc.writeLaTeX options pandoc)
            Right pdf -> pure pdf

handleCellGetReportHTML
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Column -> Id Row -> m Text
handleCellGetReportHTML columnId rowId = do
  userId <- asks Api._uiUserId
  permissionColumn userId columnId
  col <- getById' columnId
  (repCol, plain) <- evalReport columnId rowId
  case repCol ^. reportColLanguage of
    Nothing   -> pure plain
    Just lang -> case getPandocReader lang (repCol ^. reportColFormat) of
      Nothing -> pure plain
      Just r -> case r Pandoc.def (T.unpack plain) of
        Left err -> pure $ show err
        Right pandoc -> do
          template <- getDefaultTemplate "html5" >>= \case
            Left msg -> throwError $
              ErrBug $ "Could not load html5 template: " <> msg
            Right template -> pure template
          let options = Pandoc.def
                { Pandoc.writerTemplate = Just $ T.unpack template
                , Pandoc.writerVariables =
                  [ ("pagetitle", T.unpack (col ^. columnName))
                  , ("title-prefix", "Report")
                  ]
                }
          pure $ T.pack $ Pandoc.writeHtmlString options pandoc

handleCellGetReportPlain
  :: (MonadHexl m, MonadReader Api.UserInfo m)
  => Id Column -> Id Row -> m Text
handleCellGetReportPlain columnId rowId = do
  userId <- asks Api._uiUserId
  permissionColumn userId columnId
  snd <$> evalReport columnId rowId

getPandocReader
  :: ReportLanguage
  -> ReportFormat
  -> Maybe (Pandoc.ReaderOptions ->
            String ->
            Either Pandoc.PandocError Pandoc.Pandoc)
getPandocReader lang format = case lang of
  ReportLanguageMarkdown -> Just Pandoc.readMarkdown
  ReportLanguageLatex    -> case format of
    ReportFormatPDF -> Nothing
    _               -> Just Pandoc.readLaTeX
  ReportLanguageHTML     -> case format of
    ReportFormatHTML -> Nothing
    _                -> Just Pandoc.readHtml

evalReport :: MonadHexl m => Id Column -> Id Row -> m (ReportCol, Text)
evalReport columnId rowId = do
  col <- getById' columnId
  projectId <- (^. tableProjectId) <$> getById' (col ^. columnTableId)
  withReportCol col $ \repCol -> case repCol ^. reportColCompiledTemplate of
    CompileResultOk ttpl ->
      fmap fst $ runEngineT projectId emptyDependencyGraph $ do
        evalTemplate ttpl (mkGetter rowId) preludeTermEnv >>= \case
          Left e -> pure (repCol, e)
          Right res -> pure (repCol, res)
    _ -> throwError $ ErrBug "Getting report for non compiled template."
