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
import           Lib.Model.Auth                 (ChangePwdData (..),
                                                 ChangePwdResponse (..),
                                                 GetUserInfoResponse (..),
                                                 LoginData (..),
                                                 LoginResponse (..), Session,
                                                 SessionKey, SignupData (..),
                                                 SignupResponse (..), User (..),
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
                                                 permissionProject)
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
  :<|> handleAuthChangePwd

  :<|> handleProjectCreate
  :<|> handleProjectList
  :<|> handleProjectSetName
  :<|> handleProjectDelete
  :<|> handleProjectLoad
  :<|> handleProjectRunCommand

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

handleAuthChangePwd :: (MonadIO m, MonadHexl m)
                    => SessionData -> ChangePwdData -> m ChangePwdResponse
handleAuthChangePwd (UserInfo userId _ _) (ChangePwdData oldPwd newPwd) = do
  user <- getById' userId
  if verifyPassword oldPwd (user ^. userPwHash)
    then do
      pwHash <- mkPwHash newPwd
      update userId (userPwHash .~ pwHash)
      pure ChangePwdSuccess
    else pure $ ChangePwdFailure "wrong password"

-- Project ----------------------------------------------------------------------

handleProjectCreate :: MonadHexl m => SessionData -> Text -> m (Entity ProjectClient)
handleProjectCreate (UserInfo userId _ _) projName = do
  let project = Project projName userId emptyDependencyGraph
  projectId <- create project
  pure $ toClient $ Entity projectId project

handleProjectList :: MonadHexl m => SessionData -> m [Entity ProjectClient]
handleProjectList (UserInfo userId _ _) =
  map toClient <$> listByQuery [ "owner" =: toObjectId userId ]

handleProjectSetName :: MonadHexl m => SessionData -> Id ProjectClient -> Text -> m ()
handleProjectSetName (UserInfo userId _ _) projectId name = do
  let i = fromClientId projectId
  permissionProject userId i
  update i $ projectName .~ name

handleProjectDelete :: MonadHexl m => SessionData -> Id ProjectClient -> m ()
handleProjectDelete sessionData@(UserInfo userId _ _) projectId = do
  let i = fromClientId projectId
  permissionProject userId i
  delete i
  tables <- listByQuery [ "projectId" =: toObjectId i ]
  traverse_ (handleProjectRunCommand sessionData projectId .
             CmdTableDelete .
             entityId) tables

handleProjectLoad :: MonadHexl m => SessionData -> Id ProjectClient
                  -> m ( ProjectClient
                       , [Entity Table]
                       , [Entity Column]
                       , [Entity Row]
                       , [Entity Cell] )
handleProjectLoad (UserInfo userId _ _) projectId = do
  let i = fromClientId projectId
  permissionProject userId i
  project <- getById' i
  tables <- listByQuery [ "projectId" =: toObjectId i ]
  dat <- for tables $ \(Entity tableId _) -> do
    columns <- listByQuery [ "tableId" =: toObjectId tableId ]
    rows <- listByQuery [ "tableId" =: toObjectId tableId ]
    cells <- listByQuery [ "tableId" =: toObjectId tableId ]
    pure (columns, rows, cells)
  let (columns, rows, cells) = mconcat dat
  pure (toClient project, tables, columns, rows, cells)

handleProjectRunCommand :: MonadHexl m => SessionData -> Id ProjectClient -> Command -> m ()
handleProjectRunCommand (UserInfo userId _ _) projectId cmd = do
  let i = fromClientId projectId
  permissionProject userId i
  -- Check that project id implied by the command matches `i`.
  let ofTable t = getById' t >>= \table -> if _tableProjectId table == i
        then pure ()
        else throwError $ ErrBug $ "Given project id does not match project id "
                                <> "implied by the command."
      ofColumn c = _columnTableId <$> getById' c >>= ofTable
      ofRow r = _rowTableId <$> getById' r >>= ofTable
  case cmd of
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
  runCommand i cmd

--------------------------------------------------------------------------------

handleCellGetReportPDF :: MonadHexl m => SessionData
                       -> Maybe SessionKey -> Id Column -> Id Row
                       -> m BL.ByteString
handleCellGetReportPDF (UserInfo userId _ _) _ columnId rowId = do
  permissionColumn userId columnId
  (repCol, plain) <- evalReport columnId rowId
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
handleCellGetReportHTML (UserInfo userId _ _) _ columnId rowId = do
  permissionColumn userId columnId
  col <- getById' columnId
  (repCol, plain) <- evalReport columnId rowId
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
handleCellGetReportPlain (UserInfo userId _ _) _ columnId rowId = do
  permissionColumn userId columnId
  snd <$> evalReport columnId rowId

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
evalReport columnId rowId = do
  col <- getById' columnId
  withReportCol col $ \repCol -> case repCol ^. reportColCompiledTemplate of
    CompileResultOk ttpl -> fmap fst $ runEngineT emptyDependencyGraph $ do
      let env = EvalEnv
                  { envGetCellValue = flip getCellValue rowId
                  , envGetColumnValues = getColumnValues
                  , envGetTableRows = fmap (map entityId) . getTableRows
                  , envGetRowField = getRowField
                  }
      runEvalTemplate env ttpl >>= \case
        Left e -> pure (repCol, e)
        Right res -> pure (repCol, res)
    _ -> throwError $ ErrBug "Getting report for non compiled template."
