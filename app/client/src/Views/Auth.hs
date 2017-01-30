{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Auth where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           React.Flux      (ReactElementM, ReactView, button_, classNames,
                                  cldiv_, defineStatefulView, elemText, h1_,
                                  input_, label_, onChange, onClick, onKeyDown,
                                  p_, strong_, table_, target, tbody_, td_,
                                  textarea_, tr_, view, ($=), (&=))

import           Lib.Model.Auth  (ChangePwdData (..), Email (..),
                                  LoginData (..), SignupData (..))
import qualified LoggedIn
import           Store           (Action (Login, Signup, ToLoginForm, ToSignupForm),
                                  dispatch, dispatchLoggedIn)
import           Views.Common    (keyENTER)


-- Validation helpers

emptyText :: Text -> Bool
emptyText = Text.null

validateUserName :: Text -> Bool
validateUserName = not . emptyText

validateEmail :: Text -> Bool
validateEmail = not . emptyText

validatePwd :: Text -> Bool
validatePwd = not . emptyText

-- Views

login_ :: ReactElementM eh ()
login_ = view login () mempty

data LoginViewState = LoginViewState
  { inpEmailValue   :: Text
  , inpPwdValue     :: Text
  , showLoginErrors :: Bool
  } deriving (Generic, Show, NFData)

initialLoginViewState :: LoginViewState
initialLoginViewState = LoginViewState
  { inpEmailValue   = ""
  , inpPwdValue     = ""
  , showLoginErrors = False
  }

login :: ReactView ()
login = defineStatefulView "login" initialLoginViewState $ \viewState _ ->

  let inpEmailError = not . validateEmail $ inpEmailValue viewState
      inpPwdError = not . validatePwd $ inpPwdValue viewState
      validFormData = not (inpEmailError || inpPwdError)

      loginHandler viewState' =
        if validFormData then
          ( dispatch $ Login (LoginData (Email $ inpEmailValue viewState') (inpPwdValue viewState'))
          , Just updatedErrorState
          )
        else
          ( []
          , Just updatedErrorState
          )
        where
          updatedErrorState = viewState' {
             showLoginErrors = True
           }

      inputKeyDownHandler _ evt viewState'
        | keyENTER evt = loginHandler viewState'
        | otherwise = ([], Nothing)

  in cldiv_ "form" $ do
       h1_ "Login"
       table_ $ tbody_ $ do
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "email"
             ] "E-Mail"
           td_ $ input_
             [ classNames
                 [ ("auth-input", True)
                 , ("invalid", showLoginErrors viewState && inpEmailError)
                 ]
             , "type" $= "text"
             , "value" &= inpEmailValue viewState
             , "autoFocus" &= True
             , "name" $= "email"
             , onChange $ \ev viewState' -> ([], Just viewState' {
                inpEmailValue = target ev "value"})
             , onKeyDown inputKeyDownHandler
             ]
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "password"
             ] "Password"
           td_ $ input_
             [ classNames
                 [ ("auth-input", True)
                 , ("invalid", showLoginErrors viewState && inpPwdError)
                 ]
             , "type" $= "password"
             , "value" &= inpPwdValue viewState
             , "name" $= "password"
             , onChange $ \ev viewState' -> ([], Just viewState' {
                inpPwdValue = target ev "value"})
             , onKeyDown inputKeyDownHandler
             ]
         tr_ $ td_
           [ "className" $= "submit"
           , "colSpan" $= "2"
           ] $ button_
             [ classNames [ ("enabled", validFormData)]
             , onClick $ \_ _ viewState' -> loginHandler viewState'
             ] "Submit"
         tr_ $ td_
           [ "colSpan" $= "2"
           ] $ do
             "Not registered yet? "
             button_
               [ classNames [ ("pure", True) ]
               , onClick $ \_ _ _ -> (dispatch ToSignupForm, Nothing)
               ] "Sign up"
             "."

signup_ :: ReactElementM eh ()
signup_ = view signup () mempty

data SignupViewState = SignupViewState
  { signupUserName   :: Text
  , signupEmail      :: Text
  , signupPwd        :: Text
  , signupPwdConfirm :: Text
  , signupIntention  :: Text
  , showSignupErrors :: Bool
  } deriving (Generic, Show, NFData)

initialSignupViewState :: SignupViewState
initialSignupViewState = SignupViewState
  { signupUserName   = ""
  , signupEmail      = ""
  , signupPwd        = ""
  , signupPwdConfirm = ""
  , signupIntention  = ""
  , showSignupErrors = False
  }

signup :: ReactView ()
signup = defineStatefulView "signup" initialSignupViewState $ \viewState _ ->

  let signupUserNameError = not . validateUserName $ signupUserName viewState
      signupEmailError = not . validateEmail $ signupEmail viewState
      equalPwdValues = signupPwd viewState == signupPwdConfirm viewState
      signupPwdError = (not . validatePwd $ signupPwd viewState)
        || not equalPwdValues
      signupPwdConfirmError = (not . validatePwd $ signupPwdConfirm viewState)
        || not equalPwdValues
      signupIntentionError = emptyText $ signupIntention viewState

      validFormData = not $ any id
        [ signupUserNameError
        , signupPwdError
        , signupPwdConfirmError
        , signupIntentionError
        ]

      signupHandler viewState' =
        if validFormData then
          let signupData = SignupData
                { suUserName  = signupUserName viewState'
                , suEmail     = Email (signupEmail viewState')
                , suPassword  = signupPwd viewState'
                , suIntention = signupIntention viewState'
                }
          in (dispatch $ Signup signupData, Just updatedErrorState)
        else
          ([], Just updatedErrorState)
        where
          updatedErrorState = viewState'
            { showSignupErrors = True
            }

      inputKeyDownHandler _ evt viewState'
       | keyENTER evt = signupHandler viewState'
       | otherwise = ([], Nothing)

  in cldiv_ "form" $ do
       h1_ "Signup"
       table_ $ tbody_ $ do
         tr_ $ do
           td_ $
             label_
               [ "htmlFor" $= "intention"
               ] $ elemText $ Text.unlines
                 [ "What are you planning to use Herculus for? Describe a"
                 , "project, idea, use case, or state that you are just going"
                 , "to play around."
                 ]
           td_ $
             textarea_
               [ classNames
                   [ ("auth-txtarea", True)
                   , ("invalid", showSignupErrors viewState && signupIntentionError)
                   ]
               , "value" &= signupIntention viewState
               , "name" $= "intention"
               , "rows" $= "5"
               , onChange $ \ev st -> ([], Just st {
                   signupIntention = target ev "value" })
               ] ""
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "username"
             ] "Name"
           td_ $ input_
             [ classNames
                 [ ("auth-input", True)
                 , ("invalid", showSignupErrors viewState && signupUserNameError)
                 ]
             , "type" $= "text"
             , "value" &= signupUserName viewState
             , "autoFocus" &= True
             , "name" $= "username"
             , onChange $ \ev st -> ([], Just st {
                 signupUserName = target ev "value"})
             , onKeyDown inputKeyDownHandler
             ]
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "email"
             ] "Email"
           td_ $ input_
             [ classNames
                 [ ("auth-input", True)
                 , ("invalid", showSignupErrors viewState && signupEmailError)
                 ]
             , "type" $= "text"
             , "value" &= signupEmail viewState
             , "autoFocus" &= True
             , "name" $= "email"
             , onChange $ \ev st -> ([], Just st {
                 signupEmail = target ev "value"})
             , onKeyDown inputKeyDownHandler
             ]
         tr_ $ do
            td_ $ label_
              [ "htmlFor" $= "password"
              ] "Password"
            td_ $ input_
              [ classNames
                  [ ("auth-input", True)
                  , ("invalid", showSignupErrors viewState && signupPwdError)
                  ]
              , "type" $= "password"
              , "value" &= signupPwd viewState
              , "name" $= "password"
              , onChange $ \ev st -> ([], Just st {
                 signupPwd = target ev "value"})
              , onKeyDown inputKeyDownHandler
              ]
         tr_ $ do
            td_ $ label_
              [ "htmlFor" $= "passwordConfirm"
              ] "Password (again)"
            td_ $ input_
              [ classNames
                  [ ("auth-input", True)
                  , ("invalid", showSignupErrors viewState && signupPwdConfirmError)
                  ]
              , "type" $= "password"
              , "value" &= signupPwdConfirm viewState
              , "name" $= "passwordConfirm"
              , onChange $ \ev st -> ([], Just st {
                 signupPwdConfirm = target ev "value"})
              , onKeyDown inputKeyDownHandler
              ]
         tr_ $ td_
            [ "className" $= "submit"
            , "colSpan" $= "2"
            ] $ button_
              [ classNames [ ("enabled", validFormData)]
              , onClick $ \_ _ st -> signupHandler st
              ] "Submit"
       cldiv_ "disclaimer" $ do
         p_ $ elemText $ Text.unlines
           [ "Please be aware that this is a beta version."
           , "We are still in the progress of implementing a lot of features."
           , "Bugs may occur, and you may lose your data."
           ]
         p_ $ do
           "Your feedback to "
           strong_ "moritz@herculus.io"
           " is highly appreciated."
       p_ $ do
         "Back to "
         button_
           [ classNames [ ("pure", True) ]
           , onClick $ \_ _ _ -> (dispatch ToLoginForm, Nothing)
           ] "login"
         "."

-- Change password

data ChangePasswordState = ChangePasswordState
  { cpsOldPassword     :: Text
  , cpsPassword        :: Text
  , cpsPasswordConfirm :: Text
  , cpsShowErrors      :: Bool
  } deriving (Generic, Show, NFData)

initialCPS :: ChangePasswordState
initialCPS = ChangePasswordState "" "" "" False

-- prop: True  -> valid old password or initial state
--       False -> invalid old password
changePassword_ :: Bool -> ReactElementM eh ()
changePassword_ !valid = view changePassword valid mempty

changePassword :: ReactView Bool
changePassword =
  defineStatefulView "change password" initialCPS $
    \ChangePasswordState{..} oldPwdValid -> do
    let pwdValid = not $ Text.null cpsPassword
        pwdConfirmValid = cpsPassword == cpsPasswordConfirm
        formValid = pwdValid && pwdConfirmValid
        ajaxSubmit st@ChangePasswordState{..} = if formValid
          then (dispatchLoggedIn $ LoggedIn.ChangePassword $ ChangePwdData cpsOldPassword cpsPassword,
                Just $ st { cpsShowErrors = True})
          else ([], Just $ st { cpsShowErrors = True})
        keyDownHandler _ evt st | keyENTER evt = ajaxSubmit st
                                | otherwise    = ([], Nothing)
    cldiv_ "form" $ do
      h1_ "Change Password"
      table_ $ tbody_ $ do
        tr_ $ do
          td_ $
            label_
              [ "htmlFor" $= "old-password"
              ] "Old password"
          td_ $ input_
            [ classNames
                [ ("auth-input", True)
                , ("invalid", not oldPwdValid)
                ]
            , "type" $= "password"
            , "value" &= cpsOldPassword
            , "autoFocus" &= True
            , "name" $= "old-password"
            , onChange $ \ev st ->
                ([], Just st { cpsOldPassword = target ev "value" })
            , onKeyDown keyDownHandler
            ]
        tr_ $ do
          td_ $
            label_
              [ "htmlFor" $= "password"
              ] "New password"
          td_ $ input_
            [ classNames
                [ ("auth-input", True)
                , ("invalid", cpsShowErrors && not pwdValid)
                ]
            , "type" $= "password"
            , "value" &= cpsPassword
            , "name" $= "password"
            , onChange $ \ev st ->
                ([], Just st { cpsPassword = target ev "value" })
            , onKeyDown keyDownHandler
            ]
        tr_ $ do
          td_ $ label_
            [ "htmlFor" $= "passwordConfirm"
            ] "Confirm new password"
          td_ $ input_
            [ classNames
                [ ("auth-input", True)
                , ("invalid", cpsShowErrors && not pwdConfirmValid)
                ]
            , "type" $= "password"
            , "value" &= cpsPasswordConfirm
            , "name" $= "passwordConfirm"
            , onChange $ \ev st ->
                ([], Just st { cpsPasswordConfirm = target ev "value"})
            , onKeyDown keyDownHandler
            ]
        tr_ $ td_
           [ "className" $= "submit"
           , "colSpan" $= "2"
           ] $ button_
             [ classNames [ ("enabled", formValid)]
             , onClick $ \_ _ st -> ajaxSubmit st
             ] "Submit"
