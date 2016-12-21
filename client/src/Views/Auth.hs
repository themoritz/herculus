{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Auth where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Helper          (keyENTER)

import           React.Flux      (ReactElementM, ReactView, button_, classNames,
                                  cldiv_, defineStatefulView, defineView,
                                  elemText, input_, label_, onChange, onClick,
                                  onKeyDown, p_, span_, strong_, table_, target,
                                  tbody_, td_, textarea_, tr_, view, ($=), (&=))

import           Action          (Action (Login, Logout, Signup, ToLoginForm, ToSignupForm))
import           Lib.Model.Auth  (LoginData (..), SignupData (..))
import           Store           (dispatch)


-- validation helpers

emptyText :: Text -> Bool
emptyText = Text.null

validateUserName :: Text -> Bool
validateUserName = not . emptyText

validatePwd :: Text -> Bool
validatePwd = not . emptyText


-- views

login_ :: ReactElementM eh ()
login_ = view login () mempty

data LoginViewState = LoginViewState
  { inpUserNameValue :: Text
  , inpPwdValue      :: Text
  , showLoginErrors  :: Bool
  } deriving (Generic, Show, NFData)

initialLoginViewState :: LoginViewState
initialLoginViewState = LoginViewState {
  inpUserNameValue = ""
  , inpPwdValue = ""
  , showLoginErrors = False
}

login :: ReactView ()
login = defineStatefulView "login" initialLoginViewState $ \viewState _ ->

  let inpUserNameError = not . validateUserName $ inpUserNameValue viewState
      inpPwdError = not . validatePwd $ inpPwdValue viewState
      validFormData = not (inpUserNameError || inpPwdError)

      loginHandler viewState' =
        if validFormData then
          (dispatch $ Login (LoginData (inpUserNameValue viewState') (inpPwdValue viewState'))
            , Just updatedErrorState)
        else
          ([], Just updatedErrorState)
        where
          updatedErrorState = viewState' {
             showLoginErrors = True
           }

      inputKeyDownHandler _ evt viewState'
        | keyENTER evt = loginHandler viewState'
        | otherwise = ([], Nothing)

  in cldiv_ "login" $ table_ $ tbody_ $ do
       tr_ $ do
         td_ $ label_
           [ "htmlFor" $= "username"
           ] "User name"
         td_ $ input_
           [ classNames
               [ ("auth-input", True)
               , ("invalid", showLoginErrors viewState && inpUserNameError)
               ]
           , "type" $= "text"
           , "value" &= inpUserNameValue viewState
           , "autoFocus" &= True
           , "name" $= "username"
           , onChange $ \ev viewState' -> ([], Just viewState' {
              inpUserNameValue = target ev "value"})
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
  , signupPwd        :: Text
  , signupPwdConfirm :: Text
  , signupIntention  :: Text
  , showSignupErrors :: Bool
  } deriving (Generic, Show, NFData)

initialSignupViewState :: SignupViewState
initialSignupViewState = SignupViewState {
  signupUserName = ""
  , signupPwd = ""
  , signupPwdConfirm = ""
  , signupIntention = ""
  , showSignupErrors = False
}

signup :: ReactView ()
signup = defineStatefulView "signup" initialSignupViewState $ \viewState _ ->

  let signupUserNameError = not . validateUserName $ signupUserName viewState
      equalPwdValues = signupPwd viewState == signupPwdConfirm viewState
      signupPwdError = (not . validatePwd $ signupPwd viewState)
        || not equalPwdValues
      signupPwdConfirmError = (not . validatePwd $ signupPwdConfirm viewState)
        || not equalPwdValues
      signupIntentionError = emptyText $ signupIntention viewState

      validFormData = not (signupUserNameError || signupPwdError ||
        signupPwdConfirmError || signupIntentionError)

      signupHandler viewState' =
        if validFormData then
          let signupData = SignupData
                { suUserName = signupUserName viewState'
                , suPassword = signupPwd viewState'
                , suIntention = signupIntention viewState'
                }
          in (dispatch $ Signup signupData, Just updatedErrorState)
        else
          ([], Just updatedErrorState)
        where
          updatedErrorState = viewState' {
            showSignupErrors = True
          }

      inputKeyDownHandler _ evt viewState'
       | keyENTER evt = signupHandler viewState'
       | otherwise = ([], Nothing)

  in cldiv_ "signup" $ do
       table_ $ tbody_ $ do
         tr_ $ do
           td_ $
             label_
               [ "htmlFor" $= "intention"
               ] "What are you planning to use Herculus for? Describe a project, idea, use case or that you are just going to play around."
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
             ] "User name"
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
         p_ "Please be aware that this a beta version. We are still in the progress of implementing a lot of features. Bugs may occur. Please note that we cannot at the moment guarantee that projects will be carried over to future versions and thus data you enter now potentially gets lost."
         p_ $ do
           "Your feedback to "
           strong_ "hi@herculus.io"
           " is highly appreciated."
       p_ $ do
         "Back to "
         button_
           [ classNames [ ("pure", True) ]
           , onClick $ \_ _ _ -> (dispatch ToLoginForm, Nothing)
           ] "login"
         "."
