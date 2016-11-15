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
                                  onKeyDown, span_, table_, target, tbody_, td_,
                                  textarea_, tr_, view, ($=), (&=))

import           Action          (Action (Login, Logout, Signup, ToSignupForm))
import           Lib.Model.Auth  (LoginData (..), SignupData (..))
import           Store           (dispatch)


login_ :: ReactElementM eh ()
login_ = view login () mempty

data LoginViewState = LoginViewState
  { inpUserNameValue :: Text
  , inpUserNameError :: Bool
  , inpPwdValue      :: Text
  , inpPwdError      :: Bool
  } deriving (Generic, Show, NFData)

initialLoginViewState :: LoginViewState
initialLoginViewState = LoginViewState {
  inpUserNameValue = ""
  , inpUserNameError = False
  , inpPwdValue = ""
  , inpPwdError = False
}

-- validation -- TODO (jk): Move into Validation.hs
emptyText :: Text -> Bool
emptyText = Text.null

validateUserName :: Text -> Bool
validateUserName = not . emptyText

validatePwd :: Text -> Bool
validatePwd = not . emptyText

login :: ReactView ()
login = defineStatefulView "login" initialLoginViewState $ \viewState _ ->
  let validFormData viewState' = not $ inpUserNameError viewState' || inpPwdError viewState'

      loginHandler viewState' =
        if validFormData viewState then
          let loginData = LoginData (inpUserNameValue viewState') (inpPwdValue viewState')
          in  (dispatch $ Login loginData, Nothing)
        else
          ([], Nothing)

      inputKeyDownHandler _ evt vSt
        | keyENTER evt = loginHandler vSt
        | otherwise = ([], Just vSt)

  in cldiv_ "login" $ table_ $ tbody_ $ do
       tr_ $ do
         td_ $ label_
           [ "htmlFor" $= "username"
           ] "User name"
         td_ $ input_
           [ classNames
               [ ("inp", True)
               , ("error", inpUserNameError viewState)
               ]
           , "type" $= "text"
           , "value" &= inpUserNameValue viewState
           , "placeholder" $= "user name"
           , "autoFocus" &= True
           , "name" $= "username"
           , onChange $ \ev viewState' -> ([], Just viewState' {
              inpUserNameValue = target ev "value"
              , inpUserNameError = validateUserName $ target ev "value"})
           , onKeyDown inputKeyDownHandler
           ]
       tr_ $ do
         td_ $ label_
           [ "htmlFor" $= "password"
           ] "Password"
         td_ $ input_
           [ classNames
               [ ("inp", True)
               , ("error", inpPwdError viewState)
               ]
           , "type" $= "password"
           , "value" &= inpPwdValue viewState
           , "placeholder" $= "password"
           , "name" $= "password"
           , onChange $ \ev viewState' -> ([], Just viewState' {
              inpPwdValue = target ev "value"
              , inpPwdError = validatePwd $ target ev "value"})
           , onKeyDown inputKeyDownHandler
           ]
       tr_ $ td_
         [ "className" $= "submit"
         , "colSpan" $= "2"
         ] $ button_
           [ classNames [ ("enabled", validFormData viewState)]
           , onClick $ \_ _ _ -> loginHandler viewState
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

logout_ :: Text -> ReactElementM eh ()
logout_ !userName = view logout userName mempty

logout :: ReactView Text
logout = defineView "login" $ \userName -> cldiv_ "logout" $ do
  "Logged in as "
  span_ [ "className" $= "user"
    ] $ elemText userName
  "("
  button_
    [ classNames
      [ ("pure", True)
      ]
    , onClick $ \_ _ -> dispatch Logout
    ] "Logout"
  ")"

signup_ :: ReactElementM eh ()
signup_ = view signup () mempty

data SignupViewState = SignupViewState
  { signupUserName   :: Text
  , signupPwd        :: Text
  , signupPwdConfirm :: Text
  , signupIntention  :: Text
  } deriving (Generic, Show, NFData)

initialSignupViewState :: SignupViewState
initialSignupViewState = SignupViewState "" "" "" ""

signup :: ReactView ()
signup = defineStatefulView "signup" initialSignupViewState $ \viewState _ ->
  -- TODO: client-side validation
  let validFormData = not (Text.null (signupUserName viewState)
                           || Text.null (signupPwd viewState)
                           || Text.null (signupIntention viewState))
                   && signupPwd viewState == signupPwdConfirm viewState
  in cldiv_ "signup" $ do
       table_ $ tbody_ $ do
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "intention"
             ] "What are you planning to use Herculus for?"
           td_ $ textarea_
             [ "value" &= signupIntention viewState
             , "placeholder" $= "Describe a project, idea, use case or that you just play around"
             , "name" $= "intention"
             , onChange $ \ev st -> ([], Just st { signupIntention = target ev "value" })
             ] "What are you planning to use Herculus for?"
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "username"
             ] "User name"
           td_ $ input_
             [ "className" $= "inp"
             , "type" $= "text"
             , "value" &= signupUserName viewState
             , "placeholder" $= "user name"
             , "autoFocus" &= True
             , "name" $= "username"
             , onChange $ \ev st -> ([], Just st { signupUserName = target ev "value"})
             ]
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "password"
             ] "Password"
           td_ $ input_
             [ "className" $= "inp"
             , "type" $= "password"
             , "value" &= signupPwd viewState
             , "placeholder" $= "password"
             , "name" $= "password"
             , onChange $ \ev st -> ([], Just st { signupPwd = target ev "value"})
             ]
         tr_ $ do
           td_ $ label_
             [ "htmlFor" $= "passwordConfirm"
             ] "Password (again)"
           td_ $ input_
             [ "className" $= "inp"
             , "type" $= "password"
             , "value" &= signupPwdConfirm viewState
             , "placeholder" $= "password confirmation"
             , "name" $= "passwordConfirm"
             , onChange $ \ev st -> ([], Just st { signupPwdConfirm = target ev "value"})
             ]
         tr_ $ td_
           [ "className" $= "submit"
           , "colSpan" $= "2"
           ] $ button_
             [ classNames [ ("enabled", validFormData)]
             , onClick $ \_ _ _ ->
               if validFormData then
                 let signupData = SignupData
                       { suUserName = signupUserName viewState
                       , suPassword = signupPwd viewState
                       , suIntention = signupIntention viewState
                       }
                 in  (dispatch $ Signup signupData, Nothing)
               else
                 ([], Nothing)
             ] "Submit"
       cldiv_ "disclaimer" "Please be aware that this a beta version. We are still in the progress of implementing a lot of features. Bugs may occur. Your feedback to hi@herculus.io is highly appreciated. Please note that we cannot at the moment guarantee that projects will be carried over to future versions and thus data you enter now gets potentially lost."
