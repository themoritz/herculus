{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Auth where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           React.Flux      (ReactElementM, ReactView, a_, button_,
                                  classNames, cldiv_, defineStatefulView,
                                  defineView, elemText, input_, label_,
                                  onChange, onClick, span_, table_, target, td_,
                                  tr_, view, ($=), (&=))

import           Action          (Action (Login, Logout))
import           Lib.Model.Auth  (LoginData (..))
import           Store           (State, dispatch)


login_ :: State -> ReactElementM eh ()
login_ !st = view login st mempty

data LoginViewState = LoginViewState
  { inpUserNameValue :: Text
  , inpPwdValue      :: Text
  } deriving (Generic, Show, NFData)

initialLoginViewState :: LoginViewState
initialLoginViewState = LoginViewState "" ""
-- initialLoginViewState = LoginViewState "jens" "admin"

login :: ReactView State
login = defineStatefulView "login" initialLoginViewState $ \viewState st ->
  -- TODO: client-side validation
  let validFormData = not $ Text.null (inpUserNameValue viewState) || Text.null (inpPwdValue viewState)
  in cldiv_ "login" $ table_ $ do
       tr_ $ do
         td_ $ label_
           [ "for" $= "username"
           ] $ elemText "User name"
         td_ $ input_
           [ "className" $= "inp"
           , "type" $= "text"
           , "value" &= inpUserNameValue viewState
           , "placeholder" $= "user name"
           , "autoFocus" &= True
           , "name" $= "username"
           , onChange $ \ev viewState' -> ([], Just viewState' { inpUserNameValue = target ev "value"})
           ]
       tr_ $ do
         td_ $ label_
           [ "for" $= "password"
           ] $ elemText "Password"
         td_ $ input_
           [ "className" $= "inp"
           , "type" $= "password"
           , "value" &= inpPwdValue viewState
           , "placeholder" $= "password"
           , "name" $= "password"
           , onChange $ \ev viewState' -> ([], Just viewState' { inpPwdValue = target ev "value"})
           ]
       tr_ $ td_
         [ "className" $= "submit"
         , "colSpan" $= "2"
         ] $ button_
           [ classNames [ ("enabled", validFormData)]
           , onClick $ \_ _ _ ->
             if validFormData then
               let loginData = LoginData (inpUserNameValue viewState) (inpPwdValue viewState)
               in  (dispatch $ Login loginData, Nothing)
             else
               ([], Nothing)
           ] $ elemText "Submit"
       tr_ $ td_
         [ "colSpan" $= "2"
         ] $ do
           "Not registered yet? "
           a_
             -- TODO: safe link
             [ "href" $= "/auth/signup"
             ] $ "Sign up"
           "."

logout_ :: State -> ReactElementM eh ()
logout_ !st = view logout st mempty

logout :: ReactView State
logout = defineView "login" $ \st -> do
  span_ [ "className" $= "user"
    ] $ elemText "User"

  button_
    [ classNames
      [ ("btn", True)
      ]
    , onClick $ \_ _ -> dispatch Logout
    ] $ elemText "Logout"
