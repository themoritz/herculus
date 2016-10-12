{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views.Auth.Login where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           React.Flux      (ReactElementM, ReactView, button_, classNames,
                                  cldiv_, defineStatefulView, elemText, input_,
                                  onChange, onClick, span_, target, view, ($=),
                                  (&=))

import           Lib.Model.Auth  (LoginData (..))
import           Store           (Action (Login), State, dispatch)

login_ :: State -> ReactElementM eh ()
login_ !st = view login st mempty

data LoginViewState = LoginViewState
  { inpUserNameValue :: Text
  , inpPwdValue      :: Text
  } deriving (Generic, Show, NFData)

initialLoginViewState :: LoginViewState
initialLoginViewState = LoginViewState "" ""

login :: ReactView State
login = defineStatefulView "login" initialLoginViewState $ \viewState st ->
  let validFormData = (not . Text.null $ inpUserNameValue viewState) && (not . Text.null $ inpPwdValue viewState)
  in cldiv_ "login" $ do
       input_ [ "className" $= "inp"
           , "type" $= "text"
           , "value" &= inpUserNameValue viewState
           , "placeholder" $= "username"
           , "autoFocus" &= True
           , onChange $ \ev viewState' -> ([], Just viewState' { inpUserNameValue = target ev "value"})
           ]

       input_ [ "className" $= "inp"
           , "type" $= "password"
           , "value" &= inpPwdValue viewState
           , "placeholder" $= "password"
           , onChange $ \ev viewState' -> ([], Just viewState' { inpPwdValue = target ev "value"})
           ]

       button_
        [ classNames
          [ ("enabled", validFormData)
          ]
        , onClick $ \_ _ _ ->
            if validFormData then
              let loginData = LoginData (inpUserNameValue viewState) (inpPwdValue viewState)
              in
                (dispatch $ Login loginData, Nothing)
            else
              ([], Nothing)
        ] $ elemText "Submit"
