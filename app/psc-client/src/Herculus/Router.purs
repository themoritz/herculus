module Herculus.Router where

import Herculus.Prelude
import DOM (DOM)
import Lib.Api.Schema.Auth (UserInfo(..))
import Routing.Hash.Aff (setHash)
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Routes
  = LoggedIn UserInfo
  | SignUp
  | LogIn
  | ForgotPassword
  | Initializing

pRoute :: Match Routes
pRoute =
      SignUp <$ lit "signup"
  <|> LogIn <$ lit "login"
  <|> ForgotPassword <$ lit "reset-password"

toPath :: Routes -> String
toPath = case _ of
  SignUp -> "signup"
  LogIn -> "login"
  ForgotPassword -> "reset-password"
  Initializing -> ""
  LoggedIn _ -> ""

getLink :: Routes -> String
getLink r = "#" <> toPath r

setPath :: forall eff. Routes -> Aff (dom :: DOM | eff) Unit
setPath = setHash <<< toPath
