module Herculus.Router where

import Herculus.Prelude
import Herculus.App as App
import Routing.Match (Match)
import Routing.Match.Class (lit)

pRoute :: Match App.View
pRoute =
      App.SignUp <$ lit "signup"
  <|> App.LogIn <$ lit "login"
  <|> App.ForgotPassword <$ lit "reset-password"
