module Herculus.Router where

import Herculus.Prelude
import Lib.Api.Schema.Project (Project) as Lib
import Lib.Model.Table (Table) as Lib
import DOM (DOM)
import Lib.Custom (Id(..))
import Routing.Hash (setHash)
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

data Root
  = LoggedIn LoggedIn
  | SignUp
  | LogIn
  | ForgotPassword

data LoggedIn
  = ProjectOverview
  | ProjectDetail ProjectDetail

data ProjectDetail
  = Project (Id Lib.Project)
  | Table (Id Lib.Table)

pRoute :: Match Root
pRoute =
      SignUp <$ lit "signup"
  <|> LogIn <$ lit "login"
  <|> ForgotPassword <$ lit "reset-password"
  <|> LoggedIn ProjectOverview <$ lit "projects"
  <|> (LoggedIn <<< ProjectDetail) <$> pProjectDetail

  where

  pProjectDetail :: Match ProjectDetail
  pProjectDetail =
        (Project <<< Id) <$> (lit "project" *> str)
    <|> (Table <<< Id) <$> (lit "table" *> str)

toPath :: Root -> String
toPath = case _ of
  SignUp -> "signup"
  LogIn -> "login"
  ForgotPassword -> "reset-password"
  LoggedIn li -> case li of
    ProjectOverview -> "projects"
    ProjectDetail pd -> case pd of
      Project (Id p) -> "project/" <> p
      Table (Id t) -> "table/" <> t

getLink :: Root -> String
getLink r = "#" <> toPath r

setPath :: forall eff. Root -> Eff (dom :: DOM | eff) Unit
setPath = setHash <<< toPath
