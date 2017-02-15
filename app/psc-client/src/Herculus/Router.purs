module Herculus.Router where

import Herculus.Prelude
import DOM (DOM)
import Lib.Custom (Id(..), ProjectTag)
import Lib.Model.Table (Table) as Lib
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
  | ProjectDetail Project

data Project = Project (Id ProjectTag) (Maybe (Id Lib.Table))

pRoute :: Match Root
pRoute =
      SignUp <$ lit "signup"
  <|> LogIn <$ lit "login"
  <|> ForgotPassword <$ lit "reset-password"
  <|> LoggedIn ProjectOverview <$ lit "projects"
  <|> (LoggedIn <<< ProjectDetail) <$> pProject

pProject :: Match Project
pProject =
  let
    p = Id <$> (lit "project" *> str)
    mTable = (Just <<< Id) <$> (lit "table" *> str)
         <|> pure Nothing
  in
    Project <$> p <*> mTable

toPath :: Root -> String
toPath = case _ of
  SignUp -> "signup"
  LogIn -> "login"
  ForgotPassword -> "reset-password"
  LoggedIn li -> case li of
    ProjectOverview -> "projects"
    ProjectDetail (Project (Id p) mTable) ->
      "project/" <> p <> case mTable of
        Just (Id t) -> "/table/" <> t
        Nothing -> ""

getLink :: Root -> String
getLink r = "#" <> toPath r

setPath :: forall eff. Root -> Eff (dom :: DOM | eff) Unit
setPath = setHash <<< toPath
