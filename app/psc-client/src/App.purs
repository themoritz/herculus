module App where

import Prelude
import Api.Rest as Api
import Halogen as H
import Halogen.HTML as HH
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (modify)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Nothing, Just))
import Lib.Types (Id(..))
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Types (AppM)

type State = Unit

data Query a = Init a

app :: forall i o eff. H.Component HH.HTML Query i o (AppM eff)
app = H.lifecycleComponent
    { initialState: const unit
    , receiver: const Nothing
    , render
    , eval
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }
  where

    render :: State -> H.ComponentHTML Query
    render st = HH.div_
        [
        ]
    
    eval :: Query ~> H.ComponentDSL State Query o (AppM eff)
    eval (Init next) = do

      apiCall (Api.postProjectSetNameByProjectId "DLDJK" (Id "f")) $ \t ->
        pure unit

      pure next

-- Utils -----------------------------------------------------------------------

-- | Logs `AjaxError`s to the console.
apiCall
  :: forall eff s i o a
   . ExceptT AjaxError (AppM eff) a
  -> (a -> H.ComponentDSL s i o (AppM eff) Unit)
  -> H.ComponentDSL s i o (AppM eff) Unit
apiCall call handler = do
  result <- H.lift $ runExceptT call
  case result of
    Left err -> liftAff $ log $ errorToString err
    Right a -> handler a
