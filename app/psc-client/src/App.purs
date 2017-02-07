module App where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Play as Play
import Control.Coroutine (emit)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (liftAff)
import Halogen.Query.EventSource (EventSource(..), SubscribeStatus(..))
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Types (AppM)

data Query a
  = SetError String a
  | Initialize a

type State =
  { error :: String
  , var   :: Maybe (AVar String)
  }

app :: forall i o eff. H.Component HH.HTML Query i o (AppM eff)
app = H.lifecycleParentComponent
  { initialState: const { error: "", var: Nothing }
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

  where

    render :: State -> H.ParentHTML Query Play.Query Unit (AppM eff)
    render st = HH.div_
      [ renderPlay
      , HH.text st.error
      ]
      where renderPlay = case st.var of
              Nothing -> HH.text "loading"
              Just var -> HH.slot unit (Play.play (env var)) unit (const Nothing)
      

    eval :: Query ~> H.ParentDSL State Query Play.Query Unit o (AppM eff)
    eval (SetError e next) = do
      H.modify _{ error = e }
      pure next

    eval (Initialize next) = do
      var <- liftAff makeVar
      H.modify _{ var = Just var }
      H.subscribe $ EventSource $ do
        let go = do
              e <- lift $ liftAff $ takeVar var
              emit (SetError e Listening)
              go
        pure { producer: go, done: pure unit }
      pure next


-- Utils -----------------------------------------------------------------------

env :: AVar String -> Play.Env
env var =
  { runApi: apiCall (putVar var)
  }

-- | Logs `AjaxError`s to the console.
apiCall
  :: forall eff s i o
   . (forall eff'. String -> Aff (avar :: AVAR | eff') Unit)
  -> ExceptT AjaxError (H.ComponentDSL s i o (AppM eff)) Unit
  -> H.ComponentDSL s i o (AppM eff) Unit
apiCall throw action = do
  params <- H.lift ask
  result <- runExceptT action
  case result of
    Left err -> do
      let msg = errorToString err
      liftAff $ throw msg
    Right a -> do
      pure a
