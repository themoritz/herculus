-- |

module Views.Util where

import           Control.Lens
import           React.Flux   (ReactElementM, div_)

import           Store        (LoggedInState, SessionState (..), State,
                               stateSession)

forLoggedIn :: (LoggedInState -> a -> ReactElementM eh ()) -> State -> a -> ReactElementM eh ()
forLoggedIn func st = case st ^. stateSession of
  StateLoggedIn  liSt -> func liSt
  StateLoggedOut _    -> \_ -> div_ "Client error: Unexpected: not logged in."
