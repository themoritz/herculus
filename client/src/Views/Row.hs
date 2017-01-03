module Views.Row where

import           React.Flux

import           Lib.Model
import           Lib.Model.Record

import           Action            (Action (TableDeleteRecord))
import           Store             (dispatch)
import           Views.Combinators

row_ :: Entity Row -> ReactElementM eh ()
row_ !c = view row c mempty

row :: ReactView (Entity Row)
row = defineView "record" $ \(Entity i _) ->
  faButton_ "minus-circle" $ dispatch $ TableDeleteRecord i
