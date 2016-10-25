module Views.Record where

import           React.Flux

import           Lib.Model
import           Lib.Model.Record

import           Action            (Action (TableDeleteRecord))
import           Store             (dispatch)
import           Views.Combinators

record_ :: Entity Record -> ReactElementM eh ()
record_ !c = view record c mempty

record :: ReactView (Entity Record)
record = defineView "record" $ \(Entity i _) ->
  faButton_ "minus-circle" $ dispatch $ TableDeleteRecord i
