module Herculus.Utils where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array (length, zip, (..))

cldiv_ :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
cldiv_ cls = HH.div
  [ HP.class_ (H.ClassName cls) ]

clspan_ :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
clspan_ cls = HH.span
  [ HP.class_ (H.ClassName cls) ]

faIcon_ :: forall p i. String -> HH.HTML p i
faIcon_ icon = HH.i
  [ HP.class_ (H.ClassName ("fa fa-" <> icon))]
  []

mkIndexed :: forall a. Array a -> Array (Tuple Int a)
mkIndexed xs = zip (0 .. length xs) xs
