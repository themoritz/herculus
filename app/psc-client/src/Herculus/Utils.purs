module Herculus.Utils where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed (HTMLdiv)
import Data.Array (length, zip, (..))
import Data.Lens (Iso', iso)
import Data.Map (Map)

cldiv_ :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
cldiv_ cls = HH.div
  [ HP.class_ (H.ClassName cls) ]

cldiv :: forall p i. String -> Array (HH.IProp HTMLdiv i) -> Array (HH.HTML p i) -> HH.HTML p i
cldiv cls props = HH.div
  ([ HP.class_ (H.ClassName cls) ] <> props)

clspan_ :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
clspan_ cls = HH.span
  [ HP.class_ (H.ClassName cls) ]

faIcon_ :: forall p i. String -> HH.HTML p i
faIcon_ icon = HH.i
  [ HP.class_ (H.ClassName ("fa fa-" <> icon))]
  []

faButton_ :: forall p f. String -> H.Action f -> HH.HTML p (f Unit)
faButton_ icon query = HH.button
  [ HE.onClick (HE.input_ query)
  , HP.class_ (H.ClassName "button--pure")
  ]
  [ faIcon_ (icon <> " fa-fw fa-lg") ]

mkIndexed :: forall a. Array a -> Array (Tuple Int a)
mkIndexed xs = zip (0 .. length xs) xs

-- | A version of `non` specialized to `Map.empty` that does not require the
-- | `Eq` instance.
nonEmpty :: forall k v. Iso' (Maybe (Map k v)) (Map k v)
nonEmpty = iso (fromMaybe Map.empty) g
  where g m | Map.isEmpty m = Nothing
            | otherwise     = Just m
