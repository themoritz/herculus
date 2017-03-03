module Herculus.Utils where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.HTML.HTMLElement (focus) as DOM
import DOM.HTML.Indexed (HTMLdiv, HTMLspan)
import Data.Array (catMaybes, find, length, zip, (..))
import Data.Generic (gShow)
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

clspan :: forall p i. String -> Array (HH.IProp HTMLspan i) -> Array (HH.HTML p i) -> HH.HTML p i
clspan cls props = HH.span
  ([ HP.class_ (H.ClassName cls) ] <> props)

faIcon_ :: forall p i. String -> HH.HTML p i
faIcon_ icon = HH.i
  [ HP.class_ (H.ClassName ("fa fa-" <> icon))]
  []

clbutton_ :: forall p f. String -> H.Action f -> Array (HH.HTML p (f Unit)) -> HH.HTML p (f Unit)
clbutton_ cls query = HH.button
  [ HE.onClick (HE.input_ query)
  , HP.class_ (H.ClassName cls)
  ]

faButton_ :: forall p f. String -> H.Action f -> HH.HTML p (f Unit)
faButton_ icon query = HH.button
  [ HE.onClick (HE.input_ query)
  , HP.class_ (H.ClassName "button--pure")
  ]
  [ faIcon_ (icon <> " fa-fw fa-lg") ]

conditionalClasses
  :: forall r i. Array { cls :: String, on :: Boolean }
  -> HH.IProp ("class" :: String | r) i
conditionalClasses arr = HP.classes $ catMaybes $ arr <#> \x ->
  if x.on then Just (H.ClassName x.cls)
          else Nothing

--------------------------------------------------------------------------------

type Options a = Array
  { value :: a
  , label :: String
  }

dropdown
  :: forall p f a
   . Generic a
  => String -> Options a -> a -> (a -> H.Action f) -> HH.HTML p (f Unit)
dropdown cls options value query = HH.select
  [ HP.class_ (H.ClassName cls)
  , HE.onValueChange \val -> do
      opt <- find (\o -> gShow o.value == val) options
      pure (H.action $ query opt.value)
  , HP.value (gShow value)
  ]
  ( options <#> \opt -> HH.option
      [ HP.value (gShow opt.value) ]
      [ HH.text opt.label ]
  )

--------------------------------------------------------------------------------

focusElement
  :: forall s f g p o m eff
   . MonadEff (dom :: DOM | eff) m
  => H.RefLabel -> H.HalogenM s f g p o m Unit
focusElement ref = H.getHTMLElementRef ref >>= case _ of
  Nothing -> pure unit
  Just el -> liftEff $ DOM.focus el

--------------------------------------------------------------------------------

mkIndexed :: forall a. Array a -> Array (Tuple Int a)
mkIndexed xs = zip (0 .. length xs) xs

-- | A version of `non` specialized to `Map.empty` that does not require the
-- | `Eq` instance.
nonEmpty :: forall k v. Iso' (Maybe (Map k v)) (Map k v)
nonEmpty = iso (fromMaybe Map.empty) g
  where g m | Map.isEmpty m = Nothing
            | otherwise     = Just m
