module Herculus.ProjectModules where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Ace as Ace
import Halogen.Component.ChildPath (cp1, type (\/), type (<\/>))
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv_)
import Lib.Compiler.AST.Position (Pos(..), Span(..))
import Lib.Compiler.Error (Error(..))

data Query a
  = Mark a
  | Update String a

type State =
  { val :: String
  }

type Child =
  Ace.Query <\/>
  Const Void

type Slot =
  Unit \/
  Unit

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.parentComponent
  { initialState: const
    { val: "Hallo"
    }
  , render
  , eval
  , receiver: const Nothing
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = cldiv_ "p2"
  [ HH.h1
    [ HP.class_ (HH.ClassName "h2 m0 mb3") ]
    [ HH.text "Code Modules" ]
  , HH.slot' cp1 unit Ace.comp
             { mode: "ace/mode/haskell"
             }
             (\(Ace.TextChanged v) -> Just $ H.action $ Update v)
  , HH.button
    [ HE.onClick $ HE.input_ $ Mark ]
    [ HH.text "Mark"
    ]
  ]

eval :: Query ~> H.ParentDSL State Query Child Slot Void Herc
eval = case _ of
  Mark next -> do
    H.query' cp1 unit $ H.action $ Ace.SetAnnotations
      [ Error
        { errMsg: "Foo"
        , errSpan: Span
          { spanStart: Pos
            { posLine: 1
            , posCol: 1
            }
          , spanEnd: Pos
            { posLine: 1
            , posCol: 4
            }
          }
        }
      ]
    pure next
  Update val next -> do
    modify _{ val = val }
    pure next
    
