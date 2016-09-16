module Helper
( keyESC
, keyENTER
) where

import           React.Flux

keyESC :: KeyboardEvent -> Bool
keyESC = compareKeyCode 27

keyENTER :: KeyboardEvent -> Bool
keyENTER = compareKeyCode 13

compareKeyCode :: Int -> KeyboardEvent -> Bool
compareKeyCode keyCode' = (==) keyCode' . keyCode
