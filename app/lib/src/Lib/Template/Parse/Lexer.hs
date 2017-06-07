-- |

module Lib.Template.Parse.Lexer where

import           Lib.Prelude

import           Lib.Compiler.Parse.Lexer

startInstr :: Parser ()
startInstr = text "{%"

endInstr :: Parser ()
endInstr = text' "%}"

startPrint :: Parser ()
startPrint = text "{{"

endPrint :: Parser ()
endPrint = text' "}}"

reservedNames :: [Text]
reservedNames = ["for", "in", "endfor", "if", "else", "endif"]
