module Calpas.Compiler.Parser
  ( Parser
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)


type Parser = Parsec Void Text
