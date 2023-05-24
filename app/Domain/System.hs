module Domain.System where

import Data.Text (Text)
import Domain.Clef

data System = System
  { name :: Text,
    clef :: Clef
  }
  deriving (Eq, Show)