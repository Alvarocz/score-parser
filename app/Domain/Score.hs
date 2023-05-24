module Domain.Score where

import Data.Text (Text)
import Domain.System

data Score = Score
  { title :: Text,
    subtitle :: Text,
    artist :: Text,
    composer :: Text,
    writer :: Text,
    year :: Int,
    text :: Text,
    tempo :: Int,
    systems :: [System]
  }
  deriving (Eq, Show)