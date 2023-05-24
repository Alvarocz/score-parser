module Domain.PropertyStmt where

import Data.Text (Text)

data PropertyStmt
  = Title Text
  | Subtitle Text
  | Artist Text
  | Composer Text
  | Writer Text
  | Year Int
  | Text Text
  | Tempo Int
  | Oct Int
  deriving (Eq, Show)