module Domain.Clef where

data Clef
  = None
  | Treble
  | Alto
  | Tenor
  | Bass
  | BassAndTreble
  deriving (Eq, Show)