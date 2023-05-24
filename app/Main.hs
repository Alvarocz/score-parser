{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Void
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Blaze.Html.Renderer.String (renderHtml)

import Domain.Score
import Domain.Clef
import Domain.PropertyStmt
import Domain.System
import Template
import Data.Text.Lazy (fromStrict)

type Parser = Parsec Void Text

instance Default Score where
  def =
    Score
      { title = ""
      , subtitle = ""
      , artist = ""
      , composer = ""
      , writer = ""
      , year = 0
      , text = ""
      , tempo = 0
      , systems = []
      }

scoreFromProperties :: Score -> [PropertyStmt] -> Score
scoreFromProperties score properties =
  case properties of
    [] -> score
    (prop:tl) ->
      scoreFromProperties new_score tl
      where new_score = case prop of
                             Title value -> score { title=value }
                             Subtitle value -> score { subtitle=value }
                             Artist value -> score { artist=value }
                             Composer value -> score { composer=value }
                             Writer value -> score { writer=value }
                             Year value -> score { year=value }
                             Text value -> score { text=value }
                             Tempo value -> score { tempo=value }
                             Oct _ -> score

spaces :: Parser ()
spaces = skipSome spaceChar

symbol :: Char -> Parser ()
symbol value = do
  skipManyTill spaceChar (char value)
  skipMany spaces

propertyName :: Text -> Parser ()
propertyName name = do
  skipMany spaceChar
  string name
  spaces

textValue :: Parser Text
textValue = T.pack <$> someTill (satisfy (/= '\n')) newline

intValue :: Parser Int
intValue = read <$> someTill digitChar newline

personNameValue :: Parser Text
personNameValue = T.pack <$> someTill (letterChar <|> oneOf (" .'" :: String)) newline

titleStmt :: Parser PropertyStmt
titleStmt = propertyName "title" >> Title <$> textValue

subtitleStmt :: Parser PropertyStmt
subtitleStmt = propertyName "subtitle" >> Subtitle <$> textValue

artistStmt :: Parser PropertyStmt
artistStmt = propertyName "artist" >> Artist <$> personNameValue

composerStmt :: Parser PropertyStmt
composerStmt = propertyName "composer" >> Composer <$> personNameValue

writerStmt :: Parser PropertyStmt
writerStmt = propertyName "writer" >> Writer <$> personNameValue

yearStmt :: Parser PropertyStmt
yearStmt = propertyName "year" >> Year <$> intValue

textStmt :: Parser PropertyStmt
textStmt = propertyName "text" >> Text <$> textValue

tempoStmt :: Parser PropertyStmt
tempoStmt = propertyName "tempo" >> Tempo <$> intValue

propertyStmt :: Parser PropertyStmt
propertyStmt =
  choice [ titleStmt
         , subtitleStmt
         , artistStmt
         , composerStmt
         , writerStmt
         , yearStmt
         , textStmt
         , tempoStmt ]

braceOpen = symbol '{'

braceClose = symbol '}'

systemBlock :: Parser System
systemBlock = do
  propertyName "system" >> braceOpen >> braceClose
  return $ System "default" None

parseProperties :: Parser [PropertyStmt]
parseProperties = someTill propertyStmt (newline >> newline)

parseScore :: Parser Score
parseScore = do
  score <- scoreFromProperties def <$> parseProperties
  systems <- many systemBlock
  return score{ systems=systems }

main :: IO ()
main = do
  args <- getArgs
  let file_name = case args of
                       (name:tl) -> name
                       [] -> error "Por favor provee un archivo staff"
  file <- T.pack <$> readFile file_name
  let score_result = runParser parseScore file_name file
  print $ either show (renderHtml . buildTemplate) score_result
