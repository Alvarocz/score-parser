module Template where

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Domain.Score

buildTemplate :: Score -> Html
buildTemplate score = H.docTypeHtml $ do
    H.head $ do
        H.title $ toHtml (title score)
    H.body $ do
        H.h1 $ toHtml (title score)
