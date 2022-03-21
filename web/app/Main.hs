{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Text hiding (init)

import Cob
import Cob.RecordM
import Cob.RecordM.TH
import Reflex.Dom

newtype Todo = Todo { todoText :: Text }
mkRecord ''Todo "ROMES Todos" ["Todo"]

main :: IO ()
main = do
    host     <- init <$> readFile "cob-host.secret"
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobToken
    -- Right todos <- runCob session $ rmDefinitionSearch_ ("*" :: String)
    mainWidgetWithHead headWidget (rootWidget [])

headWidget :: MonadWidget t m => m ()
headWidget = do
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css") blank

rootWidget :: MonadWidget t m => [Todo] -> m ()
rootWidget initTodos = elClass "section" "section" $ divClass "container has-text-centered" $ do
    elClass "h2" "title" $ text "Todos"
    newTodoEv <- newTodoForm
    todosDyn <- foldDyn (:) initTodos newTodoEv
    el "hr" blank
    todoListWidget todosDyn

newTodoForm :: MonadWidget t m => m (Event t Todo)
newTodoForm = divClass "field has-addons has-addons-centered" $ do
    newTodoDyn <- divClass "control" $ fmap Todo . value <$> inputElement (def & initialAttributes .~ ("type" =: "text" <> "class" =: "input"))
    (btnEl, _) <- divClass "control" $ elClass' "a" "button" $ text "Add Todo"
    pure $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl

todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)
