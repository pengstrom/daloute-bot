{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           Control.Applicative
import           Control.Monad.IO.Class

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser

import           Dice.Roller                      (present)

type Model = ()

data Action
  = NoOp
  | Roll Text
  deriving (Show, Read)

echoBot :: BotApp Model Action
echoBot =
  BotApp
  { botInitialModel = ()
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ =
  parseUpdate $ Roll <$> command "roll" <|> callbackQueryDataRead

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    NoOp -> pure model
    Roll text ->
      model <# do
        reply <- Text.pack <$> liftIO (present (Text.unpack text))
        replyText reply
        return NoOp

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId echoBot) env

main :: IO ()
main = do
  putStrLn "Enter token:"
  token <- Token . Text.pack <$> getLine
  run token
