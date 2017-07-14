{-# LANGUAGE OverloadedStrings #-}

module Main (main, parseCommand, parseWord) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Functor.Identity (Identity(Identity))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lambdabot.Main
import Modules (modulesInfo)
import Prelude hiding (filter)
import System.Environment (lookupEnv)
import System.IO.Silently (capture)
import Text.Parsec (anyChar, between, char, eof, letter, many, many1, noneOf,
  parse, spaces, string, try)
import Text.Parsec.Text (Parser)
import Web.Slack (Event(Message), SlackBot, SlackConfig(..), runBot)
import Web.Slack.Message (sendMessage)

-- | Parse a command.
parseCommand :: Parser String
parseCommand
  =  try $ spaces
    *> parsePrefix
    *> parseContent
    <* spaces
    <* eof

parsePrefix :: Parser Char
parsePrefix =  char '!'

parseContent :: Parser String
parseContent = 
  parseWord
    >>= \cmd -> spaces
    >> parseCode
    >>= \code -> return (if null code then cmd else cmd ++ " " ++ code)

parseWord :: Parser String
parseWord = many1 letter 

-- | Attempt to parse a code block surrounded in @```@. If that does not
-- work, attempt to parse inline code surrounded in @`@. Finally, if that
-- does not work, just treat the whole string as code.
parseCode :: Parser String
parseCode = try parseCodeBlock <|> try parseInlineCode <|> anyString

-- | Parses a code block surrounded in @```@.
parseCodeBlock :: Parser String
parseCodeBlock = between (string "```") (string "```") (many $ noneOf "`")

-- | Parses inline code surrounded in @`@.
parseInlineCode :: Parser String
parseInlineCode = between (char '`') (char '`') (many $ noneOf "`")

-- | Parse any string.
anyString :: Parser String
anyString = many anyChar

-------------------------------------------------------------------------------
-- Lambdabot
-------------------------------------------------------------------------------

-- | Run one or more commands against Lambdabot and capture the response.
lambdabot :: String -> IO String
lambdabot command = do
  let request = void $ lambdabotMain modulesInfo
        [onStartupCmds :=> Identity [command]]
  (response, _) <- capture request
  return response

-------------------------------------------------------------------------------
-- Slack
-------------------------------------------------------------------------------

-- | Construct a @SlackConfig@, taking the Slack API token from an environment
-- variable.
envMkSlackConfig :: String -> IO SlackConfig
envMkSlackConfig key
  =  mkSlackConfig
 <$> fromMaybe (error $ key <> " not set")
 <$> lookupEnv key

-- | Construct a @SlackConfig@ from a Slack API token.
mkSlackConfig :: String -> SlackConfig
mkSlackConfig apiToken = SlackConfig { _slackApiToken = apiToken }

-- | Construct a @SlackBot@ from a name. This bot will pass messages addressed
-- to it to 'lambdabot' and relay 'lambdabot''s response.
slackBot :: SlackBot a
slackBot (Message cid _ someMessage _ _ _) = do
  case parse parseCommand "" (decodeHtml someMessage) of
    Left _ -> return ()
    Right command -> do
      rawResponse <- liftIO (pack . decodeString <$> lambdabot command)
      sendMessage cid $ "```\n" <> rawResponse <> "```"
slackBot _ = return ()

decodeHtml :: Text -> Text
decodeHtml = toStrict . toLazyText . htmlEncodedText

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  slackConfig <- envMkSlackConfig "SLACK_API_TOKEN"
  runBot slackConfig slackBot ()