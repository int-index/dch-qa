module Main where

import BasePrelude hiding (FilePath, (%), fold)
import Data.Reflection
import Control.Applicative as A
import Filesystem.Path.CurrentOS as FS
import Data.ByteString as BS
import Data.Text as Text
import Turtle
import qualified Turtle.Bytes as Bytes

import qualified Control.Foldl as Fold

import FromYAML
import ToHTML
import ToFeed
import Types
import ResolvePeople

newtype PeopleFile = PeopleFile { peopleFile :: FilePath }
newtype InputDir = InputDir { inputDir :: FilePath }
newtype OutputFile = OutputFile { outputFile :: FilePath }

data Options =
  Options
    { optsInputDir :: !InputDir,
      optsOutputHtml :: !(Maybe OutputFile),
      optsOutputFeed :: !(Maybe OutputFile),
      optsPeopleFile :: !PeopleFile,
      optsSiteUrl :: !SiteUrl
    }

optionsP :: Parser Options
optionsP = do
  optsInputDir <-
    InputDir <$>
      argPath "INPUT-YAML-DIR" "The directory with Q/A yaml files"
  optsOutputHtml <- optional $
    OutputFile <$>
      argPath "OUTPUT-HTML-FILE" "The filepath for the resulting html file"
  optsOutputFeed <- optional $
    OutputFile <$>
      optPath "OUTPUT-FEED-FILE" 'f' "The filepath for the resulting feed file"
  optsPeopleFile <- (<|> pure (PeopleFile "people.yaml")) $
    PeopleFile <$>
      optPath "PEOPLE-FILE" 'p' "Path to the file with people descriptions"
  optsSiteUrl <- (<|> pure (SiteUrl "https://dirtcheaphaskell.io")) $
    SiteUrl <$>
      optText "SITE-URL" 'u' "Base URL of the website"
  pure Options{..}

main :: IO ()
main = sh $ do
  Options{..} <- options "DCH Q/A page generator" optionsP
  give optsSiteUrl $ do
    people <- liftIO $ processPeople (peopleFile optsPeopleFile)
    yamlPaths <-
      fold (ls (inputDir optsInputDir))
      (Fold.prefilter isYamlExt Fold.set)
    qaSessions <- liftIO $ traverse (readQaSession people) (toList yamlPaths)
    -- Write HTML
    give @Target Web $ do
      let
        outHtml, outAppendHtml :: Shell Line -> Shell ()
        (outHtml, outAppendHtml) =
          case optsOutputHtml of
            Nothing -> (stdout, stdout)
            Just OutputFile{..} -> ( output outputFile,
                                     Turtle.append outputFile )
      outHtml A.empty -- cleans the file if non-empty
      for_ qaSessions $ \qaSession -> do
        let outputT = qaSessionToHtml qaSession
        outAppendHtml (select (textToLines outputT))
    -- Write the feed
    give @Target Feed $ do
      for_ optsOutputFeed $ \OutputFile{..} -> do
        let bsFeed = give optsSiteUrl $ qaSessionsToFeed qaSessions
        Bytes.output outputFile (pure bsFeed)

isYamlExt :: FilePath -> Bool
isYamlExt p = extension p == Just "yaml"

readBSFile :: FilePath -> IO ByteString
readBSFile = BS.readFile . FS.encodeString

readQaSession :: People -> FilePath -> IO (QaSession Id Person)
readQaSession people inputFilePath = process inputFilePath
  where
    process =
      toErrIO . resolvePeople people <=<
      toErrIO . qaSessionFromYaml inputFilePath' <=<
      readBSFile

    inputFilePath' :: String
    inputFilePath' = Text.unpack (format fp inputFilePath)

    processingError :: Exception e => e -> IO a
    processingError exc = Turtle.die $ format
      ("An error occured while processing "%fp%":\n"%s)
      inputFilePath
      (Text.pack (displayException exc))

    toErrIO :: Exception e => Either e a -> IO a
    toErrIO = either processingError return

processPeople :: FilePath -> IO People
processPeople peopleFilePath = do
  inputBS <- readBSFile peopleFilePath
  let peopleE = peopleFromYaml inputBS
  case peopleE of
    Right a -> return a
    Left exc -> Turtle.die $ format
      ("The people file "%fp%" is malformed:\n"%s)
      peopleFilePath
      (Text.pack (displayException exc))
