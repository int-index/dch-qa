module Main where

import BasePrelude as P hiding (FilePath, (%), fold)
import Data.Reflection
import Filesystem.Path.CurrentOS as FS
import Data.ByteString as BS
import Data.Text as Text
import Turtle
import qualified Turtle.Bytes as Bytes

import qualified Control.Foldl as Fold

import FromYAML
import ToHTML.Common
import ToFeed.Qa
import ToFeed.Blog
import ToYAML.Qa
import ToYAML.Blog
import Types
import ResolvePeople

newtype PeopleFile = PeopleFile { peopleFile :: FilePath }
newtype InputDir = InputDir { inputDir :: FilePath }
newtype OutputFile = OutputFile { outputFile :: FilePath }
newtype OutputDir = OutputDir { outputDir :: FilePath }

----------------------------------------------------------------------------
-- CLI parsing
----------------------------------------------------------------------------

data Command =
  GenerateQa
    { optsInputDir :: !InputDir,
      optsOutputYaml :: !(Maybe OutputFile),
      optsOutputFeed :: !(Maybe OutputFile),
      optsPeopleFile :: !PeopleFile,
      optsSiteUrl :: !SiteUrl
    }
  |
  GenerateBlog
    { optsInputDir :: !InputDir,
      optsOutputDir :: !OutputDir,
      optsOutputIndex :: !OutputFile,
      optsOutputFeed :: !(Maybe OutputFile),
      optsPeopleFile :: !PeopleFile,
      optsSiteUrl :: !SiteUrl
    }

generateQaP :: Parser Command
generateQaP = subcommand "qa" "Generate Q&A" $ do
  optsInputDir <-
    InputDir <$>
      argPath "INPUT-YAML-DIR" "The directory with Q/A yaml files"
  optsOutputYaml <- optional $
    OutputFile <$>
      argPath "OUTPUT-YAML-FILE" "The filepath for the resulting combined yaml file"
  optsOutputFeed <- optional $
    OutputFile <$>
      optPath "OUTPUT-FEED-FILE" 'f' "The filepath for the resulting feed file"
  optsPeopleFile <- (<|> pure (PeopleFile "people.yaml")) $
    PeopleFile <$>
      optPath "PEOPLE-FILE" 'p' "Path to the file with people descriptions"
  optsSiteUrl <- (<|> pure (SiteUrl "https://monadfix.io")) $
    SiteUrl <$>
      optText "SITE-URL" 'u' "Base URL of the website"
  pure GenerateQa{..}

generateBlogP :: Parser Command
generateBlogP = subcommand "blog" "Generate blog" $ do
  optsInputDir <-
    InputDir <$>
      argPath "INPUT-YAML-DIR" "The directory with post yaml files"
  optsOutputDir <-
    OutputDir <$>
      argPath "OUTPUT-YAML-DIR" "The directory for the resulting rendered files"
  optsOutputIndex <-
    OutputFile <$>
      argPath "OUTPUT-INDEX-FILE" "The directory for the resulting blog index file"
  optsOutputFeed <- optional $
    OutputFile <$>
      optPath "OUTPUT-FEED-FILE" 'f' "The filepath for the resulting feed file"
  optsPeopleFile <- (<|> pure (PeopleFile "people.yaml")) $
    PeopleFile <$>
      optPath "PEOPLE-FILE" 'p' "Path to the file with people descriptions"
  optsSiteUrl <- (<|> pure (SiteUrl "https://monadfix.io")) $
    SiteUrl <$>
      optText "SITE-URL" 'u' "Base URL of the website"
  pure GenerateBlog{..}

commandP :: Parser Command
commandP = generateQaP <|> generateBlogP

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = sh $ do
  options "Monadfix site generator" commandP >>= \case

    GenerateQa{..} -> do
      give optsSiteUrl $ do
        -- Read sessions
        people <- liftIO $ processPeople (peopleFile optsPeopleFile)
        yamlPaths <-
          fold (ls (inputDir optsInputDir))
          (Fold.prefilter isYamlExt Fold.set)
        qaSessions <- liftIO $ traverse (readQaSession people) (P.toList yamlPaths)
        -- Write YAML
        give @Target Web $
          for_ optsOutputYaml $ \OutputFile{..} -> do
            let bsSessions = qaSessionsToYaml qaSessions
            Bytes.output outputFile (pure bsSessions)
        -- Write the feed
        give @Target Feed $ do
          for_ optsOutputFeed $ \OutputFile{..} -> do
            let bsFeed = qaSessionsToFeed qaSessions
            Bytes.output outputFile (pure bsFeed)

    GenerateBlog{..} -> do
      give optsSiteUrl $ do
        people <- liftIO $ processPeople (peopleFile optsPeopleFile)
        yamlPaths <-
          fold (ls (inputDir optsInputDir))
          (Fold.prefilter isYamlExt Fold.set)
        posts <- liftIO $ traverse (readPost people) (P.toList yamlPaths)
        -- Write the index
        give @Target Web $ do
          let bsIndex = blogIndexToYaml posts
          Bytes.output (outputFile optsOutputIndex) (pure bsIndex)
        -- Write posts
        give @Target Web $ do
          for_ posts $ \post -> do
            let path = outputDir optsOutputDir </>
                       fromText (idText (postId post)) <.> "yaml"
                bsPost = postToYaml post
            Bytes.output path (pure bsPost)
        -- Write the feed
        give @Target Feed $ do
          for_ optsOutputFeed $ \OutputFile{..} -> do
            let bsFeed = blogToFeed posts
            Bytes.output outputFile (pure bsFeed)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

isYamlExt :: FilePath -> Bool
isYamlExt p = extension p == Just "yaml"

readBSFile :: FilePath -> IO ByteString
readBSFile = BS.readFile . FS.encodeString

readQaSession :: People -> FilePath -> IO (QaSession Id Person)
readQaSession people inputFilePath = process inputFilePath
  where
    process =
      toErrIO . resolvePeopleQa people <=<
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

readPost :: People -> FilePath -> IO (Post Id Person)
readPost people inputFilePath = process inputFilePath
  where
    process =
      toErrIO . resolvePeoplePost people <=<
      toErrIO . postFromYaml inputFilePath' <=<
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
