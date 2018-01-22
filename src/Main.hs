module Main where

import Control.Applicative as A
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as FS
import Data.ByteString as BS
import Data.Text
import Control.Exception
import Turtle

import FromYAML
import ToHTML
import QaSession
import ResolvePeople

newtype PeopleFile = PeopleFile { peopleFile :: FilePath }
newtype InputDir = InputDir { inputDir :: FilePath }
newtype OutputFile = OutputFile { outputFile :: FilePath }

data Options =
  Options
    { optsInputDir :: !InputDir,
      optsOutputFile :: !(Maybe OutputFile),
      optsPeopleFile :: !PeopleFile
    }

optionsP :: Parser Options
optionsP = do
  optsInputDir <-
    InputDir <$>
      argPath "INPUT-YAML-DIR" "The directory with Q/A yaml files"
  optsOutputFile <- optional $
    OutputFile <$>
      argPath "OUTPUT-HTML-FILE" "The filepath for the resulting html file"
  optsPeopleFile <- (<|> pure (PeopleFile "people.yaml")) $
    PeopleFile <$>
      optPath "PEOPLE-FILE" 'p' "Path to the file with people descriptions"
  pure Options{..}

main :: IO ()
main = sh $ do
  Options{..} <- options "DCH Q/A page generator" optionsP
  people <- processPeople (peopleFile optsPeopleFile)
  let
    out, outAppend :: Shell Line -> Shell ()
    (out, outAppend) =
      case optsOutputFile of
        Nothing -> (stdout, stdout)
        Just OutputFile{..} -> (output outputFile, Turtle.append outputFile)
  out A.empty -- cleans the file if non-empty
  yamlPath <- lsif (return . isYamlExt) (inputDir optsInputDir)
  outAppend (processQaSession people yamlPath)

isYamlExt :: FilePath -> Bool
isYamlExt p = extension p == Just "yaml"

readBSFile :: FilePath -> Shell ByteString
readBSFile = liftIO . BS.readFile . FS.encodeString

processQaSession :: People -> FilePath -> Shell Line
processQaSession people inputFilePath = do
  inputBS <- readBSFile inputFilePath
  qaSession <- either processingError return $ qaSessionFromYAML inputBS
  qaSession' <- either processingError return $ resolvePeople people qaSession
  let outputT = qaSessionToHTML qaSession'
  select $ textToLines outputT
  where
    processingError :: Exception e => e -> Shell a
    processingError exc = die $ format
      ("An error occured while processing "%fp%":\n"%s)
      inputFilePath
      (Data.Text.pack (displayException exc))

processPeople :: FilePath -> Shell People
processPeople peopleFilePath = do
  inputBS <- readBSFile peopleFilePath
  let peopleE = peopleFromYAML inputBS
  case peopleE of
    Right a -> return a
    Left exc -> die $ format
      ("The people file "%fp%" is malformed:\n"%s)
      peopleFilePath
      (Data.Text.pack (displayException exc))
