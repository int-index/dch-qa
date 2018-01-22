module ResolvePeople where

import Control.Exception
import QaSession

import qualified Data.Map as Map

data PersonNotFound = PersonNotFound Nickname
  deriving (Show)

instance Exception PersonNotFound where
  displayException (PersonNotFound (Nickname t)) =
    "Could not find a person with nickname " ++ show t

resolvePeople ::
  People ->
  QaSession Nickname ->
  Either PersonNotFound (QaSession Person)
resolvePeople people =
  (qassConversationL . conversationMessagesL . traverse . msgAuthorL)
    (resolvePerson people)

resolvePerson ::
  People ->
  Nickname ->
  Either PersonNotFound Person
resolvePerson people nickname =
  case Map.lookup nickname people of
    Just person -> Right person
    Nothing -> Left (PersonNotFound nickname)
