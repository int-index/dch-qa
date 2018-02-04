module ResolvePeople where

import BasePrelude
import Types

import qualified Data.Map as Map

data PersonNotFound = PersonNotFound Nickname
  deriving (Show)

instance Exception PersonNotFound where
  displayException (PersonNotFound (Nickname t)) =
    "Could not find a person with nickname " ++ show t

resolvePeople ::
  People ->
  QaSession id Nickname ->
  Either PersonNotFound (QaSession id Person)
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
