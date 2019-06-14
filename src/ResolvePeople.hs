module ResolvePeople where

import BasePrelude
import Types

import qualified Data.Map as Map

data PersonNotFound = PersonNotFound Nickname
  deriving (Show)

instance Exception PersonNotFound where
  displayException (PersonNotFound (Nickname t)) =
    "Could not find a person with nickname " ++ show t

resolvePeopleQa ::
  People ->
  QaSession id Nickname ->
  Either PersonNotFound (QaSession id Person)
resolvePeopleQa people =
  (qassConversationL . conversationMessagesL . traverse . msgAuthorL)
    (resolvePerson people)

resolvePeoplePost ::
  People ->
  Post id Nickname ->
  Either PersonNotFound (Post id Person)
resolvePeoplePost people =
  postAuthorL
    (resolvePerson people)

resolvePerson ::
  People ->
  Nickname ->
  Either PersonNotFound Person
resolvePerson people nickname =
  case Map.lookup nickname people of
    Just person -> Right person
    Nothing -> Left (PersonNotFound nickname)
