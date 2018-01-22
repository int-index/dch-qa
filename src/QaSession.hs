{-# LANGUAGE TemplateHaskell #-}

module QaSession
  ( Title(..),
    Message(..),
    msgAuthorL,
    msgContentL,
    Conversation(..),
    conversationMessagesL,
    QaSession(..),
    qassTitleL,
    qassDateL,
    qassConversationL,
    Role(..),
    Name(..),
    Alias(..),
    Nickname(..),
    Person(..),
    People
  ) where

import Control.Lens
import Data.Text
import Data.Time
import Data.List.NonEmpty
import Data.Map
import Text.MMark

import LensUtil

data Message person =
  Message
    { msgAuthor :: !person,
      msgContent :: !MMark
    }
  deriving (Show)

newtype Conversation person =
  Conversation { conversationMessages :: NonEmpty (Message person) }
  deriving (Show)

newtype Title =
  Title { titleText :: Text }
  deriving (Show)

data QaSession person =
  QaSession
    { qassTitle :: !Title,
      qassDate :: !Day,
      qassConversation :: !(Conversation person)
    }
  deriving (Show)

data Role = Client | Consultant
  deriving (Show)

newtype Nickname = Nickname Text
  deriving (Eq, Ord, Show)

newtype Name = Name Text
  deriving (Eq, Ord, Show)

newtype Alias = Alias Text
  deriving (Eq, Ord, Show)

data Person =
  Person
    { pNicks :: ![Nickname],
      pName :: !Name,
      pAlias :: !Alias,
      pRole :: !Role
    }
  deriving (Show)

type People = Map Nickname Person

makeLensesWith postfixLFields ''Message
makeLensesWith postfixLFields ''Conversation
makeLensesWith postfixLFields ''QaSession