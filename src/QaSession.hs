{-# OPTIONS -fno-warn-orphans #-} -- TODO: remove this

module QaSession
  ( Title(..),
    Message(..),
    Conversation(..),
    QaSession(..),
    Role(..),
    Name(..),
    Alias(..),
    Nickname(..),
    Person(..),
    People
  ) where

import Data.Text
import Data.Time
import Data.List.NonEmpty
import Data.Map
import Text.MMark

data Message =
  Message
    { msgAuthor :: !Nickname,
      msgContent :: !MMark
    }
  deriving (Show)

newtype Conversation = Conversation (NonEmpty Message)
  deriving (Show)

newtype Title = Title Text
  deriving (Show)

data QaSession =
  QaSession
    { qassTitle :: !Title,
      qassDate :: !Day,
      qassConversation :: !Conversation
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
