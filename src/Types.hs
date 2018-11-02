{-# LANGUAGE TemplateHaskell #-}

module Types
  ( -- * QA session
    Title(..),
    Id(..),
    Featured(..),
    Highlight(..),
    Side(..),
    Class(..),
    Caption(..),
    Thumbnail(..),
    ContentPart(..),
    Message(..),
    msgAuthorL,
    msgContentL,
    msgHighlightL,
    Conversation(..),
    conversationMessagesL,
    Dates(..),
    dateAnsweredL,
    datePublishedL,
    QaSession(..),
    qassIdL,
    qassTitleL,
    qassDatesL,
    qassFeaturedL,
    qassConversationL,
    Role(..),
    Name(..),
    Alias(..),
    Pic(..),
    Link(..),
    Nickname(..),
    Person(..),
    People,

    -- * Misc
    SiteUrl(..)
  ) where

import BasePrelude
import Control.Lens
import Data.Text
import Data.Time
import Data.Map
import Data.Set
import Text.MMark

import LensUtil

newtype Highlight = Highlight Bool
  deriving (Eq, Ord, Show)

data Side = SideLeft | SideRight
  deriving (Show)

newtype Class =
  Class { className :: Text }
  deriving (Eq, Ord, Show)

newtype Caption =
  Caption { captionMMark :: MMark }
  deriving (Show)

data Thumbnail =
  Thumbnail
    { thumbnailSide :: !Side,
      thumbnailPic :: !Pic,
      thumbnailLink :: !(Maybe Link),
      thumbnailClass :: !(Set Class),
      thumbnailCaption :: !(Maybe Caption)
    }
  deriving (Show)

data ContentPart =
  ContentPart
    { contentPartThumbnail :: !(Maybe Thumbnail),
      contentPartMMark :: !MMark
    }
  deriving (Show)

data Message person =
  Message
    { msgAuthor :: !person,
      msgContent :: !(NonEmpty ContentPart),
      msgHighlight :: !Highlight
    }
  deriving (Show)

newtype Conversation person =
  Conversation { conversationMessages :: NonEmpty (Message person) }
  deriving (Show)

newtype Title =
  Title { titleMMark :: MMark }
  deriving (Show)

newtype Id =
  Id { idText :: Text }
  deriving (Eq, Ord, Show)

newtype Featured =
  Featured { featured :: Bool }
  deriving (Eq, Show)

data Dates =
  Dates
    { dateAnswered :: !Day,
      datePublished :: !Day
    }
  deriving (Show)

data QaSession id person =
  QaSession
    { qassId :: !id,
      qassTitle :: !Title,
      qassDates :: !Dates,
      qassFeatured :: !Featured,
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

newtype Pic = PicFile Text
  deriving (Eq, Ord, Show)

newtype Link = Link Text
  deriving (Eq, Ord, Show)

data Person =
  Person
    { pNicks :: ![Nickname],
      pName :: !Name,
      pAlias :: !Alias,
      pPic :: !(Maybe Pic),
      pLink :: !(Maybe Link),
      pRole :: !Role
    }
  deriving (Show)

type People = Map Nickname Person

makeLensesWith postfixLFields ''Dates
makeLensesWith postfixLFields ''Message
makeLensesWith postfixLFields ''Conversation
makeLensesWith postfixLFields ''QaSession

newtype SiteUrl = SiteUrl Text
  deriving Show
