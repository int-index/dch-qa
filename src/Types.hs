{-# LANGUAGE TemplateHaskell #-}

module Types
  ( -- * QA session
    Featured(..),
    Highlight(..),
    Message(..),
    msgAuthorL,
    msgContentL,
    msgHighlightL,
    Conversation(..),
    conversationMessagesL,
    QaDates(..),
    qdAnsweredL,
    qdPublishedL,
    QaSession(..),
    qassIdL,
    qassTitleL,
    qassDatesL,
    qassFeaturedL,
    qassConversationL,

    -- * Blog
    Post(..),
    PostBody(..),
    PostDates(..),
    pdPublishedL,

    -- * Thumbnails
    Side(..),
    Class(..),
    Caption(..),
    Thumbnail(..),

    -- * Common
    Role(..),
    Name(..),
    Alias(..),
    Pic(..),
    Link(..),
    Nickname(..),
    Person(..),
    People,
    Title(..),
    Id(..),
    ContentPart(..),

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

----------------------------------------------------------------------------
-- QA session
----------------------------------------------------------------------------

newtype Highlight = Highlight Bool
  deriving (Eq, Ord, Show)

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

newtype Featured =
  Featured { featured :: Bool }
  deriving (Eq, Show)

data QaDates =
  QaDates
    { qdAnswered :: !Day,
      qdPublished :: !Day
    }
  deriving (Show)

data QaSession id person =
  QaSession
    { qassId :: !id,
      qassTitle :: !Title,
      qassDates :: !QaDates,
      qassFeatured :: !Featured,
      qassConversation :: !(Conversation person)
    }
  deriving (Show)

----------------------------------------------------------------------------
-- Blog
----------------------------------------------------------------------------

data Post id person =
  Post
    { postId :: !id,
      postTitle :: !Title,
      postDates :: !PostDates,
      postAuthor :: !person,
      postBody :: !PostBody
    }
  deriving (Show)

data PostBody =
  PostBody { postBodyPieces :: NonEmpty ContentPart }
  deriving (Show)

data PostDates =
  PostDates
    { pdPublished :: !Day
    }
  deriving (Show)

----------------------------------------------------------------------------
-- Thumbnails
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

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

newtype Title =
  Title { titleMMark :: MMark }
  deriving (Show)

newtype Id =
  Id { idText :: Text }
  deriving (Eq, Ord, Show)

data ContentPart =
  ContentPart
    { contentPartThumbnail :: !(Maybe Thumbnail),
      contentPartMMark :: !MMark
    }
  deriving (Show)

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

newtype SiteUrl = SiteUrl Text
  deriving Show

----------------------------------------------------------------------------
-- Template Haskell
----------------------------------------------------------------------------

makeLensesWith postfixLFields ''QaDates
makeLensesWith postfixLFields ''PostDates
makeLensesWith postfixLFields ''Message
makeLensesWith postfixLFields ''Conversation
makeLensesWith postfixLFields ''QaSession
