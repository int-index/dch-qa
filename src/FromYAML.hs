{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

{-# OPTIONS -Wno-unticked-promoted-constructors #-}

module FromYAML (qaSessionFromYaml, postFromYaml, peopleFromYaml) where

import BasePrelude
import Data.Text as Text
import Data.Time
import Data.ByteString
import Data.Yaml (ParseException, decodeEither')
import Data.Aeson.Tagged
import Data.Map as Map
import Data.Set as Set
import Data.List.NonEmpty
import Data.Foldable as Foldable
import Data.HashMap.Lazy as HashMap
import System.FilePath
import MarkdownUtil (parseMMark)
import Types

----------------------------------------------------------------------------
-- JSON tags
----------------------------------------------------------------------------

data J = JPost | JQa | JPeople

----------------------------------------------------------------------------
-- FromYAML
----------------------------------------------------------------------------

qaSessionFromYaml
  :: FilePath
  -> ByteString
  -> Either ParseException (QaSession Id Nickname)
qaSessionFromYaml fp bs = do
  session <- fromTaggedAeson @JQa <$> decodeEither' bs
  pure session {qassId = Id (Text.pack (takeBaseName fp))}

postFromYaml
  :: FilePath
  -> ByteString
  -> Either ParseException (Post Id Nickname)
postFromYaml fp bs = do
  post <- fromTaggedAeson @JPost <$> decodeEither' bs
  pure post {postId = Id (Text.pack (takeBaseName fp))}

peopleFromYaml :: ByteString -> Either ParseException People
peopleFromYaml bs = do
  personList <- fromTaggedAeson @JPeople <$> decodeEither' bs
  case buildPeople personList of
    Just a -> return a
    Nothing -> fail "Duplicate nicknames"
  where
    buildPeople :: [Person] -> Maybe People
    buildPeople persons =
      sequenceA . Map.fromListWith (\_ _ -> Nothing) $ do
        person <- persons
        nick <- pNicks person
        [(nick, Just person)]

----------------------------------------------------------------------------
-- Common instances
----------------------------------------------------------------------------

instance FromJSON (j :: J) Title where
  parseJSON =
    withText "Title" $ pure . Title . parseMMark

instance FromJSON (j :: J) Day where
  parseJSON =
    withText "Date" $ \t ->
      parseTimeM
        True -- Accept leading and trailing whitespace
        defaultTimeLocale
        "%Y-%m-%d" -- ISO-8601
        (Text.unpack t)

instance FromJSON (j :: J) ContentPart where
  parseJSON j =
      -- Note: we must not allow arrays as 'ContentPart's
      parseContentPartText j <|>
      parseContentPartObject j
    where
      parseContentPartText =
        withText "ContentPart" $ \contentText -> do
          pure $ ContentPart
              { contentPartThumbnail = Nothing,
                contentPartMMark = parseMMark contentText }
      parseContentPartObject =
        withObject "ContentPart" $ \contentObject -> do
          contentPartThumbnail <- contentObject .:? "thumbnail"
          contentPartMMark <- parseMMark <$> contentObject .: "content"
          return $ ContentPart{..}

instance FromJSON (j :: J) Thumbnail where
  parseJSON =
    withObject "Thumbnail" $ \j -> do
      thumbnailSide <- j .: "side"
      thumbnailPic <- j .: "pic"
      thumbnailLink <- j .:? "link"
      thumbnailClass <- fmap (fromMaybe mempty) $ optional $
        j .: "class" <|>
        fmap Set.singleton (j .: "class")
      thumbnailCaption <- j .:? "caption"
      return $ Thumbnail{..}

instance FromJSON (j :: J) Side where
  parseJSON =
    withText "Side" $ \t ->
      case t of
        "left" -> return SideLeft
        "right" -> return SideRight
        _ -> fail "Unknown side"

instance FromJSON (j :: J) Class where
  parseJSON =
    withText "Class" $ pure . Class

instance FromJSON (j :: J) Caption where
  parseJSON =
    withText "Caption" $ pure . Caption . parseMMark

instance FromJSON (j :: J) Nickname where
  parseJSON =
    withText "Nickname" $ return . Nickname

instance FromJSON (j :: J) Name where
  parseJSON =
    withText "Name" $ return . Name

instance FromJSON (j :: J) Alias where
  parseJSON =
    withText "Alias" $ return . Alias

instance FromJSON (j :: J) Pic where
  parseJSON =
    withText "Pic" $ return . PicFile

instance FromJSON (j :: J) Link where
  parseJSON =
    withText "Link" $ return . Link

instance FromJSON (j :: J) Role where
  parseJSON =
    withText "Role" $ \t ->
      case t of
        "client" -> return Client
        "consultant" -> return Consultant
        _ -> fail "Unknown role"

----------------------------------------------------------------------------
-- QA session instances
----------------------------------------------------------------------------

instance FromJSON JQa Featured where
  parseJSON =
    withBool "Featured" (return . Featured)

instance FromJSON JQa QaDates where
  parseJSON = \j -> singleDate j <|> recordOfDates j
    where
      singleDate j = do
        date <- parseJSON j
        return $ QaDates date date
      recordOfDates =
        withObject "Dates" $ \j -> do
          qdAnswered <- j .: "answered"
          qdPublished <- j .: "published"
          return $ QaDates{..}

instance (id ~ (), p ~ Nickname) => FromJSON JQa (QaSession id p) where
  parseJSON =
    withObject "Q/A Session" $ \j -> do
      let qassId = ()
      qassTitle <- j .: "title"
      qassDates <- j .: "date"
      qassFeatured <- fromMaybe (Featured False) <$> j .:? "featured"
      qassConversation <- j .: "conversation"
      return $ QaSession{..}

instance p ~ Nickname => FromJSON JQa (Conversation p) where
  parseJSON =
    withArray "Conversation" $ \j -> do
      case nonEmpty (Foldable.toList j) of
        Nothing -> fail "empty"
        Just js -> Conversation <$> traverse parseJSON js

instance p ~ Nickname => FromJSON JQa (Message p) where
  parseJSON =
    withObject "Message" $ \j -> do
      (Highlight . fromMaybe False -> msgHighlight) <-
        j .:? "highlight"
      case filterSlashEntities j of
        [(k, v)] -> do
          let msgAuthor = Nickname k
          msgContent <- parseContent v
          return $ Message{..}
        [] -> fail "no author and content specified for a message"
        _ -> fail "multiple slash entities encountered"
    where
      parseContent j =
        -- Assumes a 'ContentPart' can not be an array
        parseContentPart j <|>
        parseContentArray j
      parseContentPart j =
        (\a -> a :| []) <$> parseJSON @JQa j
      parseContentArray =
        withArray "Content" $ \contentParts -> do
          contentParts' <-
            case nonEmpty (Foldable.toList contentParts) of
              Nothing -> fail "empty content parts"
              Just a -> return a
          traverse parseJSON contentParts'
      filterSlashEntities obj = do
        (k, v) <- HashMap.toList obj
        a <-
          case Text.uncons k of
            Just ('/', a) -> [a]
            _ -> []
        [(a, v)]

----------------------------------------------------------------------------
-- Post instances
----------------------------------------------------------------------------

instance (id ~ (), p ~ Nickname) => FromJSON JPost (Post id p) where
  parseJSON =
    withObject "Post" $ \j -> do
      let postId = ()
      postTitle <- j .: "title"
      postDates <- j .: "date"
      postAuthor <- j .: "author"
      postBody <- j .: "body"
      return $ Post{..}

instance FromJSON JPost PostDates where
  parseJSON = \j -> singleDate j
    where
      singleDate j = do
        date <- parseJSON j
        return $ PostDates date

instance FromJSON JPost PostBody where
  parseJSON =
    withArray "PostBody" $ \contentParts -> do
      contentParts' <-
        case nonEmpty (Foldable.toList contentParts) of
          Nothing -> fail "empty content parts"
          Just a -> return a
      PostBody <$> traverse parseJSON contentParts'

----------------------------------------------------------------------------
-- People instances
----------------------------------------------------------------------------

instance FromJSON JPeople Person where
  parseJSON =
    withObject "Person" $ \j -> do
      pNicks <- j .: "nicks"
      pName <- j .: "name"
      pAlias <- j .: "alias"
      pPic <- j .:? "pic"
      pLink <- j .:? "link"
      pRole <- j .: "role"
      return $ Person{..}

----------------------------------------------------------------------------
-- Standard types
----------------------------------------------------------------------------

instance FromJSON (j :: J) Text where
  parseJSON = using @Aeson parseJSON

instance FromJSON (j :: J) Bool where
  parseJSON = using @Aeson parseJSON

instance FromJSON j a => FromJSON (j :: J) [a] where
  parseJSON = parseList

instance (Ord a, FromJSON j a) => FromJSON (j :: J) (Set a) where
  parseJSON = parseSet
