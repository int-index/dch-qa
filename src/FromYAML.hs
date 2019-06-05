module FromYAML (qaSessionFromYaml, postFromYaml, peopleFromYaml) where

import BasePrelude
import Data.Text as Text
import Data.Time
import Data.ByteString
import Data.Yaml
import Data.Map as Map
import Data.Set as Set
import Data.List.NonEmpty
import Data.Foldable as Foldable
import Data.HashMap.Lazy as HashMap
import System.FilePath
import MarkdownUtil (parseMMark)
import Types

qaSessionFromYaml
  :: FilePath
  -> ByteString
  -> Either ParseException (QaSession Id Nickname)
qaSessionFromYaml fp bs = do
  session <- getJ <$> decodeEither' bs
  pure session {qassId = Id (Text.pack (takeBaseName fp))}

postFromYaml
  :: FilePath
  -> ByteString
  -> Either ParseException (Post Id Nickname)
postFromYaml fp bs = do
  post <- getJ <$> decodeEither' bs
  pure post {postId = Id (Text.pack (takeBaseName fp))}

getJs :: [J a] -> [a]
getJs = coerce

getJm :: Maybe (J a) -> Maybe a
getJm = coerce

getJs1 :: NonEmpty (J a) -> NonEmpty a
getJs1 = coerce

getJset :: Ord a => Set (J a) -> Set a
getJset = Set.map getJ

peopleFromYaml :: ByteString -> Either ParseException People
peopleFromYaml bs = do
  (getJs -> personList) <- decodeEither' bs
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

newtype J a = J { getJ :: a }
  deriving (Eq, Ord)
----------------------------------------------------------------------------
-- Common instances
----------------------------------------------------------------------------

instance FromJSON (J Title) where
  parseJSON =
    withText "Title" $ pure . J . Title . parseMMark

instance FromJSON (J Day) where
  parseJSON =
    withText "Date" $ \t ->
      J <$> parseTimeM
        True -- Accept leading and trailing whitespace
        defaultTimeLocale
        "%Y-%m-%d" -- ISO-8601
        (Text.unpack t)

instance FromJSON (J ContentPart) where
  parseJSON j =
      -- Note: we must not allow arrays as 'ContentPart's
      parseContentPartText j <|>
      parseContentPartObject j
    where
      parseContentPartText =
        withText "ContentPart" $ \contentText -> do
          pure $ J ContentPart
              { contentPartThumbnail = Nothing,
                contentPartMMark = parseMMark contentText }
      parseContentPartObject =
        withObject "ContentPart" $ \contentObject -> do
          (getJm -> contentPartThumbnail) <- contentObject .:? "thumbnail"
          contentPartMMark <- parseMMark <$> (contentObject .: "content")
          return $ J ContentPart{..}

instance FromJSON (J Thumbnail) where
  parseJSON =
    withObject "Thumbnail" $ \j -> do
      J thumbnailSide <- j .: "side"
      J thumbnailPic <- j .: "pic"
      (getJm -> thumbnailLink) <- j .:? "link"
      thumbnailClass <- fmap (fromMaybe mempty) $ optional $
        fmap getJset (j .: "class") <|>
        fmap (Set.singleton . getJ) (j .: "class")
      (getJm -> thumbnailCaption) <- j .:? "caption"
      return $ J Thumbnail{..}

instance FromJSON (J Side) where
  parseJSON =
    withText "Side" $ \t ->
      J <$> case t of
        "left" -> return SideLeft
        "right" -> return SideRight
        _ -> fail "Unknown side"

instance FromJSON (J Class) where
  parseJSON =
    withText "Class" $ return . J . Class

instance FromJSON (J Caption) where
  parseJSON =
    withText "Caption" $ pure . J . Caption . parseMMark

instance FromJSON (J Person) where
  parseJSON =
    withObject "Person" $ \j -> do
      (getJs -> pNicks) <- j .: "nicks"
      J pName <- j .: "name"
      J pAlias <- j .: "alias"
      (getJm -> pPic) <- j .:? "pic"
      (getJm -> pLink) <- j .:? "link"
      J pRole <- j .: "role"
      return $ J Person{..}

instance FromJSON (J Nickname) where
  parseJSON =
    withText "Nickname" $ return . J . Nickname

instance FromJSON (J Name) where
  parseJSON =
    withText "Name" $ return . J . Name

instance FromJSON (J Alias) where
  parseJSON =
    withText "Alias" $ return . J . Alias

instance FromJSON (J Pic) where
  parseJSON =
    withText "Pic" $ return . J . PicFile

instance FromJSON (J Link) where
  parseJSON =
    withText "Link" $ return . J . Link

instance FromJSON (J Role) where
  parseJSON =
    withText "Role" $ \t ->
      J <$> case t of
        "client" -> return Client
        "consultant" -> return Consultant
        _ -> fail "Unknown role"

----------------------------------------------------------------------------
-- QA session instances
----------------------------------------------------------------------------

instance FromJSON (J Featured) where
  parseJSON =
    withBool "Featured" (return . J . Featured)

instance FromJSON (J QaDates) where
  parseJSON = \j -> singleDate j <|> recordOfDates j
    where
      singleDate j = do
        J date <- parseJSON j
        return $ J (QaDates date date)
      recordOfDates =
        withObject "Dates" $ \j -> do
          J qdAnswered <- j .: "answered"
          J qdPublished <- j .: "published"
          return $ J QaDates{..}

instance (id ~ (), p ~ Nickname) => FromJSON (J (QaSession id p)) where
  parseJSON =
    withObject "Q/A Session" $ \j -> do
      let qassId = ()
      J qassTitle <- j .: "title"
      J qassDates <- j .: "date"
      J qassFeatured <- fromMaybe (J (Featured False)) <$> j .:? "featured"
      J qassConversation <- j .: "conversation"
      return $ J QaSession{..}

instance p ~ Nickname => FromJSON (J (Conversation p)) where
  parseJSON =
    withArray "Conversation" $ \j -> do
      case nonEmpty (Foldable.toList j) of
        Nothing -> fail "empty"
        Just js -> J . Conversation . getJs1 <$> traverse parseJSON js

instance p ~ Nickname => FromJSON (J (Message p)) where
  parseJSON =
    withObject "Message" $ \j -> do
      (Highlight . fromMaybe False -> msgHighlight) <-
        j .:? "highlight"
      case filterSlashEntities j of
        [(k, v)] -> do
          let msgAuthor = Nickname k
          msgContent <- parseContent v
          return $ J Message{..}
        [] -> fail "no author and content specified for a message"
        _ -> fail "multiple slash entities encountered"
    where
      parseContent j =
        -- Assumes a 'ContentPart' can not be an array
        parseContentPart j <|>
        parseContentArray j
      parseContentPart j =
        (\a -> a :| []) . getJ <$> parseJSON j
      parseContentArray =
        withArray "Content" $ \contentParts -> do
          contentParts' <-
            case nonEmpty (Foldable.toList contentParts) of
              Nothing -> fail "empty content parts"
              Just a -> return a
          getJs1 <$> traverse parseJSON contentParts'
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

instance (id ~ (), p ~ Nickname) => FromJSON (J (Post id p)) where
  parseJSON =
    withObject "Post" $ \j -> do
      let postId = ()
      J postTitle <- j .: "title"
      J postDates <- j .: "date"
      J postAuthor <- j .: "author"
      J postBody <- j .: "body"
      return $ J Post{..}

instance FromJSON (J PostDates) where
  parseJSON = \j -> singleDate j
    where
      singleDate j = do
        J date <- parseJSON j
        return $ J (PostDates date)

instance FromJSON (J PostBody) where
  parseJSON =
    withArray "PostBody" $ \contentParts -> do
      contentParts' <-
        case nonEmpty (Foldable.toList contentParts) of
          Nothing -> fail "empty content parts"
          Just a -> return a
      J . PostBody . getJs1 <$> traverse parseJSON contentParts'
