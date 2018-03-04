module FromYAML (qaSessionFromYaml, peopleFromYaml) where

import BasePrelude
import Data.Text as Text
import Data.Time
import Data.ByteString
import Data.Yaml
import Data.Map as Map
import Data.Set as Set
import Data.Coerce
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

instance FromJSON (J Title) where
  parseJSON =
    withText "Title" $ fmap (J . Title) . parseMMark

instance FromJSON (J Featured) where
  parseJSON =
    withBool "Featured" (return . J . Featured)

instance FromJSON (J Day) where
  parseJSON =
    withText "Date" $ \t ->
      J <$> parseTimeM
        True -- Accept leading and trailing whitespace
        defaultTimeLocale
        "%Y-%m-%d" -- ISO-8601
        (Text.unpack t)

instance (id ~ (), p ~ Nickname) => FromJSON (J (QaSession id p)) where
  parseJSON =
    withObject "Q/A Session" $ \j -> do
      let qassId = ()
      J qassTitle <- j .: "title"
      J qassDate <- j .: "date"
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
        parseContentText j <|>
        parseContentArray j
      parseContentText j =
        (\a -> a :| []) <$> parseContentPartText j
      parseContentArray =
        withArray "Content" $ \contentParts -> do
          contentParts' <-
            case nonEmpty (Foldable.toList contentParts) of
              Nothing -> fail "empty content parts"
              Just a -> return a
          for contentParts' parseContentPart
      parseContentPart j =
        parseContentPartText j <|>
        parseContentPartObject j
      parseContentPartText =
        withText "ContentPart" $ \contentText -> do
          a <- parseMMark contentText
          return $
            ContentPart
              { contentPartThumbnail = Nothing,
                contentPartMMark = a }
      parseContentPartObject =
        withObject "ContentPart" $ \j -> do
          (getJm -> contentPartThumbnail) <- j .:? "thumbnail"
          contentPartMMark <- parseMMark =<< (j .: "content")
          return $ ContentPart{..}
      filterSlashEntities obj = do
        (k, v) <- HashMap.toList obj
        a <-
          case Text.uncons k of
            Just ('/', a) -> [a]
            _ -> []
        [(a, v)]

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
    withText "Caption" $ fmap (J . Caption) . parseMMark

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
