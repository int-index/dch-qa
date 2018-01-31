module FromYAML (qaSessionFromYAML, peopleFromYAML) where

import Data.Maybe
import Data.Text
import Data.Time
import Data.ByteString
import Data.Yaml
import Data.Map as Map
import Data.Coerce
import Data.List.NonEmpty
import Data.Foldable as Foldable
import Data.HashMap.Lazy as HashMap
import Text.MMark as MMark
import QaSession

qaSessionFromYAML :: ByteString -> Either ParseException (QaSession Nickname)
qaSessionFromYAML bs = getJ <$> decodeEither' bs

getJs :: [J a] -> [a]
getJs = coerce

getJm :: Maybe (J a) -> Maybe a
getJm = coerce

getJs1 :: NonEmpty (J a) -> NonEmpty a
getJs1 = coerce

peopleFromYAML :: ByteString -> Either ParseException People
peopleFromYAML bs = do
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

instance FromJSON (J Title) where
  parseJSON =
    withText "Title" (return . J . Title)

instance FromJSON (J Day) where
  parseJSON =
    withText "Date" $ \t ->
      J <$> parseTimeM
        True -- Accept leading and trailing whitespace
        defaultTimeLocale
        "%Y-%m-%d" -- ISO-8601
        (Data.Text.unpack t)

instance p ~ Nickname => FromJSON (J (QaSession p)) where
  parseJSON =
    withObject "Q/A Session" $ \j -> do
      J qassTitle <- j .: "title"
      J qassDate <- j .: "date"
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
    withObject "Message" $ \j ->
      case HashMap.toList j of
        [(k, v)] -> do
          let msgHighlight = Highlight False
          let msgAuthor = Nickname k
          msgContent <- parseContent v
          return $ J Message{..}
        _ -> do
          (Highlight . fromMaybe False -> msgHighlight) <-
            j .:? "highlight"
          (Nickname -> msgAuthor) <- j .: "author"
          msgContent <- parseContent =<< (j .: "content")
          return $ J Message{..}
    where
      parseContent =
        withText "Content" $ \content ->
          case MMark.parse "" content of
            Left e -> fail (parseErrorsPretty content e)
            Right a -> return a

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
    withText "Pic" $ return . J . PicUrl

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
