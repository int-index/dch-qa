module FromYAML (qaSessionFromYAML, peopleFromYAML) where

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

qaSessionFromYAML :: ByteString -> Either ParseException QaSession
qaSessionFromYAML bs = getJ <$> decodeEither' bs

getJs :: [J a] -> [a]
getJs = coerce

getJs1 :: NonEmpty (J a) -> NonEmpty a
getJs1 = coerce

peopleFromYAML :: ByteString -> Either ParseException People
peopleFromYAML bs = buildPeople . getJs <$> decodeEither' bs
  where
    buildPeople :: [Person] -> People
    buildPeople persons = Map.fromList $ do
      person <- persons
      nick <- pNicks person
      [(nick, person)]

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

instance FromJSON (J QaSession) where
  parseJSON =
    withObject "Q/A Session" $ \j -> do
      J qassTitle <- j .: "title"
      J qassDate <- j .: "date"
      J qassConversation <- j .: "conversation"
      return $ J QaSession{..}

instance FromJSON (J Conversation) where
  parseJSON =
    withArray "Conversation" $ \j -> do
      case nonEmpty (Foldable.toList j) of
        Nothing -> fail "empty"
        Just js -> J . Conversation . getJs1 <$> traverse parseJSON js

instance FromJSON (J Message) where
  parseJSON =
    withObject "Message" $ \j ->
      case HashMap.toList j of
        [(k, v)] -> do
          let msgAuthor = Nickname k
          msgContent <-
            (\withContent -> withText "Content" withContent v) $ \content ->
              case MMark.parse "" content of
                Left e -> fail (parseErrorsPretty content e)
                Right a -> return a
          return $ J Message{..}
        _ -> fail "malformed message"

instance FromJSON (J Person) where
  parseJSON =
    withObject "Person" $ \j -> do
      (getJs -> pNicks) <- j .: "nicks"
      J pName <- j .: "name"
      J pAlias <- j .: "alias"
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

instance FromJSON (J Role) where
  parseJSON =
    withText "Role" $ \t ->
      J <$> case t of
        "client" -> return Client
        "consultant" -> return Consultant
        _ -> fail "Unknown role"
