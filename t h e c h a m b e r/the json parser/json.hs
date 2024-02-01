import Data.Map (Map)
newtype JsonError = JsonError String

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Double
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject (Map String JsonValue)
               deriving (Show, Eq)


instance Semigroup JsonValue where
    (<>) :: JsonValue -> JsonValue -> JsonValue
    (<>) (JsonObject map1) (JsonObject map2) = JsonObject $ map1 <> map2
    (<>) (JsonArray a) (JsonArray b)         = JsonArray  $ a    <> b
    (<>) _ _                                 = JsonNull

class ToJson a where
    toJson :: a -> JsonValue

instance ToJson Double where
    toJson :: Double -> JsonValue
    toJson = JsonNumber

instance ToJson Bool where
    toJson :: Bool -> JsonValue
    toJson = JsonBool 

instance ToJson String where
    toJson :: String -> JsonValue
    toJson = JsonString

instance (ToJson a) => ToJson [a] where
    toJson :: ToJson a => [a] -> JsonValue
    toJson xs = JsonArray $ toJson <$> xs


class FromJson a where
    -- could return an error if something is wrong
    fromJson :: JsonValue -> Either JsonError a

instance FromJson Double where
    fromJson (JsonNumber num) = Right num
    fromJson _                = Left $ JsonError "can only read number"
instance FromJson String where
    fromJson (JsonString x) = Right x
    fromJson _ = Left $ JsonError "can only read string"

instance (FromJson a) => FromJson [a] where
    fromJson (JsonArray x) = Right $ fromJson <$> x