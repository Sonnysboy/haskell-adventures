import Data.Map (Map)
newtype JsonError = JsonError String
    deriving Show

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
    toJson = JsonNumber

instance ToJson Bool where
    toJson = JsonBool 

instance ToJson String where
    toJson = JsonString

instance (ToJson a) => ToJson [a] where
    toJson xs = JsonArray $ toJson <$> xs


class FromJson a where
    fromJson :: JsonValue -> Either JsonError a

instance FromJson Double where
    fromJson (JsonNumber num) = Right num
    fromJson _                = Left $ JsonError "can only read number"
instance FromJson String where
    fromJson (JsonString x) = Right x
    fromJson _ = Left $ JsonError "can only read string"
instance FromJson Bool where
    fromJson (JsonBool x) = Right x
    fromJson _ = Left $ JsonError "can only read string"

instance (FromJson a) => FromJson [Either JsonError a] where
    fromJson (JsonArray arr) = Right $ fromJson <$> arr