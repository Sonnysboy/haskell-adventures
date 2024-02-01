import Data.Map (Map)
import Data.Map qualified
import Data.Text (Text, pack, concat, append, unpack)
import Control.Monad

newtype JsonError = JsonError String
  deriving (Show)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (Map String JsonValue)
  deriving (Show, Eq)

{-

Converts a map of ToJsonables into a JsonObject

-}
fromMap :: (ToJson a) => Map String a -> JsonValue
fromMap map = JsonObject $ Data.Map.map toJson map

{--
    Converts a JsonObject to a map
-}
toMap :: (FromJson a) => JsonValue -> Map String a
toMap (JsonObject map) = snd $ Data.Map.mapEither fromJson map

instance Semigroup JsonValue where
  (<>) :: JsonValue -> JsonValue -> JsonValue
  (<>) (JsonObject map1) (JsonObject map2) = JsonObject $ map1 <> map2
  (<>) (JsonArray a) (JsonArray b) = JsonArray $ a <> b
  (<>) _ _ = JsonNull

instance Monoid JsonValue where
  mempty = JsonNull

class ToJson a where
  toJson :: a -> JsonValue

instance ToJson Double where
  toJson = JsonNumber

instance ToJson Bool where
  toJson = JsonBool

instance (ToJson a) => ToJson [a] where
  toJson xs = JsonArray $ toJson <$> xs

instance {-# OVERLAPPING #-} ToJson String where
  toJson = JsonString

class FromJson a where
  fromJson :: JsonValue -> Either JsonError a

instance FromJson Double where
  fromJson (JsonNumber num) = Right num
  fromJson _ = Left $ JsonError "only numbers"

instance FromJson String where
  fromJson (JsonString x) = Right x
  fromJson _ = Left $ JsonError "only strings"

instance FromJson Bool where
  fromJson (JsonBool x) = Right x
  fromJson _ = Left $ JsonError "only bools"

instance (FromJson a) => FromJson (Map String a) where
  fromJson (JsonObject m)
    | null $ fst value = Right $ snd value
    | otherwise = Left $ JsonError ("something went wrong while parsing: " ++ foldr (\(JsonError msg) acc -> (acc ++ msg) ++ "\n") "" (fst value))
    where
      value = Data.Map.mapEither fromJson m

instance (FromJson a) => FromJson [a] where
  fromJson (JsonArray arr) = mapM fromJson arr


-------------------

stringify :: JsonValue -> Text
stringify (JsonBool bool)     = pack $ if bool then "true" else "false"
stringify (JsonString string) = pack string
stringify (JsonNumber num)    = pack $ show num
stringify JsonNull            = pack "null"
stringify (JsonArray arr)     = pack $ "[" ++ tail (foldl (\x acc -> x ++  "," ++ unpack (stringify acc)) "" arr ++  "]")
stringify (JsonObject map)    = pack $ init (Data.Map.foldrWithKey folder "{" map) ++ "}"
    where folder k a result   = result ++ show k ++ ":" ++ unpack (stringify a) ++ ","

toString :: JsonValue -> Text
toString (JsonObject object) = pack ""
toString _ =  pack ""