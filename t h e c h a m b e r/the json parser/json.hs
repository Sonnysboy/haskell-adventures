
module Json where

import Data.Map (Map)
import Data.Map qualified
import Data.Text (Text, pack, concat, append, unpack)
import Control.Monad

newtype JsonError = JsonError String
  deriving (Show)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInteger Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (Map String JsonValue)
  deriving (Eq, Show)

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

fromPairs :: [(String, JsonValue)] -> JsonValue
fromPairs = JsonObject . Data.Map.fromList



{-
Creates a jsonobject from the given string and value
-}
(.=) :: (ToJson a) => String -> a -> JsonValue 
(.=) key value = JsonObject (Data.Map.fromList [(key, toJson value)]) 


instance Semigroup JsonValue where
  (<>) :: JsonValue -> JsonValue -> JsonValue
  (<>) (JsonObject map1) (JsonObject map2) = JsonObject $ map1 <> map2
  (<>) (JsonArray a) (JsonArray b) = JsonArray $ a <> b
  (<>) _ _ = JsonNull

instance Monoid JsonValue where
  mempty = JsonNull

class ToJson a where
  toJson :: a -> JsonValue

instance ToJson Integer where
  toJson = JsonInteger

instance ToJson Bool where
  toJson = JsonBool

instance (ToJson a) => ToJson [a] where
  toJson xs = JsonArray $ toJson <$> xs

instance {-# OVERLAPPING #-} ToJson String where
  toJson = JsonString

class FromJson a where
  fromJson :: JsonValue -> Either JsonError a

instance FromJson Integer where
  fromJson (JsonInteger num) = Right num
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
-- instance Show JsonValue where
  -- show = unpack . stringify

stringify :: JsonValue -> Text
stringify = flip stringify' 0

stringify' :: JsonValue -> Int -> Text
stringify' (JsonBool bool)     depth = pack $ show bool
stringify' (JsonString string) depth = pack ("\"" ++ string ++ "\"")
stringify' (JsonInteger num)    depth = pack $ show num
stringify' JsonNull            depth = pack "null"
stringify' (JsonArray arr)     depth = pack $ "[" ++ tail (foldl (\x acc -> x ++  "," ++ unpack (stringify acc)) "" arr ++  "]")
stringify' (JsonObject map)    depth = pack $ init (Data.Map.foldrWithKey (folder (depth+1)) "{" map) ++ (indent $ depth) ++ "}"
  where
      folder depth k a result  =  result ++ indent depth ++ show k ++ " : " ++ unpack (stringify' a depth) ++ ","
      indent n = join $ "\n" : replicate n "  "

z = JsonObject (Data.Map.fromList [("b", JsonObject $ Data.Map.fromList [("c", JsonInteger 4), ("d", JsonArray $ map JsonInteger [1..10])]), ("e", JsonInteger 4), ("f", JsonObject $ Data.Map.fromList [("g", JsonNull), ("h", JsonString "monkeys"), ("i", JsonArray $ map (JsonString . (: [])) "this is a string into its characters wrapped as strings" )])])
