import Data.Map (Map)
data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Double
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject (Map String JsonValue)
               deriving (Show, Eq)
