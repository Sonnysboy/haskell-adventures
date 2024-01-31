data Json = Json 
          | JsonString String
          | JsonBoolean Bool
          | JsonArray [Json]
          | JsonObject [(String, Json)]
          | JsonNumber Integer 
    deriving Show

class Jsonable a where
    fromJson :: Json -> a
    toJson   :: a -> Json
