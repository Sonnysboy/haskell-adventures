module LogAnalysis where

-- I for information, W for warnings, E for errors
-- severity timestamp context
-- i.e I 147 mice in the air, I’m afraid, but you might catch a bat, and
-- E 2 148 #56k istereadeat lo d200ff] BOOTMEM
import Log


{-

Exercise 1 The first step is figuring out how to parse an individual
message. Define a function
parseMessage :: String -> LogMessage
which parses an individual line from the log file. For example
-}

typeFromString :: String -> Maybe MessageType
typeFromString str = case str of
  ('I' : s) -> Just Info
  ('W' : s) -> Just Warning
  ('E' : ' ' : n) -> Just (Error ((read . head . words) n))
  _ -> Nothing

-- gets the timestamp from the string
timestampFromString :: String -> TimeStamp
timestampFromString str = case typeFromString str of
  Just Info -> read (words str !! 1)
  Just Warning -> read (words str !! 1)
  Just (Error _) -> read (words str !! 2)
  Nothing -> 0

-- seperates the message from a string.
messageFromString :: String -> String
messageFromString str = case typeFromString str of
  Just Info -> (unwords . dt) wds
  Just Warning -> (unwords . dt) wds
  Just (Error _) -> (unwords . (tail . dt)) wds
  Nothing -> ""
  where
    wds = words str
    dt = tail . tail -- double tail to strip of the prefixes

parseMessage :: String -> LogMessage
parseMessage str = case typeFromString str of
  Nothing -> Unknown str
  Just Info -> LogMessage Info ts msg
  Just Warning -> LogMessage Warning ts msg
  Just (Error severity) ->  LogMessage (Error severity) ts msg
  where
    ts = timestampFromString str
    msg = messageFromString str

{-

Once we can parse one log message, we can parse a whole log file.
Define a function
parse :: String -> [LogMessage]
-}

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)




{-
Exercise 2 Define a function
insert :: LogMessage -> MessageTree -> MessageTree
which inserts a new LogMessage into an existing MessageTree, pro-
ducing a new MessageTree. insert may assume that it is given a
sorted MessageTree, and must produce a new sorted MessageTree
containing the new LogMessage in addition to the contents of the
original MessageTree.
However, note that if insert is given a LogMessage which is
Unknown, it should return the MessageTree unchanged
-}
-- are you serious

insert :: LogMessage -> MessageTree -> MessageTree

insert message Leaf = case message of
    (Unknown _) -> Leaf
    _ -> Node Leaf message Leaf

insert (Unknown msg) tree = tree
insert toInsert@(LogMessage typeOf thisTimestamp _) (Node left currentMessage@(LogMessage _ timeStamp  _) right)
  | thisTimestamp == timeStamp = Node left currentMessage right
  | thisTimestamp < timeStamp = Node (insert toInsert left) currentMessage right
  | thisTimestamp > timeStamp = Node left currentMessage (insert toInsert right)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = inOrder' tree []
inOrder' :: MessageTree -> [LogMessage] -> [LogMessage]
inOrder' (Node Leaf message Leaf) arr = [message]
inOrder' Leaf arr = arr
inOrder' tree@(Node left message right) arr = inOrder' left arr ++ [message] ++ inOrder' right arr


build :: [LogMessage] -> MessageTree
build arr = foldr insert Leaf (reverse arr)

-- WHO THE HELL MAKES THEIR NEW PROGRAMMING STUDENTS IMPLEMENT AN ENTIRE BST?!?!??!  THIS TOOK ME AN HOUR AND A HALF


{-
which takes an unsorted list of LogMessages, and returns a list of the
messages corresponding to any errors with a severity of 50 or greater,
sorted by timestamp. (Of course, you can use your functions from the
previous exercises to do the sorting.)
-}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = concatMap (\(LogMessage (Error _) _ str) -> [str]) ((inOrder . build) (filter errorFilter messages))
errorFilter :: LogMessage -> Bool
errorFilter message@(LogMessage (Error sev) _ _) = sev >= 50 -- only return Just if we have an error with severity >= 50
errorFilter _ = False
