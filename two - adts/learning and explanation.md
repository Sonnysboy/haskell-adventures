# 02-ADTS

## Highlights:

### **Maybe**

The `Maybe` type seems to be like Optional in java, and has two possible things:

- `Just <value>`
- `Nothing`

I.e how I did it in `LogAnalysis.hs`:

```hs
typeFromString :: String -> Maybe MessageType
typeFromString str = case str of
  ('I' : s) -> Just Info
  ('W' : s) -> Just Warning
  ('E' : ' ' : n) -> Just (Error ((read . head . words) n))
  _ -> Nothing
```

And used it in the same file to decide what we were going to do, i.e in `messageFromString`:

```hs
  Just Info -> (unwords . dt) wds
  Just Warning -> (unwords . dt) wds
  Just (Error _) -> (unwords . (tail . dt)) wds
  Nothing -> ""
```

<br>

### **. (yes, just a dot)**

This thing is absolutely busted, it's function composition.
`(f . g) x` = `f(g(x))`

### **ALL FUNCTIONS ARE CURRIED?!?!?**

I lost my goddamn mind when someone taught me that all functions are curried. Say we have a function:

```hs
example :: Integer -> Integer -> Integer
example x y = x + y
```

Then,

```hs
example 3 4
```

and

```hs
(example 3) 4
```

are equivalent.

### Pattern matching

too big to explain in the little attention span I have right now, but this is a pretty huge concept.

### I think I discovered lambdas but I'm not sure yet.

I was reading docs on [concatMap](http://zvon.org/other/haskell/Outputprelude/concatMap_f.html) for use in the `whatWentWrong` method, and it turned out like this:

```hs
concatMap (\(LogMessage (Error _) _ str) -> [str]) ((inOrder . build) (filter errorFilter messages))
```

Now, I'm 99% sure this is a lambda-type thing, and i'm excited to look into it.

### Why the hell did this assignment make me implemenet a binary search tree

Yep, I had to implement an entire BST for this assignment for some reason, not sure why they made me do that.

Inorder traversel took me like 45 minutes to make for some reason, I just couldn't figure it out.

```hs
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
```

#### New builtins I learned:

- `foldr`
- `concatMap`
- `filter`
- `tail`
- `words`
- `unwords`
- `map`
- `lines`
- `read`
- `.`
- `Maybe` and its constituents

#### New syntax and keywords:

- `case` as seen in almost every function I made this lesson.
- `\`, which I'm pretty sure is some lambda-like thing
- `Pattern Matching`, which is taking me a while to wrap my head around.
