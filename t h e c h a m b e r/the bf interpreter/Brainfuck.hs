{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Brainfuck where

import Control.Applicative
import Control.Arrow (Arrow (first))
import Control.Lens
  ( Ixed (ix),
    makeLenses,
    (%~),
    (&),
    (+~),
    (-~),
    (^.),
    (^?!),
  )
import Control.Monad ((>=>))
import Data.Maybe (fromJust)
import Data.Word
import GHC.Base (unsafeChr)
import GHC.IO.Handle (hSetBinaryMode)
import System.IO (stdin)

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> (String -> Maybe (a, String))
parse (Parser p) = p

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap transform p = Parser (fmap (first transform) . parse p)

instance Applicative Parser where
  pure a = Parser $ \x -> Just (a, x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser g = Parser $ f >=> \(r, s') -> g s' >>= \(r', s'') -> Just (r r', s'')

instance Monad Parser where
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser f) >>= g = Parser $ f >=> (\(r, s) -> parse (g r) s)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p <|> Parser r = Parser $ \x -> p x <|> r x

sat :: (Char -> Bool) -> Parser Char
sat pred = Parser parseChar'
  where
    parseChar' "" = Nothing
    parseChar' (x : xs) = if pred x then Just (x, xs) else Nothing

char :: Char -> Parser Char
char x = sat (== x)

next :: Parser Char
next = sat (const True)

-- match until
-- succeeds for everything between these two characters
between :: Char -> Char -> Parser String
between x y = char x *> many (sat $ (/=) y) <* char y

data ProgramState = ProgramState
  { _tape :: [Word8],
    _dp :: Int
  }


w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

makeLenses ''ProgramState

data Instruction where
  Instruction ::
    {runInsn :: ProgramState -> IO ProgramState} ->
    Instruction

data Instruction'
  = IncrementPointer
  | DecrementPointer
  | IncrementByte
  | DecrementByte
  | AcceptByte
  | OutputByte
  | Loop [Instruction']
  deriving (Show)

runInstr :: Instruction' -> ProgramState -> IO ProgramState
runInstr IncrementPointer = runInsn incrementPointer
runInstr DecrementPointer = runInsn decrementPointer
runInstr IncrementByte = runInsn incrementByte
runInstr DecrementByte = runInsn decrementByte
runInstr AcceptByte = runInsn acceptByte
runInstr OutputByte = runInsn outputByte
runInstr (Loop is) = runLoop is

incrementPointer, decrementPointer, incrementByte, decrementByte, acceptByte, outputByte :: Instruction
incrementPointer = Instruction $ pure . (dp +~ 1)
decrementPointer = Instruction $ pure . (dp -~ 1)

modify :: (Word8 -> Word8) -> Instruction
modify f = Instruction $ \p -> pure $ p & tape . ix (p ^. dp) %~ f

incrementByte = modify (+ 1)

decrementByte = modify $ \x -> x - 1

runLoop :: [Instruction'] -> ProgramState -> IO ProgramState
runLoop is p = do
  state <- foldl (\s i -> do _s <- s; runInstr i _s) (pure p) is
  if (_tape state !! _dp state) /= 0
    then
      runLoop is state
    else
      pure state

parseLoop :: Parser [Instruction']
parseLoop = char '[' >> many parseInstruction <* char ']'

parseInstruction :: Parser Instruction' -- how do i tell vscode not to format this
parseInstruction =
  ( next >>= \x -> Parser $ \rest ->
      (,rest) <$> case x of
        '>' -> Just IncrementPointer
        '<' -> Just DecrementPointer
        '+' -> Just IncrementByte
        '-' -> Just DecrementByte
        ',' -> Just AcceptByte
        '.' -> Just OutputByte
        _ -> Nothing
  )
    -- <|> (between '[' ']' >>= \x -> Parser $ \rest -> pure (Loop $ map (fst . fromJust . parse parseInstruction . return) x, rest))
    <|> Loop <$> parseLoop

acceptByte = Instruction $ \p -> do
  hSetBinaryMode stdin True
  c <- getChar -- get one byte
  getLine -- flush leftover
  hSetBinaryMode stdin False
  runInsn (modify $ const $ fromIntegral $ fromEnum c) p -- and set it

outputByte = Instruction $ \p -> do
  putStr $ return . unsafeChr . fromIntegral $ (p ^. tape ^?! ix (p ^. dp))
  pure p

type Program = [Instruction']

parseProgram :: String -> Maybe Program
parseProgram = fmap fst . parse (many parseInstruction)

runProgramFromFile :: FilePath -> IO ()
runProgramFromFile file = do
  lines <- readFile file
  runProgram . fromJust . parseProgram $ lines
  pure ()

runProgram :: Program -> IO ProgramState
runProgram = go (return $ ProgramState (replicate 20 0) 0)
  where
    go state [] = state
    go state (insn : insns) = do
      _state <- state
      new <- runInstr insn _state
      go (pure new) insns
