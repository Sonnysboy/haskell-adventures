import Brainfuck 
import Data.Maybe

a :: Program
a = replicate 97 IncrementByte  ++ [OutputByte]

b :: Program
b = [AcceptByte, OutputByte]

input :: Program
input = [AcceptByte]

helloWorldString :: String
helloWorldString = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
helloWorldProgram = fromJust . parseProgram $ helloWorldString

seven = fromJust . parseProgram $ "++>+++++[<+>-]++++++++[<++++++>-]<."