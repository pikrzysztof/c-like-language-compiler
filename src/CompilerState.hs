{-# LANGUAGE FlexibleInstances, FlexibleContexts, GADTs #-}

module CompilerState (Instruction (..),
                      MemLoc (..),
                      Label,
                      Register (..),
                      add,
                      stateZero,
                      CompilerState,
                      nextId,
                      strings, showCompiled)
       where

import Data.Sequence as S
import Data.Map as M
import Gramatyka.AbsLatte
import ErrorMessages
import Data.Char (ord)

varSize :: Integer
varSize = 4

data MemLoc = ML Register Integer
            deriving (Eq, Ord)

type Label = Integer

data Instruction = Pop Register
  | Comment String
  | Push Register
  | Label Label

  | Add Register Register
  | AddInt Register Integer
  | Sub Register Register
  | Mul Register Register
  | IDiv Register
  | And Register Register
  | Or Register Register
  | TwoCompNegation Register
  | OneCompNegation Register
  | Cdq
  | PushStringLabel Label

  | Call Ident
  | IRet

  | Cmp Register Register
  | JGE Label
  | JE Label
  | JG Label
  | JZ Label
  | JECXZ Label                 -- skacz gdy ecx == 0
  | JMP Label
  | IConst Integer
  | FalseConst
  | TrueConst
  | StrConst String
  | Function Ident
  | Set MemLoc Register
  | Get Register MemLoc
  | Mov Register Register
  | PutConst Register Integer
  | Cmovge Register Register
  | Cmove Register Register
  | Cmovg Register Register
  deriving (Eq, Ord)



data CompilerState = CS { instrs :: Seq Instruction,
                          strings :: Map String Label,
                          nextId :: Label
                        } deriving (Eq, Ord, Show)

stateZero :: CompilerState
stateZero = CS { instrs = S.empty,
                 strings = M.empty,
                 nextId = 0
               }

data Register = EAX
              | EBX
              | ECX
              | EDX
              | ESP
              | EBP
              | None
                deriving (Eq, Ord, Show)

class Addable a where
  add :: a -> CompilerState -> CompilerState


instance Addable Instruction where
  add i s = s { instrs = (instrs s) |> i }

instance Addable (String, Label) where
  add (s, l) st = st { strings = insert s l (strings st) }

instance Show MemLoc where
  show (ML a b) = case a of
    None -> show $ b * varSize
    _ -> a +++ " + " +++ (b * varSize)

instance Show Instruction where
  show i = case i of
    Pop a -> "\tpop " +++ a
    Push a -> "\tpush " +++ a
    Label a -> "LABEL" +++ a +++ ":"
    Add r1 r2 -> "\tadd " +++ comma r1 r2
    AddInt r1 c -> "\tadd " +++ comma r1 (c * varSize)
    Sub r1 r2 -> "\tsub " +++ comma r1 r2
    Mul r1 r2 -> "\timul " +++ comma r1 r2
    IDiv r1 -> "\tidiv " +++ r1
    Mov r1 r2 -> "\tmov " +++ comma r1 r2
    Call (Ident ident) -> "\tcall " +++ ident
    IRet -> "\tret"
    Cmp r1 r2 -> "\tcmp " +++ comma r1 r2
    JGE l -> "\tjge " +++ showL l
    JE l -> "\tje " +++ showL l
    JG l -> "\tjg " +++ showL l
    JZ l -> "\tjz " +++ showL l
    JECXZ l -> "\tjecxz " +++ showL l
    JMP l -> "\tjmp " +++ showL l
    IConst c -> "\tpush " +++ "DWORD " +++ c
    FalseConst -> "\tpush " ++ "DWORD " +++ (0 :: Integer)
    TrueConst -> "\tpush " ++ "DWORD " +++ (1 :: Integer)
    StrConst s -> "\t.string " ++ s
    Function (Ident s) -> s +++ ":"
    Set ml r -> "\tmov " ++ "[" +++ ml +++ "], " +++ r
    Get r ml -> "\tmov " ++ r +++ ", [" +++ ml +++ "]"
    TwoCompNegation reg -> "\tneg " +++ reg
    OneCompNegation reg -> "\tnot " +++ reg
    Cdq -> "\tcdq"
    And r1 r2 -> "\tand " ++ comma r1 r2
    Or r1 r2 -> "\tor " ++ comma r1 r2
    PutConst r1 val -> "\tmov " ++ comma r1 val
    Cmovge r1 val -> "\tcmovge " ++ comma r1 val
    Cmove r1 val -> "\tcmove " ++ comma r1 val
    Cmovg r1 val -> "\tcmovg " ++ comma r1 val
    PushStringLabel lab -> "\tpush " ++ "str" +++ lab
    Comment s -> ";\t" ++ s
    where
      comma a b = a +++ ", " +++ b
      showL :: Label -> String
      showL l = "LABEL" +++ l

showCompiled :: CompilerState -> String
showCompiled (CS {instrs = is, strings = ss, nextId=_ni}) =
  "BITS 32\n" ++
  "extern printInt,printString,error,readInt,readString,concat_strings\n" ++
  "section .data\n" ++
  (Prelude.concatMap showMappedString (toList ss)) ++
  "section .text\n" ++
  "global _start\n" ++
  "_start:\n" ++
  "\tpush ebp\n" ++
  "\tmov ebp, esp\n" ++
  "\tcall main\n" ++
  "\tmov ebx, eax\n" ++
  "\tmov eax, 0x1\n" ++
  "\tint 0x80\n" ++
  (Prelude.concatMap (((flip (++) ) "\n") . show) is)
  where
     showMappedString (string, label) = "str" +++ (show label) +++ ":" +++ " db " +++ (concatMap (((flip (++)) ","). show . ord)  string) +++ " 0\n"
