module Parser where

import Text.Read (readMaybe)

data CommandType = C_ARITHMETIC | C_POP | C_PUSH
  deriving (Show, Eq)

logicCmd :: [String]
logicCmd = ["add", "sub", "neg", "and", "or", "not", "ls", "eq", "gt"]

commandType :: String -> Maybe CommandType
commandType cmd
  | cmd == "pop" = Just C_POP
  | cmd == "push" = Just C_PUSH
  | cmd `elem` logicCmd = Just C_ARITHMETIC
  | otherwise = Nothing

fstCmd :: String -> Maybe CommandType
fstCmd = commandType . head . words

nthWord :: Int -> String -> String
nthWord n = (!! n) . words

arg1 :: String -> String
arg1 line = case fstCmd line of
  Just C_ARITHMETIC -> nthWord 0 line
  Just C_POP -> nthWord 1 line
  Just C_PUSH -> nthWord 1 line
  _ -> "Parser error!"

arg2 :: String -> Maybe Int
arg2 line = case fstCmd line of
  Just C_POP -> readMaybe . nthWord 2 $ line
  Just C_PUSH -> readMaybe . nthWord 2 $ line
  _ -> Nothing

