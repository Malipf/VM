module Parser (Command(..), Segment(..), parseFile)  where

import Data.Maybe (mapMaybe)

data Segment = Argument | Local | Static | Constant | This | That | Pointer | Temp
  deriving (Show, Eq)

data Command
  = Push Segment Int
  | Pop Segment Int
  | Add | Sub | Neg
  | Eq | Gt | Lt
  | And | Or | Not
  deriving (Show, Eq)

parseFile :: FilePath -> IO [Command]
parseFile path = do
  contents <- readFile path
  let ls = map cleanLine (lines contents)
  return $ mapMaybe parseCommand (filterNull ls)

cleanLine :: String -> String
cleanLine = takeWhile (/= '/') . dropWhile (== ' ')

filterNull :: [String] -> [String]
filterNull = filter (not . null)

parseCommand :: String -> Maybe Command
parseCommand line =
  case words line of
    ["push", seg, n] -> Push <$> parseSegment seg <*> pure (read n)
    ["pop", seg, n]  -> Pop  <$> parseSegment seg <*> pure (read n)
    ["add"]  -> Just Add
    ["sub"]  -> Just Sub
    ["neg"]  -> Just Neg
    ["eq"]   -> Just Eq
    ["gt"]   -> Just Gt
    ["lt"]   -> Just Lt
    ["and"]  -> Just And
    ["or"]   -> Just Or
    ["not"]  -> Just Not
    _        -> Nothing

parseSegment :: String -> Maybe Segment
parseSegment s =
  case s of
    "argument" -> Just Argument
    "local"    -> Just Local
    "static"   -> Just Static
    "constant" -> Just Constant
    "this"     -> Just This
    "that"     -> Just That
    "pointer"  -> Just Pointer
    "temp"     -> Just Temp
    _          -> Nothing
