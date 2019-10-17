{-# LANGUAGE ViewPatterns #-}

module Todos
  ( runCli,
  )
where

import Control.Monad (void)
import qualified Data.Dates as Dates
import qualified Data.Dates.Formats as Dates
import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as Sys
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as ParsecChar
import qualified Text.Parsec.Combinator as ParsecComb
import Text.Parsec.Prim ((<|>))
import qualified Text.Parsec.String as Parsec
import Text.Parsec.String (Parser)
import Text.Printf (printf)

-- System ---

printStderr :: Show a => a -> IO ()
printStderr = Sys.hPrint Sys.stderr

-- Parsing ---

eolOrEof :: Parser ()
eolOrEof = void ParsecChar.endOfLine <|> ParsecComb.eof

restOfLine :: Parser String
restOfLine = Parsec.manyTill (ParsecChar.noneOf "\n\r") eolOrEof

line :: Parser Todo
line = ParsecChar.char '-' *> ParsecChar.spaces *> restOfLine

todoFileParser :: FilePath -> Parser TodoFile
todoFileParser name = do
  void $ ParsecChar.string "TODOS" *> ParsecChar.endOfLine
  todos <- Parsec.many line
  void $ ParsecChar.endOfLine
  void $ ParsecChar.string "Accomplished"
  eolOrEof
  TodoFile name todos <$> Parsec.many line

parseFileName :: String -> Either Parsec.ParseError Dates.DateTime
parseFileName = Dates.parseDateFormat "YYYY-MM-DD.txt"

-- Types ---

type Todo = String

type Done = String

data TodoFile = TodoFile FilePath [Todo] [Done]

instance Show TodoFile where
  show = formatFile

formatLine :: String -> String
formatLine item = "- " ++ item ++ "\n"

formatLineWithIndex :: (Int, String) -> String
formatLineWithIndex (i, item) = "- " ++ show i ++ ": " ++ item ++ "\n"

formatItems :: [String] -> String
formatItems = concat . fmap formatLine

formatItemsWithIndex :: [String] -> String
formatItemsWithIndex = concat . fmap formatLineWithIndex . List.zip [1 ..]

formatFile :: TodoFile -> String
formatFile (TodoFile _ todos dones) =
  "TODOS\n"
    ++ formatItems todos
    ++ "\nAccomplished\n"
    ++ formatItems dones

formatFileWithIndex :: TodoFile -> String
formatFileWithIndex (TodoFile _ todos dones) =
  "TODOS\n"
    ++ formatItemsWithIndex todos
    ++ "\nAccomplished\n"
    ++ formatItemsWithIndex dones

-- Paths ---

withHomeDir :: FilePath -> FilePath -> FilePath
withHomeDir ('~' : name) home = home ++ name
withHomeDir relative _ = relative

(//) :: FilePath -> FilePath -> FilePath
a // ('/' : b) = a ++ "/" ++ b
a // b = a ++ "/" ++ b

--- TodoFile utils ---

sortTodoFiles :: [FilePath] -> [FilePath]
sortTodoFiles = List.reverse . List.sort . filter (List.isSuffixOf ".txt")

formatFileName :: Dates.DateTime -> String
formatFileName d =
  (show $ Dates.year d) ++ "-"
    ++ (printf "%02d" $ Dates.month d)
    ++ "-"
    ++ (printf "%02d" $ Dates.day d)
    ++ ".txt"

path :: TodoFile -> FilePath
path (TodoFile name _ _) = name

addTodo :: String -> TodoFile -> TodoFile
addTodo todo (TodoFile name todos dones) = TodoFile name (todos ++ [todo]) dones

fromLastTodoFile :: TodoFile -> TodoFile
fromLastTodoFile (TodoFile name todos _) = TodoFile name todos []

yyyyMmDd :: Dates.DateTime -> String
yyyyMmDd d =
  (show $ Dates.year d) ++ "/"
    ++ (printf "%02d" $ Dates.month d)
    ++ "/"
    ++ (printf "%02d" $ Dates.day d)

-- TODO: parametrize the content
data FileType
  = NoFiles
  | HasToday FilePath -- today file exists
  | HasPrev FilePath -- today fiel does not exist
  deriving (Show, Eq)

todayFile :: FilePath -> [FilePath] -> FileType
todayFile _ [] = NoFiles
todayFile today (viewHead today -> True) = HasToday today
todayFile _ (prev : _) = HasPrev prev

viewHead :: Eq a => a -> [a] -> Bool
viewHead _ [] = False
viewHead x (y : _) = x == y

prevFile :: FilePath -> [FilePath] -> FileType
prevFile _ [] = NoFiles
prevFile today [prev] =
  if today == prev
    then HasToday today
    else HasPrev prev
prevFile today (prev : rest) =
  if today == prev
    then prevFile today rest
    else HasPrev prev

-- TodoFile IO ---

saveTodoFile :: FilePath -> TodoFile -> IO ()
saveTodoFile basePath tdf = writeFile (basePath // path tdf) $ show tdf

todayFileIO :: FilePath -> [FilePath] -> IO (Either Parsec.ParseError TodoFile)
todayFileIO basePath files = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  case todayFile todayName files of
    NoFiles -> pure $ Right $ TodoFile todayName [] []
    HasToday name ->
      let fullPath = basePath // name
       in Parsec.parseFromFile (todoFileParser todayName) fullPath
    HasPrev name -> do
      let fullPath = basePath // name
      eitherFile <- Parsec.parseFromFile (todoFileParser todayName) fullPath
      pure $ fromLastTodoFile <$> eitherFile

prevFileIO ::
  FilePath -> FilePath -> [FilePath] -> IO (Either Parsec.ParseError TodoFile)
prevFileIO basePath todayName files =
  case prevFile todayName files of
    NoFiles -> pure $ Right $ TodoFile todayName [] []
    -- TODO: what do we do? create empty file for yesterday?
    HasToday name ->
      let fullPath = basePath // name
       in Parsec.parseFromFile (todoFileParser todayName) fullPath
    HasPrev name ->
      let fullPath = basePath // name
       in Parsec.parseFromFile (todoFileParser name) fullPath

-- CLI ---

handleCommands :: FilePath -> [FilePath] -> [String] -> IO ()
-- Adds a todo to today's file (creates it if it doesn't exist)
--   $ td add [word...]
handleCommands basePath todoFiles ["add"] = do
  putStrLn "Nothing to add. Make sure to provide the TODO content:"
  putStrLn " $ td add [word...]"
handleCommands basePath todoFiles ("add" : todo) = do
  let newTodo = List.intercalate " " todo
  eitherFile <- todayFileIO basePath todoFiles
  file <- case addTodo newTodo <$> eitherFile of
    Right file -> pure file
    Left err -> Exit.die $ show err
  saveTodoFile basePath file
  putStr $ formatFileWithIndex file
-- Prints the last file (that is not today's one)
--   $ td last
handleCommands basePath todoFiles ("last" : _) = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  eitherPrevFile <- prevFileIO basePath todayName todoFiles
  prevF <- case eitherPrevFile of
    Right file -> pure file
    Left err -> Exit.die $ show err
  case parseFileName $ path prevF of
    Right date -> putStrLn $ "td file: " ++ yyyyMmDd date ++ "\n"
    Left err -> printStderr err
  putStr $ formatFileWithIndex prevF
-- Lists today's file (creates it if it doesn't exist)
--   $ td
handleCommands basePath todoFiles _ = do
  eitherFile <- todayFileIO basePath todoFiles
  case eitherFile of
    Right file -> do
      saveTodoFile basePath file -- TODO: only write if it's a new file?
      putStr $ formatFileWithIndex file
    Left err -> Exit.die $ show err

runCli :: IO ()
runCli = do
  basePath <- withHomeDir "~/.todos/" <$> Dir.getHomeDirectory
  todoFiles <- sortTodoFiles <$> Dir.listDirectory basePath
  args <- Env.getArgs
  handleCommands basePath todoFiles args
