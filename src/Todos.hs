{-# LANGUAGE InstanceSigs #-}

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

yyyyMmDd :: Dates.DateTime -> String
yyyyMmDd d =
  (show $ Dates.year d) ++ "/"
    ++ (printf "%02d" $ Dates.month d)
    ++ "/"
    ++ (printf "%02d" $ Dates.day d)

data FileType a
  = NoFiles
  | HasToday a
  | HasPrev a
  | Error String
  deriving (Show, Eq)

instance Functor FileType where
  fmap :: (a -> b) -> FileType a -> FileType b
  fmap _ NoFiles = NoFiles
  fmap f (HasToday a) = HasToday $ f a
  fmap f (HasPrev a) = HasPrev $ f a
  fmap _ (Error s) = Error s

todayFile :: FilePath -> [FilePath] -> FileType FilePath
todayFile _ [] = NoFiles
todayFile today (prev : _) =
  if today == prev
    then HasToday today
    else HasPrev prev

prevFile :: FilePath -> [FilePath] -> FileType FilePath
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

parseTodoFile :: FilePath -> FilePath -> IO (Either Parsec.ParseError TodoFile)
parseTodoFile basePath name =
  Parsec.parseFromFile (todoFileParser name) (basePath // name)

fromEither :: (a -> FileType a) -> Either Parsec.ParseError a -> FileType a
fromEither f = either (Error . show) f

readTodoFile :: FilePath -> FileType FilePath -> IO (FileType TodoFile)
readTodoFile basePath (HasPrev name) =
  fromEither HasPrev <$> parseTodoFile basePath name
readTodoFile basePath (HasToday name) =
  fromEither HasToday <$> parseTodoFile basePath name
readTodoFile _ NoFiles = pure NoFiles
readTodoFile _ (Error err) = pure $ Error err

-- CLI ---

handleCommands :: FilePath -> [FilePath] -> [String] -> IO ()
-- Adds a todo to today's file (creates it if it doesn't exist)
--   $ td add [word...]
handleCommands _ _ ["add"] = do
  putStrLn "Nothing to add. Make sure to provide the TODO content:"
  putStrLn " $ td add [word...]"
handleCommands basePath todoFiles ("add" : todo) = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  let newTodo = List.intercalate " " todo
  fileType <- readTodoFile basePath $ todayFile todayName todoFiles
  file <- case addTodo newTodo <$> fileType of
    HasPrev (TodoFile _ todos dones) -> pure $ TodoFile todayName todos dones
    HasToday file -> pure file
    Error err -> Exit.die err
    NoFiles -> do
      putStrLn "Creating your first TODO file \\o/\n"
      pure $ TodoFile todayName [] []
  saveTodoFile basePath file
  putStr $ formatFileWithIndex file
-- Prints the last file (that is not today's one)
--   $ td last
handleCommands basePath todoFiles ("last" : _) = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  eitherPrevFile <- readTodoFile basePath $ prevFile todayName todoFiles
  prevF <- case eitherPrevFile of
    HasPrev file -> pure file
    HasToday _ -> Exit.die "No last file"
    NoFiles ->
      Exit.die $
        "You don't have any TODO files yet :)\n"
          ++ "Run the add or list commands to create your first file.\n"
          ++ "  $ td\n"
          ++ "  $ td add 'Start creating todos'"
    Error err -> Exit.die err
  case parseFileName $ path prevF of
    Right date -> putStrLn $ "td file: " ++ yyyyMmDd date ++ "\n"
    Left err -> printStderr err
  putStr $ formatFileWithIndex prevF
-- Lists today's file (creates it if it doesn't exist)
--   $ td
handleCommands basePath todoFiles _ = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  eitherFile <- readTodoFile basePath $ todayFile todayName todoFiles
  case eitherFile of
    HasToday file -> putStr $ formatFileWithIndex file
    HasPrev (TodoFile _ todos _) -> do
      let todayNew = TodoFile todayName todos []
      saveTodoFile basePath todayNew
      putStrLn "Created TODO file from yesterday's\n"
      putStr $ formatFileWithIndex todayNew
    NoFiles -> do
      let file = TodoFile todayName [] []
      saveTodoFile basePath file
      putStrLn "Created your first TODO file \\o/\n"
      putStr $ formatFileWithIndex file
    Error err -> Exit.die err

runCli :: IO ()
runCli = do
  basePath <- withHomeDir "~/.todos/" <$> Dir.getHomeDirectory
  todoFiles <- sortTodoFiles <$> Dir.listDirectory basePath
  args <- Env.getArgs
  handleCommands basePath todoFiles args
