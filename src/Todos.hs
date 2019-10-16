module Todos
  ( runCli,
  )
where

import Control.Monad (void)
import qualified Data.Dates as Dates
import qualified Data.Dates.Formats as Dates
import Data.Functor ((<&>))
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

accomplisedHeading :: Parser String
accomplisedHeading = ParsecChar.string "Accomplished"

todosHeading :: Parser String
todosHeading = ParsecChar.string "TODOS"

line :: Parser Todo
line = ParsecChar.char '-' *> ParsecChar.spaces *> restOfLine

restOfLine :: Parser String
restOfLine = Parsec.manyTill (ParsecChar.noneOf "\n\r") eolOrEof

eolOrEof :: Parser ()
eolOrEof = void ParsecChar.endOfLine <|> ParsecComb.eof

todoFileParser :: FilePath -> Parser TodoFile
todoFileParser name = do
  void $ todosHeading *> ParsecChar.endOfLine
  todos <- Parsec.many line
  void $ ParsecChar.endOfLine
  void $ accomplisedHeading
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

-- TodoFile IO ---

saveTodoFile :: FilePath -> TodoFile -> IO ()
saveTodoFile basePath tdf = writeFile (basePath // path tdf) $ show tdf

todayFile :: FilePath -> [FilePath] -> IO (Either Parsec.ParseError TodoFile)
todayFile _ [] = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  pure $ Right $ TodoFile todayName [] []
todayFile basePath (prevDayName : _) =
  Dates.getCurrentDateTime
    <&> formatFileName
    >>= readLastFile basePath prevDayName

readLastFile ::
  FilePath -> FilePath -> FilePath -> IO (Either Parsec.ParseError TodoFile)
readLastFile basePath prevDayName todayName
  | todayName == prevDayName =
    Parsec.parseFromFile (todoFileParser todayName) $ basePath // todayName
  | otherwise = do
    let fullPath = basePath // prevDayName
    eitherFile <- Parsec.parseFromFile (todoFileParser todayName) fullPath
    pure $ fromLastTodoFile <$> eitherFile

prevFile ::
  FilePath -> FilePath -> [FilePath] -> IO (Either Parsec.ParseError TodoFile)
prevFile _ todayName [] =
  pure $ Right $ TodoFile todayName [] []
prevFile basePath todayName (lastFileName : rest) =
  if todayName == lastFileName
    then prevFile basePath todayName rest
    else Parsec.parseFromFile (todoFileParser lastFileName) fullPath
  where
    fullPath = basePath // lastFileName

-- CLI ---

handleCommands :: FilePath -> [FilePath] -> [String] -> IO ()
-- Adds a todo to today's file (creates it if it doesn't exist)
--   $ td add [word...]
handleCommands basePath todoFiles ("add" : todo) = do
  let newTodo = List.intercalate " " todo
  eitherFile <- todayFile basePath todoFiles
  file <- case addTodo newTodo <$> eitherFile of
    Right file -> pure file
    Left err -> Exit.die $ show err
  saveTodoFile basePath file
  putStr $ formatFileWithIndex file
-- Prints the last file (that is not today's one)
--   $ td last
handleCommands basePath todoFiles ("last" : _) = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  eitherPrevFile <- prevFile basePath todayName todoFiles
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
  eitherFile <- todayFile basePath todoFiles
  case eitherFile of
    Right file -> do
      saveTodoFile basePath file -- TODO: only write if it's a new file?
      putStr $ formatFileWithIndex file

runCli :: IO ()
runCli = do
  basePath <- withHomeDir "~/.todos/" <$> Dir.getHomeDirectory
  todoFiles <- sortTodoFiles <$> Dir.listDirectory basePath
  args <- Env.getArgs
  handleCommands basePath todoFiles args
