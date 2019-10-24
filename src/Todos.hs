{-# LANGUAGE InstanceSigs #-}

module Todos
  ( runCli,
  )
where

import Control.Monad (void, when)
import Control.Monad.Loops (untilJust)
import qualified Data.Dates as Dates
import qualified Data.Dates.Formats as Dates
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
import Text.Read (readMaybe)

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

parseNatWithCap :: Int -> String -> Maybe Int
parseNatWithCap cap s = readMaybe s >>= checkCap
  where
    checkCap x
      | x < 1 = Nothing
      | x > cap = Nothing
      | otherwise = Just x

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

itemsOrMsg :: String -> [String] -> String
itemsOrMsg msg [] = msg
itemsOrMsg _ items = formatItemsWithIndex items

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

standupMsg :: TodoFile -> TodoFile -> String
standupMsg (TodoFile prevName _ prevDone) (TodoFile _ todayTodo _) =
  "Accomplished (" ++ either (const "yesterday?") yyyyMmDd prevDate ++ ")\n"
    ++ itemsOrMsg "Looks like you did not work yesterday ...\n" prevDone
    ++ "\nTODO today\n"
    ++ itemsOrMsg "No work for today? Try\n  $ td add 'Do stuff'\n" todayTodo
  where
    prevDate = parseFileName prevName

-- Paths ---

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

accomplish :: Int -> TodoFile -> TodoFile
accomplish n (TodoFile name todo done)
  | i >= 0 && i < length todo =
    let (before, (item : rest)) = splitAt i todo
     in TodoFile name (before ++ rest) $ (done ++ [item])
  | otherwise = TodoFile name todo done
  where
    i = n - 1 -- n is the TODO number, i is the index

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

readNatWithCap :: Int -> IO Int
readNatWithCap cap =
  untilJust $ do
    putStrLn $
      "Please enter a number between 1 and "
        ++ show cap
        ++ " (or press Ctrl + c to cancel):"
    parseNatWithCap cap <$> getLine

-- Cmds ---

addCmd :: FilePath -> [FilePath] -> String -> IO ()
addCmd basePath todoFiles newTodo = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  fileType <- readTodoFile basePath $ todayFile todayName todoFiles
  file <- case addTodo newTodo <$> fileType of
    HasPrev (TodoFile _ todos dones) -> pure $ TodoFile todayName todos dones
    HasToday file -> pure file
    Error err -> Exit.die err
    NoFiles -> do
      putStrLn "Creating your first TODO file \\o/\n"
      pure $ TodoFile todayName [newTodo] []
  saveTodoFile basePath file
  putStr $ formatFileWithIndex file

lastCmd :: FilePath -> [FilePath] -> IO ()
lastCmd basePath todoFiles = do
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

standupCmd :: FilePath -> [FilePath] -> IO ()
standupCmd basePath todoFiles = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  mToday <- readTodoFile basePath $ todayFile todayName todoFiles
  mPrev <- readTodoFile basePath $ prevFile todayName todoFiles
  (prev, today) <- case (mPrev, mToday) of
    -- Yay we have both files!
    (HasPrev prev, HasToday today) -> pure (prev, today)
    -- No yesterday's file
    (HasToday _, HasToday today) -> pure (TodoFile "" [] [], today)
    -- No today's file
    (HasPrev prev, HasPrev _) -> do
      let (TodoFile _ prevTodos _) = prev
      let todayNew = TodoFile todayName prevTodos []
      saveTodoFile basePath todayNew
      putStrLn "Created TODO file from yesterday's\n"
      pure (prev, todayNew)
    (NoFiles, _) -> Exit.die "No TODO files yet. Try:\n  $ td add 'Some stuff'"
    (_, NoFiles) -> Exit.die "No TODO files yet. Try:\n  $ td add 'Some stuff'"
    (Error err, _) -> Exit.die err
    (_, Error err) -> Exit.die err
    _ -> Exit.die "Something went wrong =/"
  putStr $ standupMsg prev today

doneCmd :: FilePath -> [FilePath] -> (Int -> Maybe Int) -> IO ()
doneCmd basePath todoFiles getX = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  mToday <- readTodoFile basePath $ todayFile todayName todoFiles
  today <- case mToday of
    HasToday file -> pure file
    HasPrev (TodoFile _ todos _) -> do
      let todayNew = TodoFile todayName todos []
      saveTodoFile basePath todayNew
      putStrLn "Created TODO file from yesterday's\n"
      putStr $ formatFileWithIndex todayNew
      pure todayNew
    Error err -> Exit.die err
    NoFiles -> do
      saveTodoFile basePath $ TodoFile todayName [] []
      putStrLn "Created your first TODO file \\o/"
      putStrLn "Nothing to mark as done since you just started using TODO :)"
      Exit.exitSuccess
  let (TodoFile _ todo _) = today
  let cap = length todo
  when (cap == 0) $ do
    putStrLn "No TODOs for today :)"
    Exit.exitSuccess
  i' <- Maybe.fromMaybe (readNatWithCap cap) $ pure <$> getX cap
  let todayNew = accomplish i' today
  saveTodoFile basePath todayNew
  putStr $ formatFileWithIndex todayNew

listCmd :: FilePath -> [FilePath] -> IO ()
listCmd basePath todoFiles = do
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  mToday <- readTodoFile basePath $ todayFile todayName todoFiles
  case mToday of
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

doOnboarding :: FilePath -> IO ()
doOnboarding basePath = do
  Dir.createDirectory basePath
  todayName <- formatFileName <$> Dates.getCurrentDateTime
  saveTodoFile basePath $ TodoFile todayName [] []
  putStrLn "Welcome to TODO :)\n"
  putStrLn $ "Created TODO directory (" ++ basePath ++ ") ..."
  putStrLn "This is were your TODOs are stored\n"
  putStrLn "Also went ahead and created your first TODO file,"
  putStrLn "it's empty for now, try adding things to do:\n"
  putStrLn "  $ td add Get started with TODO"
  putStrLn "  $ td add Be Awesome\n"
  putStrLn "You can also check your TODOs for the day:\n"
  putStrLn "  $ td\n"
  putStrLn "And when you finish a task, don't forget to mark it as done:\n"
  putStrLn "  $ td done 2 # since you are already awesome ;)\n"
  putStrLn "For more check out the help:\n"
  putStrLn "  $ td help\n"
  putStrLn "That's all for now. Stay cool! Stay productive!"
  Exit.exitSuccess

help :: IO ()
help = do
  putStrLn "TODO (td), a command line tool to handle your daily tasks"
  putStrLn ""
  putStrLn "TODO is inspired by \"The Power of the TODO List\""
  putStrLn "(https://dev.to/jlhcoder/the-power-of-the-todo-list), a simple, yet powerful,"
  putStrLn "approach to keep track of your daily tasks."
  putStrLn ""
  putStrLn "TODO saves your items (on ~/.todos) in a readable format that you can"
  putStrLn "edit/read/grep yourself. Make sure to checkout the article to learn more."
  putStrLn ""
  putStrLn "TODO takes care of the files for your, so you don't have to. It creates a new"
  putStrLn "one every day when you run any of the commands. All the stuff you did the"
  putStrLn "previous day stays there and the pending items are copied to today (yup you gotta"
  putStrLn "finish what you started, eh?)"
  putStrLn ""
  putStrLn "AUTHOR: gillchristian (https://gillchristian.xyz)"
  putStrLn ""
  putStrLn "VERSION: 0.0.11"
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "  $ td [command] [arguments]"
  putStrLn ""
  putStrLn "COMMANDS:"
  putStrLn "  list:"
  putStrLn "    Show today's pending and done items."
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td list"
  putStrLn "      $ td      # no command defualts to list"
  putStrLn ""
  putStrLn "  last:"
  putStrLn "    Show previous day pending and done items."
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td last"
  putStrLn ""
  putStrLn "  add:"
  putStrLn "    Add a new pending item to today's list and show the updated list."
  putStrLn "    Use quotes when you want to use a symbol that isn't supported by the shell."
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td add Do something awesome today"
  putStrLn "      $ td add 'Do stuff (not that stuff)'"
  putStrLn ""
  putStrLn "  done:"
  putStrLn "    Mark a pending item as done (Accomplished) and show the updated list."
  putStrLn "    If no number is provided you will be prompted to input one."
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td done"
  putStrLn "      $ td done [x]"
  putStrLn ""
  putStrLn "  standup:"
  putStrLn "    List previous day done items and today's pending ones."
  putStrLn "    This serves as a report for your (you guessed it) standup."
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td standup"
  putStrLn ""
  putStrLn "  version:"
  putStrLn "    Show the version (just in case you'd like to know.)"
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td version"
  putStrLn "      $ td --version"
  putStrLn ""
  putStrLn "  help:"
  putStrLn "    Show this message. Duh!"
  putStrLn ""
  putStrLn "    Usage:"
  putStrLn "      $ td help"
  putStrLn "      $ td --help"

-- CLI ---

handleCommands :: FilePath -> [FilePath] -> [String] -> IO ()
-- Adds a todo to today's file (creates it if it doesn't exist)
handleCommands basePath todoFiles ["add"] = do
  putStrLn "What is it that you are going to do today?"
  getLine >>= addCmd basePath todoFiles
handleCommands basePath todoFiles ("add" : todo) =
  addCmd basePath todoFiles $ List.intercalate " " todo
-- Prints the last file (that is not today's one)
handleCommands basePath todoFiles ("last" : _) = lastCmd basePath todoFiles
-- Lists last's dones and today's todos
handleCommands basePath todoFiles ("standup" : _) =
  standupCmd basePath todoFiles
-- Set one of today's TODOs as Accomplished
handleCommands basePath todoFiles ["done"] =
  doneCmd basePath todoFiles $ const Nothing
handleCommands basePath todoFiles ("done" : i : _) =
  doneCmd basePath todoFiles $ \cap -> parseNatWithCap cap i
-- Show version
handleCommands _ _ ("version" : _) = putStrLn "v0.0.11"
handleCommands _ _ ("--version" : _) = putStrLn "v0.0.11"
-- Show help
handleCommands _ _ ("help" : _) = help
handleCommands _ _ ("--help" : _) = help
-- Lists today's file (creates it if it doesn't exist)
handleCommands basePath todoFiles ("list" : _) = listCmd basePath todoFiles
handleCommands basePath todoFiles _ = listCmd basePath todoFiles

runCli :: IO ()
runCli = do
  basePath <- (// ".todos") <$> Dir.getHomeDirectory
  hasBasePath <- Dir.doesDirectoryExist basePath
  when (not hasBasePath) $ doOnboarding basePath
  todoFiles <- sortTodoFiles <$> Dir.listDirectory basePath
  args <- Env.getArgs
  handleCommands basePath todoFiles args
