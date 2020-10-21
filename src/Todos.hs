{-# LANGUAGE InstanceSigs #-}

module Todos
  ( runCli,
  )
where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (untilJust)
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Dates as Dates
import qualified Data.Dates.Formats as Dates
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.FilePath ((</>))
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

putStrLnStderr :: String -> IO ()
putStrLnStderr = Sys.hPutStrLn Sys.stderr

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
  void ParsecChar.endOfLine
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

newtype Env
  = Env {getBasePath :: FilePath}
  deriving (Show, Eq)

type App = R.ReaderT Env IO

type Todo = String

type Done = String

data TodoFile = TodoFile FilePath [Todo] [Done]

instance Show TodoFile where
  show = formatFile

formatLine :: String -> String
formatLine item = "- " ++ item ++ "\n"

formatLineWithIndex :: (Int, String) -> String
formatLineWithIndex (i, item) = show i ++ ") " ++ item ++ "\n"

formatItems :: [String] -> String
formatItems = concatMap formatLine

formatItemsWithIndex :: [String] -> String
formatItemsWithIndex = concatMap formatLineWithIndex . List.zip [1 ..]

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
formatFileWithIndex (TodoFile _ [] []) =
  "No TODOs for today. Add some?\n\n"
    ++ "  $ td add Something important\n"
formatFileWithIndex (TodoFile _ [] dones) =
  "Accomplished\n"
    ++ formatItemsWithIndex dones
formatFileWithIndex (TodoFile _ todos []) =
  "TODO\n"
    ++ formatItemsWithIndex todos
formatFileWithIndex (TodoFile _ todos dones) =
  "TODO\n"
    ++ formatItemsWithIndex todos
    ++ "\nAccomplished\n"
    ++ formatItemsWithIndex dones

standupMsg :: TodoFile -> TodoFile -> String
standupMsg (TodoFile prevName _ prevDone) (TodoFile _ todayTodo _) =
  -- TODO: 'yesterday' -> '
  "Accomplished (" ++ either (const "yesterday?") yyyyMmDd ePrevDate ++ ")\n"
    ++ itemsOrMsg "Looks like you did not work yesterday ...\n" prevDone
    ++ "\nTODO today\n"
    ++ itemsOrMsg "No work for today? Try\n  $ td add 'Do stuff'\n" todayTodo
  where
    ePrevDate = parseFileName prevName

--- TodoFile utils ---

sortTodoFiles :: [FilePath] -> [FilePath]
sortTodoFiles = List.sortOn Ord.Down . filter (List.isSuffixOf ".txt")

formatFileName :: Dates.DateTime -> String
formatFileName d =
  show (Dates.year d) ++ "-"
    ++ printf "%02d" (Dates.month d)
    ++ "-"
    ++ printf "%02d" (Dates.day d)
    ++ ".txt"

path :: TodoFile -> FilePath
path (TodoFile name _ _) = name

addTodo :: String -> TodoFile -> TodoFile
addTodo todo (TodoFile name todos dones) = TodoFile name (todos ++ [todo]) dones

yyyyMmDd :: Dates.DateTime -> String
yyyyMmDd d =
  show (Dates.year d) ++ "/"
    ++ printf "%02d" (Dates.month d)
    ++ "/"
    ++ printf "%02d" (Dates.day d)

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
    let (before, item : rest) = splitAt i todo
     in TodoFile name (before ++ rest) (done ++ [item])
  | otherwise = TodoFile name todo done
  where
    i = n - 1 -- n is the TODO number, i is the index

-- TodoFile Effects ---

saveTodoFile :: TodoFile -> App ()
saveTodoFile tdf = do
  filePath <- R.asks ((</> path tdf) . getBasePath)
  liftIO $ writeFile filePath $ show tdf

parseTodoFile :: FilePath -> App (Either Parsec.ParseError TodoFile)
parseTodoFile name = do
  filePath <- R.asks ((</> name) . getBasePath)
  liftIO $ Parsec.parseFromFile (todoFileParser name) filePath

fromEither :: (a -> FileType a) -> Either Parsec.ParseError a -> FileType a
fromEither = either (Error . show)

readTodoFile :: FileType FilePath -> App (FileType TodoFile)
readTodoFile (HasPrev name) = fromEither HasPrev <$> parseTodoFile name
readTodoFile (HasToday name) = fromEither HasToday <$> parseTodoFile name
readTodoFile NoFiles = pure NoFiles
readTodoFile (Error err) = pure $ Error err

readNatWithCap :: Int -> IO Int
readNatWithCap 1 = do
  putStrLn $
    "Only one todo left today, do you want to complete that one? "
      ++ "(Enter to confirm or Ctrl + c to cancel)"
  1 <$ getLine
readNatWithCap cap =
  untilJust $ do
    putStrLn $
      "Please enter a number between 1 and "
        ++ show cap
        ++ " (Ctrl + c to cancel):"
    parseNatWithCap cap <$> getLine

getTodayName :: App FilePath
getTodayName = liftIO $ fmap formatFileName Dates.getCurrentDateTime

-- Cmds ---

addCmd :: [FilePath] -> String -> App ()
addCmd todoFiles newTodo = do
  todayName <- getTodayName
  fileType <- readTodoFile $ todayFile todayName todoFiles
  file <- case addTodo newTodo <$> fileType of
    HasPrev (TodoFile _ todos _) -> pure $ TodoFile todayName todos []
    HasToday file -> pure file
    Error err -> liftIO $ Exit.die err
    NoFiles -> do
      liftIO $ putStrLn "Creating your first TODO file \\o/\n"
      pure $ TodoFile todayName [newTodo] []
  saveTodoFile file
  liftIO $ putStr $ formatFileWithIndex file

lastCmd :: [FilePath] -> App ()
lastCmd todoFiles = do
  todayName <- getTodayName
  eitherPrevFile <- readTodoFile $ prevFile todayName todoFiles
  prevF <- case eitherPrevFile of
    HasPrev file -> pure file
    HasToday _ -> liftIO $ Exit.die "No last file"
    NoFiles ->
      liftIO $ Exit.die $
        "You don't have any TODO files yet :)\n"
          ++ "Run the add or list commands to create your first file.\n"
          ++ "  $ td\n"
          ++ "  $ td add 'Start creating todos'"
    Error err -> liftIO $ Exit.die err
  case parseFileName $ path prevF of
    Right date -> liftIO $ putStrLn $ "td file: " ++ yyyyMmDd date ++ "\n"
    Left err -> liftIO $ printStderr err
  liftIO $ putStr $ formatFileWithIndex prevF

standupCmd :: [FilePath] -> App ()
standupCmd todoFiles = do
  todayName <- getTodayName
  mToday <- readTodoFile $ todayFile todayName todoFiles
  mPrev <- readTodoFile $ prevFile todayName todoFiles
  (prev, today) <- case (mPrev, mToday) of
    -- Yay we have both files!
    (HasPrev prev, HasToday today) -> pure (prev, today)
    -- No yesterday's file
    (HasToday _, HasToday today) -> pure (TodoFile "" [] [], today)
    -- No today's file
    (HasPrev prev, HasPrev _) -> do
      let (TodoFile _ prevTodos _) = prev
      let todayNew = TodoFile todayName prevTodos []
      saveTodoFile todayNew
      liftIO $ putStrLn "Created TODO file from yesterday's\n"
      pure (prev, todayNew)
    (NoFiles, _) ->
      liftIO $ Exit.die "No TODO files yet. Try:\n  $ td add 'Some stuff'"
    (_, NoFiles) ->
      liftIO $ Exit.die "No TODO files yet. Try:\n  $ td add 'Some stuff'"
    (Error err, _) -> liftIO $ Exit.die err
    (_, Error err) -> liftIO $ Exit.die err
    _ -> liftIO $ Exit.die "Something went wrong =/"
  liftIO $ putStr $ standupMsg prev today

createFromYesterday :: FilePath -> [String] -> App TodoFile
createFromYesterday todayName todos = do
  let todayNew = TodoFile todayName todos []
  saveTodoFile todayNew
  liftIO $ putStrLn "Created TODO file from yesterday's\n"
  liftIO $ putStr $ formatFileWithIndex todayNew
  pure todayNew

doneCmd :: [FilePath] -> (Int -> Maybe Int) -> App ()
doneCmd todoFiles getX = do
  todayName <- getTodayName
  mToday <- readTodoFile $ todayFile todayName todoFiles
  today <- case mToday of
    HasToday file -> pure file
    HasPrev (TodoFile _ todos _) -> createFromYesterday todayName todos
    Error err -> liftIO $ Exit.die err
    NoFiles -> do
      saveTodoFile $ TodoFile todayName [] []
      liftIO $ putStrLn "Created your first TODO file \\o/"
      liftIO $ putStrLn "Nothing to mark as done since you just started using TODO :)"
      liftIO Exit.exitSuccess
  let (TodoFile _ todo _) = today
  let cap = length todo
  when (cap == 0) $ do
    liftIO $ putStrLn "No TODOs for today :)"
    liftIO Exit.exitSuccess
  i' <- maybe (liftIO $ readNatWithCap cap) pure $ getX cap
  let todayNew = accomplish i' today
  saveTodoFile todayNew
  liftIO $ putStr $ formatFileWithIndex todayNew

listCmd :: [FilePath] -> App ()
listCmd todoFiles = do
  todayName <- getTodayName
  mToday <- readTodoFile $ todayFile todayName todoFiles
  case mToday of
    HasToday file -> liftIO $ putStr $ formatFileWithIndex file
    HasPrev (TodoFile _ todos _) -> void $ createFromYesterday todayName todos
    NoFiles -> do
      let file = TodoFile todayName [] []
      saveTodoFile file
      liftIO $ putStrLn "Created your first TODO file \\o/\n"
      liftIO $ putStr $ formatFileWithIndex file
    Error err -> liftIO $ Exit.die err

doOnboarding :: App ()
doOnboarding = do
  basePath <- R.asks getBasePath
  liftIO $ Dir.createDirectory basePath
  todayName <- getTodayName
  saveTodoFile $ TodoFile todayName [] []
  liftIO $ putStrLn "Welcome to TODO :)\n"
  liftIO $ putStrLn $ "Created TODO directory (" ++ basePath ++ ") ..."
  liftIO $ putStrLn "This is were your TODOs are stored\n"
  liftIO $ putStrLn "Also went ahead and created your first TODO file,"
  liftIO $ putStrLn "it's empty for now, try adding things to do:\n"
  liftIO $ putStrLn "  $ td add Get started with TODO"
  liftIO $ putStrLn "  $ td add Be Awesome\n"
  liftIO $ putStrLn "You can also check your TODOs for the day:\n"
  liftIO $ putStrLn "  $ td\n"
  liftIO $ putStrLn "And when you finish a task, don't forget to mark it as done:\n"
  liftIO $ putStrLn "  $ td done 2 # since you are already awesome ;)\n"
  liftIO $ putStrLn "For more check out the help:\n"
  liftIO $ putStrLn "  $ td help\n"
  liftIO $ putStrLn "That's all for now. Stay cool! Stay productive!"
  liftIO Exit.exitSuccess

help :: App ()
help = do
  liftIO $ putStrLn "TODO (td), a command line tool to handle your daily tasks"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "TODO is inspired by \"The Power of the TODO List\""
  liftIO $ putStrLn "(https://dev.to/jlhcoder/the-power-of-the-todo-list), a simple, yet powerful,"
  liftIO $ putStrLn "approach to keep track of your daily tasks."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "TODO saves your items (on ~/.todos) in a readable format that you can"
  liftIO $ putStrLn "edit/read/grep yourself. Make sure to checkout the article to learn more."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "TODO takes care of the files for your, so you don't have to. It creates a new"
  liftIO $ putStrLn "one every day when you run any of the commands. All the stuff you did the"
  liftIO $ putStrLn "previous day stays there and the pending items are copied to today (yup you"
  liftIO $ putStrLn "gotta finish what you started, eh?)"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "AUTHOR: gillchristian (https://gillchristian.xyz)"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "VERSION: 0.0.12"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "USAGE:"
  liftIO $ putStrLn "  $ td [command] [arguments]"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "COMMANDS:"
  liftIO $ putStrLn "  list:"
  liftIO $ putStrLn "    Show today's pending and done items."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td list"
  liftIO $ putStrLn "      $ td      # no command defualts to list"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  last:"
  liftIO $ putStrLn "    Show previous day pending and done items."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td last"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  add:"
  liftIO $ putStrLn "    Add a new pending item to today's list and show the updated list."
  liftIO $ putStrLn "    Use quotes when you want to use a symbol that isn't supported by the shell."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td add Do something awesome today"
  liftIO $ putStrLn "      $ td add 'Do stuff (not that stuff)'"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  done:"
  liftIO $ putStrLn "    Mark a pending item as done (Accomplished) and show the updated list."
  liftIO $ putStrLn "    If no number is provided you will be prompted to input one."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td done"
  liftIO $ putStrLn "      $ td done [x]"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  standup:"
  liftIO $ putStrLn "    List previous day done items and today's pending ones."
  liftIO $ putStrLn "    This serves as a report for your (you guessed it) standup."
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td standup"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  version:"
  liftIO $ putStrLn "    Show the version (just in case you'd like to know.)"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td version"
  liftIO $ putStrLn "      $ td --version"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "  help:"
  liftIO $ putStrLn "    Show this message. Duh!"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "    Usage:"
  liftIO $ putStrLn "      $ td help"
  liftIO $ putStrLn "      $ td --help"

shortHelp :: App ()
shortHelp = do
  liftIO $ putStrLn "TODO (td), a command line tool to handle your daily tasks"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "USAGE:"
  liftIO $ putStrLn "  $ td [command] [arguments]"
  liftIO $ putStrLn ""
  liftIO $ putStrLn "COMMANDS:"
  liftIO $ putStrLn "  list:"
  liftIO $ putStrLn "      $ td list"
  liftIO $ putStrLn "  last:"
  liftIO $ putStrLn "      $ td last"
  liftIO $ putStrLn "  add:"
  liftIO $ putStrLn "      $ td add Do something awesome today"
  liftIO $ putStrLn "      $ td add 'Do stuff (not that stuff)'"
  liftIO $ putStrLn "  done:"
  liftIO $ putStrLn "      $ td done [x]"
  liftIO $ putStrLn "  standup:"
  liftIO $ putStrLn "      $ td standup"
  liftIO $ putStrLn "  version:"
  liftIO $ putStrLn "      $ td version"
  liftIO $ putStrLn "  help:"
  liftIO $ putStrLn "      $ td help"

-- CLI ---

handleCommands :: [FilePath] -> [String] -> App ()
-- Adds a todo to today's file (creates it if it doesn't exist)
handleCommands todoFiles ["add"] = do
  liftIO $ putStrLn "What are going to do today?"
  addCmd todoFiles =<< liftIO getLine
handleCommands todoFiles ("add" : todo) =
  addCmd todoFiles $ unwords todo
-- Prints the last file (that is not today's one)
handleCommands todoFiles ("last" : _) = lastCmd todoFiles
-- Lists last's dones and today's todos
handleCommands todoFiles ("standup" : _) =
  standupCmd todoFiles
-- Set one of today's TODOs as Accomplished
handleCommands todoFiles ["done"] =
  doneCmd todoFiles $ const Nothing
handleCommands todoFiles ("done" : i : _) =
  doneCmd todoFiles $ \cap -> parseNatWithCap cap i
-- Show version
handleCommands _ ("version" : _) = liftIO $ putStrLn "v0.0.12"
handleCommands _ ("--version" : _) = liftIO $ putStrLn "v0.0.12"
-- Show help
handleCommands _ ("help" : _) = help
handleCommands _ ("--help" : _) = help
-- Lists today's file (creates it if it doesn't exist)
handleCommands todoFiles ("list" : _) = listCmd todoFiles
handleCommands todoFiles [] = listCmd todoFiles
handleCommands _ (cmd : _) = do
  liftIO $ putStrLnStderr $ "Unknown command '" ++ cmd ++ "'\n"
  shortHelp
  liftIO Exit.exitFailure

runCli :: IO ()
runCli = do
  basePath <- (</> ".todos") <$> Dir.getHomeDirectory
  hasBasePath <- Dir.doesDirectoryExist basePath
  let env = Env {getBasePath = basePath}
  unless hasBasePath $ R.runReaderT doOnboarding env
  todoFiles <- sortTodoFiles <$> Dir.listDirectory basePath
  args <- Env.getArgs
  R.runReaderT (handleCommands todoFiles args) env
