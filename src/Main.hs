module Main where

import Control.Exception
import Control.Monad.Reader
import Data.List
import Network
import Prelude hiding (log)
import System.IO
import System.Exit
import Text.Printf

server = "irc.freenode.org"
port = 6667
chan = "#tutbot-testing"
nick = "tutbot"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where loop st = runReaderT run st
        disconnect = hClose . socket

connect :: IO Bot
connect = do
  handle <- connectTo server $ PortNumber (fromIntegral port)
  hSetBuffering handle NoBuffering
  return (Bot handle)

run :: Net ()
run = do
  perform $ Nick nick
  perform $ User (nick ++ " 0 * :tutorial bot")
  perform $ Join chan
  asks socket >>= listen

data IrcCommand =
  Nick String
  | User String
  | Join String
  | PrivateMessage String
  | Pong String
  | Log String
  | Quit
  | Pass
  deriving (Eq, Show)

toString :: IrcCommand -> String
toString (User u) = "USER " ++ u
toString (Join c) = "JOIN " ++ c
toString (Nick n) = "NICK " ++ n
toString (PrivateMessage m) = "PRIVMSG " ++ chan ++ " :" ++ m
toString (Pong m) = "PONG :" ++ m
toString (Log m) = "LOG: " ++ m
toString Quit = "QUIT :Exiting"
toString Pass = ""

perform :: IrcCommand -> Net ()
perform Quit = withWrite (toString Quit) $ liftIO (exitWith ExitSuccess)
perform Pass = return ()
perform c@(Log m) = log $ toString c
perform cmd = write $ toString cmd

listen :: Handle -> Net ()
listen handle = forever $
  eval <$> readMessage >>= perform
  where forever a = a >> forever a

readMessage :: Net IrcMessage
readMessage = do
  handle <- asks socket
  parseMessage <$> liftIO (hGetLine handle)

write :: String -> Net ()
write string = send string >> log string

send :: String -> Net ()
send string = do
  handle <- asks socket
  liftIO $ hPrintf handle "%s\r\n" string

log :: String -> Net ()
log string = liftIO $ printf "> %s\n" string

withWrite s action = write s >> action

data IrcMessage =
  Ping String
  | Command IrcCommand
  deriving (Eq, Show)

parseMessage :: String -> IrcMessage
parseMessage s =
  if isPing s
  then Ping (pingBody s)
  else Command (parseCommand $ clean s)
  where isPing = isPrefixOf "PING :"
        pingBody = drop 6
        clean = drop 1 . dropWhile (/= ':') . drop 1

parseCommand :: String -> IrcCommand
parseCommand s
  | "!id " `isPrefixOf` s = PrivateMessage (drop 4 s)
  | "!quit" `isPrefixOf` s = Quit
parseCommand s = Log s

eval :: IrcMessage -> IrcCommand
eval (Ping msg) = Pong msg
eval (Command cmd) = cmd
