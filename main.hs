-- main
import System.IO
import System.Process
import Fancyput
-- Configuration
-- e.g. (1, 2, 3, "4")
data Cfig = Cfig Int Int Int String deriving (Eq, Ord, Show)

main :: IO ()
main = do
 putStrLn "Mathcon Turning On..."
 putStrLn ""
 license
 putStrLn ""
 -- c1 <- config1 (Read file)
 -- c2 <- config2 (,,)
 -- c3 <- config3 (,,)
 -- c4 <- config3 (,,)
 welcome (Cfig 1 2 3 "4") -- (Cfig c1 c2 c3)

welcome :: Cfig -> IO ()
welcome cfig = do
 putStrLn ""
 mc "Main page"
 putStrLn "------------------"
 putStrLn "Now Setting"
 print cfig
 putStrLn "------------------"
 w <- putcon
 let cmd = (head . words) w
 case (cmd) of
  "end"     -> shutdown
  "setting" -> config cfig w >>= welcome
  --"help"    -> fc id c fhelp
  --"wf"      -> fc id c $ fwf c w
  --"pf"      -> fc id c $ fpf c w
  --"vbpf"    -> fc id c $ fvbpf c w
  otherwise -> fc cfig fexception
-- fc for function call
fc :: Cfig -> IO () -> IO ()
fc cfig = (>> welcome cfig)
-- function config
config :: Cfig -> String -> IO Cfig
config cfig w = do
 let ww = words w
 if (length ww == 1)
  then do
   putStrLn ""
   mc "Setting"
   putStrLn "------------------"
   putStrLn "Now Setting"
   print cfig
   --putStrLn "If you choose invalid setting, the default cft(cft1) will be used."
   --cft <- putconch "cft> "
   return cfig
  else return cfig
-- function shutdown
shutdown :: IO()
shutdown = do
 putStrLn ""
 mc "Shutdown"
 putStrLn "Good bye."
 putStrLn "Mathverse Shutting down..."
 putStrLn ""
-- function exception
fexception :: IO()
fexception = do
 putStrLn ""
 mc "Exception"
 putStrLn "Not yet implemented or not a valid function"
 putStrLn "Choose other options..."
-- the function which shows the license
license :: IO ()
license = do
 putStrLn "To see the copyright of this code itself and"
 putStrLn "the license of packages that were used to build this code,"
 putStrLn "type 'y' or 'Y'. To skip it, type anything else."
 y <- getLine
 if (y == "y" || y == "Y")
  then do
   contents <- readFile "license"
   putStr contents
  else return ()
-- [Mathcon] Logo
mc :: String -> IO ()
mc = putStrLn . ("[Mathcon] : "++)
