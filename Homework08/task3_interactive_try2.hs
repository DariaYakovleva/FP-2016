--TASK
--Implement interactive command line application 
--for creation and modification configuration files in very simple property format
--[propery]=[value]
--Property file example:
--host=github.com
--publicName=SuperWeb
--Modify your previous homework on IO (interactive command line configuration creator) with transformers.
--Think about better way of adding them into your code and what monads can you add

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import qualified Data.Map as M
import Data.List.Split
import System.IO
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans.State  (StateT (..), get, put, modify)
import Control.Monad.IO.Class


type MMap = StateT (M.Map T.Text T.Text) IO

main :: IO ()
main = do
	putStrLn "HW8.3"
	_ <- runStateT config (M.empty)
	return ()


config :: MMap ()
config = do
		liftIO $ putStrLn "input property and value"
		command <- liftIO $ getLine
		let cmds = splitOn " " command in
			case head cmds of
				"write" -> write (T.pack "first.cfg")
				"add"   -> add (T.pack (cmds !! 1)) (T.pack (cmds !! 2))
				_       -> update (T.pack (cmds !! 1))

write :: T.Text -> MMap ()
write fileName = do
	handle <- liftIO $ openFile (T.unpack fileName) ReadMode
	contents <- liftIO $ hGetContents handle
	liftIO $ putStrLn $ "File before writing:\n" ++ contents
	liftIO $ hClose handle	
	liftIO $ writeFile (T.unpack fileName) ""
	let values = map ((\[x, y] -> [T.pack x, T.pack y]) . splitOn "=") (lines contents)
	write' fileName values


write' :: T.Text -> [[T.Text]] -> MMap ()
write' fileName [] = do
	myMap <- get
	Control.Monad.unless (null myMap) $ do  
			propsMap <- get
			let tmp = map (\(x, y) -> [x, y]) (M.toList propsMap)
			put M.empty
			write' fileName tmp

write' fileName (x:xs) = do
	let [prop, val] = x
	myMap <- get
	if M.member prop myMap
		then do
			liftIO $ appendFile (T.unpack fileName) (T.unpack prop ++ "=" ++ T.unpack (myMap M.! prop) ++ "\n")
			modify (M.delete prop)
			write' fileName xs
		else do
			liftIO $ appendFile (T.unpack fileName) (T.unpack prop ++ "=" ++ T.unpack val ++ "\n")
			write' fileName xs


add :: T.Text -> T.Text -> MMap ()
add a b = do
		liftIO $ putStrLn $ "property " ++ T.unpack a ++ " with value " ++ T.unpack b ++ " added"				
		modify (M.insert a b)
		config

update :: T.Text -> MMap ()
update a = do
		myMap <- get
		liftIO $ putStrLn $ "input new value for '" ++ T.unpack a ++ "' property (previous: '" ++ show (M.lookup a myMap) ++ "')"
		b <- liftIO $ getLine		
		modify (M.insert a (T.pack b))
		liftIO $ putStrLn $ "property " ++ T.unpack a ++ " with value " ++ b ++ " updated"		
		config