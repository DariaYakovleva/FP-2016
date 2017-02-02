--TASK
--Implement interactive command line application 
--for creation and modification configuration files in very simple property format
--[propery]=[value]
--Property file example:
--host=github.com
--publicName=SuperWeb
--dont use String

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import qualified Data.Map as M
import Data.List.Split
import System.IO
import qualified Data.Text as T
import Data.IORef
import Control.Monad

-- add property value
-- write
-- update property

main :: IO ()
main = do
	putStrLn "HW7.1"
	mymap <- newIORef M.empty
	config mymap


config :: IORef (M.Map T.Text T.Text) -> IO ()
config props = do 
		putStrLn "input property and value"
		command <- getLine
		let cmds = splitOn " " command in
			case head cmds of
				"write" -> write (T.pack "first.cfg") props
				"add"   -> add props (T.pack (cmds !! 1)) (T.pack (cmds !! 2))
				_       -> update props (T.pack (cmds !! 1))

write :: T.Text -> IORef (M.Map T.Text T.Text) -> IO ()
write fileName props = do
	handle <- openFile (T.unpack fileName) ReadMode
	contents <- hGetContents handle
	putStrLn $ "File before writing:\n" ++ contents
	hClose handle
	writeFile (T.unpack fileName) ""
	let values = map ((\[x, y] -> [T.pack x, T.pack y]) . splitOn "=") (lines contents)
	write' fileName values props	


write' :: T.Text -> [[T.Text]] -> IORef (M.Map T.Text T.Text) -> IO()
write' fileName [] props = do
	myMap <- readIORef props
	Control.Monad.unless (null myMap) $ do  
			propsMap <- readIORef props
			let tmp = map (\(x, y) -> [x, y]) (M.toList propsMap)	
			mm <- newIORef M.empty
			write' fileName tmp mm

write' fileName (x:xs) props = do
	let [prop, val] = x
	myMap <- readIORef props
	if M.member prop myMap
		then do
			appendFile (T.unpack fileName) (T.unpack prop ++ "=" ++ T.unpack (myMap M.! prop) ++ "\n")
			modifyIORef props (M.delete prop)
			write' fileName xs props
		else do
			appendFile (T.unpack fileName) (T.unpack prop ++ "=" ++ T.unpack val ++ "\n")
			write' fileName xs props



add :: IORef (M.Map T.Text T.Text) -> T.Text -> T.Text -> IO ()
add props a b = do
		putStrLn $ "property " ++ T.unpack a ++ " with value " ++ T.unpack b ++ " added"
		--print $ M.toList props'
		modifyIORef props (M.insert a b)
		config props

update :: IORef (M.Map T.Text T.Text) -> T.Text -> IO ()
update props a = do
		myMap <- readIORef props
		putStrLn $ "input new value for '" ++ T.unpack a ++ "' property (previous: '" ++ show (M.lookup a myMap) ++ "')"
		b <- getLine
		modifyIORef props (M.insert a (T.pack b))
		putStrLn $ "property " ++ T.unpack a ++ " with value " ++ b ++ " updated"
		--print $ M.toList props'
		config props