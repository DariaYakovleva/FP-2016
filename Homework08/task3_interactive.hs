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
import Control.Monad.Trans.State  (StateT (..), get, put, evalStateT, modify)


-- add property value
-- write
-- update property
type MMap = StateT (M.Map T.Text T.Text) IO (M.Map T.Text T.Text)

main :: IO ()
main = do
	putStrLn "HW7.1"
	config (return M.empty)

config :: MMap -> IO ()
config props = do 
		putStrLn "input property and value"
		command <- getLine
		let cmds = splitOn " " command in
			case head cmds of
				"write" -> write (T.pack "first.cfg") props
				"add"   -> add props (T.pack (cmds !! 1)) (T.pack (cmds !! 2))
				_       -> update props (T.pack (cmds !! 1))


write :: T.Text -> MMap -> IO ()
write fileName props = do
	handle <- openFile (T.unpack fileName) ReadMode
	contents <- hGetContents handle
	putStrLn $ "File before writing:\n" ++ contents
	hClose handle
	writeFile (T.unpack fileName) ""
	let values = map ((\[x, y] -> [T.pack x, T.pack y]) . splitOn "=") (lines contents)
	write' fileName values props	


write' :: T.Text -> [[T.Text]] -> MMap -> IO ()
write' fileName [] props = do
	kt <- evalStateT props M.empty
	Control.Monad.unless (null kt) $ do			
			let tmp = map (\(x, y) -> [x, y]) (M.toList kt)
			write' fileName tmp (return M.empty)

write' fileName (x:xs) props = do
	let [prop, val] = x
	myMap <- evalStateT props M.empty
	if M.member prop myMap
		then do
			appendFile (T.unpack fileName) (T.unpack prop ++ "=" ++ T.unpack (myMap M.! prop) ++ "\n")
			--modifyIORef props (M.delete prop)
			let mm = M.delete prop myMap			
			--put (M.delete prop mm)			
			write' fileName xs (return mm)
		else do
			appendFile (T.unpack fileName) (T.unpack prop ++ "=" ++ T.unpack val ++ "\n")
			write' fileName xs props


add :: MMap -> T.Text -> T.Text -> IO ()
add props a b = do
		putStrLn $ "property " ++ T.unpack a ++ " with value " ++ T.unpack b ++ " added"
		--print $ M.toList props'
		--modifyIORef props (M.insert a b)
		--put props (modifyIORef props (M.insert a b))
		myMap <- evalStateT props M.empty
		config (return (M.insert a b myMap))

update :: MMap -> T.Text -> IO ()
update props a = do
		--myMap <- readIORef props
		--putStrLn $ "input new value for '" ++ T.unpack a ++ "' property (previous: '" ++ show (M.lookup a myMap) ++ "')"
		b <- getLine
		--put props (a, T.pack b)
		--modifyIORef props (M.insert a (T.pack b))
		myMap <- evalStateT props M.empty
		putStrLn $ "property " ++ T.unpack a ++ " with value " ++ b ++ " updated"
		--print $ M.toList props'
		config (return (M.insert a (T.pack b) myMap))