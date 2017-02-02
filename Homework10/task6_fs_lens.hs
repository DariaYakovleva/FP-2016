{-# LANGUAGE RankNTypes #-}



main :: IO()
main = do
	putStrLn $ "HW10.6"

data FS = Dir  { name     :: FilePath  -- название папки, не полный путь
               , contents :: [FS]
               }
        | File { name     :: FilePath  -- название файла, не полный путь
               }
        deriving Show

--Создайте функцию, которая «сканирует» заданную директорию и 
--создаёт объект типа FS наподобие функции getDirectory' (или даже используя эту функцию).
--Создайте базовые производне линзы и призмы для этого типа.

getDir :: FilePath -> FS
getDir path = undefined