-- CHECK

-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-tabs #-}

{-# LANGUAGE TemplateHaskell #-}
import FstN (fstN)
import SelN (selN)
import CompileTimeIO (getVar)
import ShowMustGoOn
import Fibonacci

listFields ''MyData

main :: IO()
main = do
	putStrLn $ "HW10"
	-- TASK1
	print $ $(fstN 4) ("hello world", 1, 2, 3)
	print $ $(selN 4 1) ("hello", 1, [4,3], 2) -- "hello"
	print $ $(selN 4 3) ("hello", 1, [4,3], 2) -- [4,3]
	-- TASK2
	print $ $(getVar)
	-- TASK3	
	print $ MyData { foo = "bar", bar = 5 }
	print $ $(getFib 4)
