--TASK
--implement Simple Moving Average algorithm using State monad.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

import Control.Monad.State

main :: IO()
main = do
	print $ "HW6.3"	
	print $ moving 4 [1, 5, 3, 8, 7, 9, 6] --[1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
	print $ moving 2 [1, 5, 3, 8, 7, 9, 6] --[1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]	
	--print $ take 5 $ moving 5 [0..]

--data State s a = State { runState :: s -> (a, s) }
--(>>=)  :: m a -> (a -> m b) -> m b  -- bind

moving :: Int -> [Double] -> [Double]
moving cnt lst = rec cnt (reverse lst) (step cnt [])

step :: Int -> [Double] -> State [Double] [Double]
step cnt [] = state $ \(x:xs) -> let arr = take cnt (x:xs) in ([sum arr / fromIntegral (min cnt (length arr))], xs)
step cnt ans = state $ \(x:xs) -> let arr = take cnt (x:xs) in ([sum arr / fromIntegral (min cnt (length arr))] ++ ans, xs)

rec :: Int -> [Double] -> State [Double] [Double] -> [Double]
rec cnt arr st = let (res, ost) = (runState st arr) in case ost of
				[]-> res
				arr2 -> rec cnt arr (st >>= (step cnt))
