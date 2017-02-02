--Implement CPS-transformed view of operating system with small subset of system calls using Cont monad. 
--You can find guide and theory here.
--Support next system calls: read, write, exit, yield, fork.
--It should be possible to write such monadic code which will be interpreted by kernel:

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

import Control.Monad.Cont

main :: IO()
main = do
	print $ "HW6.4"	

--newtype Cont r a = Cont { runCont :: (a -> r) -> r }
{-
  Author: Jan Malakhovski
  Date: Mar 9, 2014

  Programming Languages Theory point of view on protected mode
  operating systems (with MMU) implemented in Haskell
  (based on implementation in Agda from 2011-2012)
-}
-- ignore this
instance Show Context where
  show ctx = (show $ tag ctx) ++ " " ++ (show $ args ctx) ++ " and some function"

-- Then
type Process r = SecureComputation r Context

data SyscallTag = Yield | SysFork | SysRead | SysWrite | SysExit deriving Show
type SyscallArgs = [String]
type SyscallResult = String

data Context = Context
               { tag :: SyscallTag
               , args :: SyscallArgs
               , cont :: Process SyscallResult
               }

main _ _ = Context
  { tag = SysRead
  , args = [ "fd1" ]
  , cont = afterRead }

afterRead _ s = Context
  { tag = SysWrite
  , args = [ "fd2", s ]
  , cont = afterWrite }

afterWrite _ _ = Context
  { tag = SysExit
  , args = [ "0" ]
  , cont = error "impossible!" }


main' _ _ = Context
  { tag = SysFork
  , args = []
  , cont = afterFork }

afterFork _ "" = Context
  { tag = SysExit
  , args = [ "1" ]
  , cont = error "impossible!" }
afterFork mmu _ = main mmu ""

-- Kernel
logging res = putStrLn $ "process asked " ++ show (tag res) ++ " " ++ show (args res)

kernel [] = return ()
kernel ((p, r):procs) = do
  putStr $ show (length procs + 1) ++ ": "
  let res = secureCall p undefined r
  logging res
  case tag res of
    Yield  -> do
              kernel (procs ++ [ (cont res, "") ])
    SysFork -> do
              kernel (procs ++ [ (cont res, "") , (cont res , "other") ])
    SysRead -> do
              kernel (procs ++ [ (cont res, "hello") ])
    SysWrite -> do
              kernel (procs ++ [ (cont res, "") ])
    SysExit -> do
              kernel procs

test = kernel [ (main, "") , (main, "") ]
test' = kernel [ (main', "") ]