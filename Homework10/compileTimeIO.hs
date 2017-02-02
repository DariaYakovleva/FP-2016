{-# LANGUAGE TemplateHaskell #-}

module CompileTimeIO where
import Language.Haskell.TH
import System.Environment

getVar :: Q Exp
getVar = do
	value <- runIO $ getEnv "TH_ENV"
	return $ LitE (StringL value)