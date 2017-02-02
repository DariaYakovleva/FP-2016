-- # LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances, RankNTypes #
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes #-}

module ShowMustGoOn where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List

--ghci> print $ MyData { foo = "bar", bar = 5 }
--MyData { 
--    foo = "bar",
--    bar = 5
--}

listFields :: Name -> Q [Dec]
listFields name = do
    TyConI (DataD _ className _ [RecC _ fields] _) <- reify name
    let names = map (\(name,_,_) -> name) fields
    let showField :: Name -> Q Exp
        showField name = [|\x -> s ++ " = " ++ show ($(varE name) x)|] 
          where s = nameBase name
    let showN :: Q Exp
        showN = [| s |] 
          where s = nameBase className
    let showFields :: Q Exp
        showFields = listE $ map showField names
    [d|instance Show $(conT name) where
          show x =  $(showN) ++ " {\n    " ++ intercalate ",\n    " (map ($ x) $showFields) ++ "\n}"|]


data MyData = MyData
	{ foo :: String
	, bar :: Int
	}