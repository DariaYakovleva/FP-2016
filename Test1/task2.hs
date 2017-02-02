--максимально короткая реализация
--Prelude, Data.List, Data.Char, Data.Map, Data.Tree
--содержит 0..9 хотя бы раз
import Data.List


main = do
	print $ digits 12334567890
	print $ digits 11111111111
	print $ digits 0123456789
	print $ digits 1122334444556677880099


digits :: Int -> Bool
digits x = (length $ group $ sort $ show x)  >= 10