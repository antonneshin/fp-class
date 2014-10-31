{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}
import Data.String

class Listable a where
  toList :: a -> [a]
  fromList :: [a] -> a

{-instance Listable Int where
  toList a = [a]
  
  fromList [] = error "List is empty!"
  fromList (x:_) = x
-}

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integral a - любое целое число разбивается на список цифр.
-}

instance Listable String where
  (toList a) :: String = (words a) :: String
  --toList str = fst $ foldl (\ (list, word) a -> if a==' ' then (list++[word],[]) else (list, word ++ [a])) ([],[]) (str++" ")
