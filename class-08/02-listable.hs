{-# LANGUAGE TypeSynonymInstances,FlexibleInstances#-}
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
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
  toList str = words str 
  
  fromList str = unwords str

instance Listable Integer where
  toList 0 = []
  toList a = toList (a `div` 10) ++ [a `mod` 10]
 
  fromList ls = fst $ foldl (\ (chislo, cnt) a -> (chislo+a*10^cnt, cnt-1 ) ) (0, (length ls)-1) ls



  
