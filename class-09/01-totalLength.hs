{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
import System.Environment

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength str = foldl ( \ sum a -> sum + (length a)) 0 str

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 _ 0 = Nothing
build1 el n = Just $ fst $ foldl (\ (ls, elem) a -> (ls ++ [take a $ repeat elem], elem)) ([], el) [1..n] 

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 el n
  | n==0 = Left "n=0" 
  | n>100 = Left "n > 100 " 
  | el=='x' = Left "can't create list with character 'x'..."
  | otherwise = Right (fst $ foldl (\ (ls, elem) a -> (ls ++ [take a $ repeat elem], elem)) ([], el) [1..n])
 
{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}


instance Functor (Either String) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x



main = do
  [name, ch, num] <- getArgs

  first <- fmap totalLength getArgs
  putStr "1) "
  print first 
   

  putStr "2) "
  second <- fmap (totalLength.lines) (readFile name)
  print second

  putStrLn "3) "
  putStr "    build1) "
  print $ fmap (totalLength) (build1 (head ch) (read num :: Int))

  putStr "    build2) "
  print $ fmap (totalLength) (build2 (head ch) (read num :: Int))


