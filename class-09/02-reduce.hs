{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

import System.Environment
import System.Random


instance Functor (Either String) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x

instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))



{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a
  | (a `mod` 3)==0 = 0
  | ((a `mod` 3)/=0) && (odd a) = a^2
  | otherwise = a^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n fu = foldl (\ res a -> fmap reduce res ) (fu) [1..n]
{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

myDiv :: Integral a => (a, a) -> a
myDiv (a, b)
  | a>b = a `div` b
  | otherwise = b `div` a


toList :: Integral a => [(a, a)]  -> [a]
toList = foldl ( \ ls (a, b) -> ls++[myDiv (a,b)] ) []

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe list = if (length res)>0 then (Just (head res)) else Nothing 
  where
    res = toList list

toEither :: Integral a => [(a, a)]  -> Either String a
toEither list
  | len==0 = Left "List is empty!"
  | len>1000000 = Left "List is the biggest list in the world!"
  | otherwise = Right (head res)
  where
    res = toList list
    len = length res

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO ls = return $ head $ toList ls

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}


{-sozdanie текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами-}
createList :: (Enum b, Num b) => StdGen -> b -> [Int]
createList mk len = fst $ foldl (\ (ls, ogen) a -> let (num, ngen)=(randomR (1,100) ogen)::(Int, StdGen) in (ls++[num],ngen)) ([], mk) [1..len]

createFile fname mk len = writeFile fname $ fst $ foldl (\ (str, fl) a -> if fl==2 then (str++"\n"++show(a)++" ", 1) else (str++show(a),fl+1)  ) ([],0) (createList mk len)
{- //////////////////////////////////////////////////////////////////////////////// -}


parseArgs :: [String] -> (FilePath, Int)
parseArgs str = (head str, (read (last str)) :: Int)


readData :: FilePath -> IO [(Int, Int)]
readData fname = do
 cont <- readFile fname 
 return $ foldl (\ ls a -> let str = words a in (ls++[( (read (head str))::Int , (read (last str))::Int )]) ) [] (lines cont)



main = do
--создание файла
{-
  gen <- newStdGen
  [ff] <- getArgs
  print $ createList gen 100
  createFile ff gen 100
-}
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  
  putStrLn ("\nList from file: "++fname)
  print ps
  putStrLn " "
  putStrLn "reduceNF n (toList ps): "
  print $ reduceNF n (toList ps)
  putStrLn " "
  putStrLn "reduceNF n (toEither ps): "
  print $ reduceNF n (toEither ps)
  putStrLn " "
  putStrLn "reduceNF n (toIO ps): " 
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}

--первый файл
{-
32 41
22 6
27 25
76 11
33 1
26 79
2 53
32 35
81 68
21 32
91 85
39 96
1 28
99 6
69 50
13 84
94 96
21 77
49 31
99 78
56 96
80 76
71 95
45 45
5 77
76 53
62 64
22 41
3 65
76 71
23 21
63 31
32 66
52 21
91 81
59 72
21 73
12 52
3 17
93 84
91 62
49 59
13 39
52 77
30 10
23 87
50 91
12 11
68 92
9 57
-}

-- второй файл
{-

-}




