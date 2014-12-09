{-# LANGUAGE TupleSections #-}
import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative

{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = do 
  first <- integer
  char '.'
  second <- natural
  return $ (read ((show first)++"."++(show second))::Float)

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = bracket "(" ")" $ do
  first <- token float
  string ", "
  second <- integer 
  return $ (first, fromIntegral second)


{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy (complex) (char ';')

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complex' :: Parser (Float, Float)
complex' = complex <|> fl
  where fl =(\s-> (s , 0.0) ) <$> float

complexList2 :: Parser [(Float, Float)]
complexList2 = bracket "[" "]" $ sepBy (complex') (char ';')

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined


