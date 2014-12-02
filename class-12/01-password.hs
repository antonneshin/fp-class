{-
   МодифицируgetValidPassword
  :: (Monad (t1 m), MonadPlus (t (t1 m)), MonadTrans t,
      MonadTrans t1, MonadIO (t (t1 m)), MonadWriter [String] m) =>
     [[Char]] -> t (t1 m) String
йте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import System.Environment

import Data.Char

isValid :: String -> [[Char]] -> Bool
isValid s ogran = let lens = head ogran
	          in (if all isNumber lens then (length s >= (read lens )) else True)&&		 
                     ( not("isAlpha" `elem` ogran)||(any isAlpha s)) && 
                     ( not("isNumber" `elem` ogran)||(any isNumber s)) && 
                     ( not("isPunctuation" `elem` ogran)||(any isPunctuation s))

getValidPassword :: [String] -> MaybeT (ReaderT [String] (WriterT [String] IO)) String 
getValidPassword ogran= do
  liftIO $ putStrLn "Введите новый пароль:"
  s <- liftIO getLine
  tell [s]
  guard (isValid s ogran)
  return s
 
askPassword :: MaybeT (ReaderT [String] (WriterT [String] IO)) ()
askPassword = do
  ogran <- lift ask
  value <- msum $ repeat (getValidPassword ogran)
  liftIO $ putStrLn "Сохранение в базе данных..."

main = getArgs>>=(\args -> runWriterT (runReaderT (runMaybeT askPassword) args) ) >>= (\ (fl, ls) -> putStrLn ("log_of_try: "++show(ls)++"\n"++"Your password: "++show (last ls)))
