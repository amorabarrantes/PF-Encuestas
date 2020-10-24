import Data.IORef

addEncuesta :: String -> [String]->  [String]
addEncuesta  x ref= [x] ++ ref

addPregunta :: IO [IORef [String]] -> IORef [String]->  IO[IORef [String]]
addPregunta  x ref= x ++ ref

makeTest :: IO [IORef [String]]
--Nombre encuesta, pregunta, respusta
makeTest = sequence [newIORef[], newIORef[], newIORef []]


main = do

  ref <- newIORef []
  
  encuesta <- makeTest

  modifyIORef (encuesta !! 0) (addEncuesta "encuesta1") -- Doesn't copy list
  modifyIORef (encuesta !! 0) (addEncuesta "encuesta2") -- Doesn't copy list
  modifyIORef (encuesta !! 0) (addEncuesta "encuesta3") -- Doesn't copy list
  modifyIORef (encuesta !! 0) (addEncuesta "encuesta4") -- Doesn't copy list

  modifyIORef ref (addPregunta encuesta)

  readIORef (encuesta !! 0) >>= print
