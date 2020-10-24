
listaEncuestas = ["Encuesta1", "Encuesta2"]

listaPreguntas = [["pregunta1", "pregunta2", "pregunta3"],["xd"]]

listaRespuestas = [ [ ["E1pregunta1Respuesta1","E1pregunta1Respuesta2"],["E1pregunta2Respuesta1","E1pregunta2Respuesta2"] ] , [["hola"],["hola2"]] ]


input ::  IO String
input  = do
    getLine


append :: String -> [String] -> [String]
append new_element xs = xs ++ [new_element]

agregarSublista :: [String] -> [[String]] -> [[String]]
agregarSublista new_element xs = xs ++ [new_element]


--Funcion para agregar preguntas.
agregarPreguntas :: [[String]] ->IO [[String]]
agregarPreguntas listaParametro = do
    pregunta <- input
    let lista2 = listaParametro ++ [[pregunta]]

    pregunta2 <- input
    let lista3 = lista2 ++ [[pregunta2]]
    return(lista3)


--loop :: Int -> IO()
--loop a=
--    if (a) /= 0
--        then do
--            print("Quiere agregar otra encuesta?")
--            variableCondicionAux <- input
--            let variableCondicion = read variableCondicionAux :: Int         
--            loop variableCondicion
--        else putStrLn ""






main :: IO()
main = do
    --(mapM_) print listaEncuestas
    --print(append "hola" ((listaRespuestas !! 0)!! 0))
    --print(listaEncuestas !! 0)
    --print(listaPreguntas !! 0)
    --print(listaRespuestas !! 0)

    print("empezamos")
    a <- agregarPreguntas []
    print(a)
    print("hola")

    
    