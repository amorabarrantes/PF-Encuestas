
listaEncuestas = ["Encuesta1", "Encuesta2"]

listaPreguntas = [["pregunta1", "pregunta2", "pregunta3"],["xd"]]

listaRespuestas = [ [ ["E1pregunta1Respuesta1","E1pregunta1Respuesta2"],["E1pregunta2Respuesta1","E1pregunta2Respuesta2"] ] , [["hola"],["hola2"]] ]


input ::  IO String
input  = do
    getLine

--Funcion para agregar preguntas.
--[["Encuesta1"],["Encuesta2"], ["etc"]]
agregarEncuestas :: [[String]] ->IO [[String]]
agregarEncuestas listaVacia = do
    pregunta <- input
    let lista2 = listaVacia ++ [[pregunta]]

    pregunta2 <- input
    let lista3 = lista2 ++ [[pregunta2]]
    return(lista3)


--Funcion para agregar preguntas
--[["Pregunta1", "Pregunta2"]]
agregarPreguntas :: [[String]] ->IO [[String]]
agregarPreguntas listaVacia = do

    retornoPreguntas <- agregarPreguntasAux listaVacia
    return(retornoPreguntas)

--Auxiliar de agregacion de preguntas.   
agregarPreguntasAux :: [[String]] ->IO [[String]]
agregarPreguntasAux listaVacia = do
    
    pregunta1 <- input
    pregunta2 <- input


    let subLista = [pregunta1] ++ [pregunta2]

    let lista = listaVacia ++ [subLista]

    return(lista)









--Funcion para agregar respuestas
--[ [ ["Respuesta1","Respuesta2"], ["Respuesta1","Respuesta2"] ] ]
agregarRespuestas :: [[String]] ->IO [[String]]
agregarRespuestas listaVacia = do
    pregunta <- input
    let lista2 = listaVacia ++ [[pregunta]]

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

    a <- agregarEncuestas []
    b <- agregarPreguntas []
    --c <- agregarRespuestas []

    putStrLn $ ""

    print("Lista de encuestas")
    print(a)

    putStrLn $ ""

    print("Lista de preguntas")
    print(b)

    
    