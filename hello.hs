
listaEncuestas = ["Encuesta1", "Encuesta2"]



listaRespuestas = [ [ ["E1pregunta1Respuesta1","E1pregunta1Respuesta2"],["E1pregunta2Respuesta1","E1pregunta2Respuesta2"] ] , [["hola"],["hola2"]] ]

--Funcion para pedir inputs al usuario.
input ::  IO String
input  = do
    getLine



--Funcion para agregar preguntas.
--[["Encuesta1"],["Encuesta2"], ["etc"]]
agregarEncuestas :: [[String]] ->IO [[String]]
agregarEncuestas listaVacia = do
    pregunta <- input
    let lista = listaVacia ++ [[pregunta]]

    return(lista)



--Funcion para agregar preguntas
--[["Pregunta1", "Pregunta2"]]
agregarPreguntas :: [[String]] ->IO [[[String]]]
agregarPreguntas listaVacia = do
    retornoPreguntas <- agregarPreguntasAux listaVacia
    return([retornoPreguntas])

--Auxiliar de agregacion de preguntas.   
agregarPreguntasAux :: [[String]] ->IO [[String]]
agregarPreguntasAux listaP = do
    print("Quiere agregar otra pregunta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int  
    if (variableCondicion /= 0)
        then do
            pregunta <- input
            let a = pregunta
            let listaNueva = listaP ++ [[a]]

            let listaRespuestas = agregarRespuestas []
            x<-(listaRespuestas)
            let listaNueva2 = listaNueva ++ x

            agregarPreguntasAux listaNueva2
       else return(listaP)



--Funcion para agregar respuestas
--[ [ ["Respuesta1","Respuesta2"], ["Respuesta1","Respuesta2"] ] ]
--Dentro de los morados estan las respuestas para cada cuestionario.
agregarRespuestas :: [String] -> IO [[String]]
agregarRespuestas listaVacia = do
    print("Quiere agregar otra respuesta para la pregunta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int  

    if (variableCondicion /= 0)
    then do
            pregunta <- input
            let a = pregunta
            let listaNueva = listaVacia ++ [a]
            agregarRespuestas listaNueva
    else return([listaVacia])


indicesOf :: Eq a => a -> [a] -> [Int]
indicesOf a as = [i | (b, i) <- zip as [0..], b == a]

listaprueba = ["hola", "adios", "xd"]
listaPreguntas = [ [ ["pregunta1"],["respuesta1x", "respuestaN"], ["pregunta2"],["respuesta1", "respuesta2", "respuestaN"] ] ]

main :: IO()
main = do
    --(mapM_) print (listaPreguntas !!0)
    print(listaPreguntas!!0)
    print(indicesOf "respuesta1x" ((listaPreguntas!!0)!!1))

    --print(buscarIndice (listaPreguntas !! 0) 0)


    --print((listaRespuestas !! 0))
    --print(listaEncuestas !! 0)
    --print(listaPreguntas !! 0)
    --print(listaRespuestas !! 0)
    
    --a <- agregarEncuestas []
    --b <- agregarPreguntas []
    --c <- agregarRespuestas []

    --putStrLn $ ""
    --print("Lista de encuestas")
    --print(a)

    --putStrLn $ ""
    --print("Lista de preguntas")
    --print(b)

    --putStrLn $ ""
    --print("Lista de respuestas")
    --print(c)





    
    