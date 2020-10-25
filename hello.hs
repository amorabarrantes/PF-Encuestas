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





responderEncuestas :: [[String]]->[[String]] ->IO [[String]]
responderEncuestas listaPR listaRetorno  = do
    
    if (listaPR /= [])
        then do
            if (length(head listaPR) /= 1)
                then do
                    print("Digite el indice a responder para la pregunta")
                    print(head listaPR)
                    indice <- input
                    let indiceInt = read indice :: Int  

                    let listaResultados = head listaPR !! indiceInt
                    let listaRetornoAux = (listaRetorno ++ [[listaResultados]])
                    responderEncuestas (tail listaPR) listaRetornoAux
                else do
                    let listaResultados = head listaPR !! 0
                    let listaRetornoAux = (listaRetorno ++ [[listaResultados]])
                    responderEncuestas (tail listaPR) listaRetornoAux
            
       else return(listaRetorno)



indicesOf :: Eq a => a -> [a] -> [Int]
indicesOf a as = [i | (b, i) <- zip as [0..], b == a]


listaprueba = ["hola", "adios", "xd"]
listaPreguntas = [ [ ["pregunta1"],["respuesta1x", "respuestaN"], ["pregunta2"],["respuesta1", "respuesta2", "respuestaN"] ] ]

main :: IO()
main = do
    respuestas <- responderEncuestas (listaPreguntas!!0) []
    print(respuestas)
    --print(listaPreguntas!!0)
    --print(indicesOf "respuesta1x" ((listaPreguntas!!0)!!1))

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





    
    