--Funcion para pedir inputs al usuario.
input ::  IO String
input  = do
    getLine

--Genera encuestas, defecto parametros [] []

generarEncuestas :: [[String]] -> [[[String]]] -> IO()
generarEncuestas listaEncuestas listaPreguntasyRespuestas = do
    
    listaFinalEncuestas <- agregarEncuestas listaEncuestas
    listaFinalPreguntas <- agregarPreguntas listaPreguntasyRespuestas

    print("Quiere agregar otra Encuesta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int

    if (variableCondicion /= 0)
        then do
            generarEncuestas listaFinalEncuestas listaFinalPreguntas
        else print("Programa finalizado")

    putStrLn $ ""
    print("Lista de encuestas")
    print(listaFinalEncuestas)
    mapM_ (appendFile "encuestas.txt" . show) listaFinalEncuestas

    putStrLn $ ""
    print("Lista de preguntas y respuestas")
    print(listaFinalPreguntas)
    mapM_ (appendFile "preguntas.txt" . show) listaFinalPreguntas
    
    putStrLn $ ""
    print(listaFinalEncuestas)
    print("¿Digite el indice de la encuesta a responder?")
    indiceEncuesta <- input
    let indiceEncuestaInt = read indiceEncuesta :: Int

    listaConRespuestas <- generarResponder (listaFinalPreguntas !! indiceEncuestaInt) []
    putStrLn $ ""
    print("Lista con las respuestas")
    print(listaConRespuestas)

    putStrLn $ ""
    print("Estadisticas")

    putStrLn $ ""
    stat0 <- generarStat0 listaFinalEncuestas
    print("Cuantas encuestas se introdujeron al sistema")
    print(stat0)

    putStrLn $ ""
    stat1 <- generarStat1 listaConRespuestas
    print("Cuantas respuestas tuvo la encuesta")
    print(stat1)
    
    putStrLn $ ""
    stat2 <- generarStat2 listaConRespuestas
    print("Cuantas preguntas tiene la encuesta contestada")
    print(stat2)

--Cuantas encuestas se introdujeron al sistema
generarStat0:: [[String]] -> IO Int
generarStat0 respuestas0 = do
    let a = length(respuestas0)
    return(a)


--Cuantas veces se respondio la encuesta
generarStat1:: [[[String]]] -> IO Int
generarStat1 respuestas1 = do
    let a = length(respuestas1)
    return(a)

--Cuantas preguntas tiene una encuesta
generarStat2:: [[[String]]] -> IO Int
generarStat2 respuestas2 = do
    let a = length(respuestas2 !!0)
    let b = div a 2
    return(b)



generarResponder:: [[String]] -> [[[String]]] -> IO [[[String]]]
generarResponder listaPreguntasGR listaRetornoGR = do
    print("Quiere dar otra respuesta a la encuesta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int

    if (variableCondicion /= 0)
        then do
            respuestasGR <- responderEncuestas listaPreguntasGR listaRetornoGR 1
            generarResponder listaPreguntasGR respuestasGR

        else return (listaRetornoGR)


--Funcion para agregar preguntas.
--[["Encuesta1"],["Encuesta2"], ["etc"]]
agregarEncuestas :: [[String]] ->IO [[String]]
agregarEncuestas listaVacia = do
    print("Digite el nombre de la encuesta por crear")
    pregunta <- input
    let lista = listaVacia ++ [[pregunta]]

    return(lista)

--Funcion para agregar preguntas
--[["Pregunta1", "Pregunta2"]]
agregarPreguntas :: [[[String]]] ->IO [[[String]]]
agregarPreguntas listaPreguntas = do
    listaAConcatenar <- agregarPreguntasAux []
    let listaNueva = listaPreguntas ++ [listaAConcatenar]
    return (listaNueva)
    

agregarPreguntasAux :: [[String]] ->IO [[String]]
agregarPreguntasAux listaVacia = do

    print("Quiere agregar otra pregunta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int  

    if (variableCondicion /= 0)
        then do
            print("Que tipo de respuesta quiere que tenga la pregunta? (1 Escalar, 0 SeleccionUnica)")
            variableCondicionAux2 <- input
            let variableCondicion2 = read variableCondicionAux2 :: Int  
            if(variableCondicion2 == 0)
                then do
                    print("Digite el nombre de la pregunta")
                    pregunta <- input
                    let a = pregunta
                    let listaRespuestas = agregarRespuestas []
                    x<-(listaRespuestas)
                    let listaNueva = [[a]] ++ x
                    let listaNueva2 = listaVacia ++ listaNueva
                    agregarPreguntasAux listaNueva2
                else do
                    print("Digite el nombre de la pregunta")
                    pregunta <- input
                    let a = pregunta
                    let listaRespuestas = agregarRespuestasEscalar [] 0
                    x<-(listaRespuestas)
                    let listaNueva = [[a]] ++ x
                    let listaNueva2 = listaVacia ++ listaNueva
                    agregarPreguntasAux listaNueva2
       else return(listaVacia)


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

agregarRespuestasEscalar :: [String] -> Int -> IO [[String]]
agregarRespuestasEscalar listaVacia intRE = do
    if (intRE < 5)
    then do
            print("Digite el nombre para el indice")
            print(intRE+1)
            pregunta <- input
            let a = pregunta
            let listaNueva = listaVacia ++ [a]
            agregarRespuestasEscalar listaNueva (intRE+1)
    else return([listaVacia])

--Responde a encuestas, mandar Lista De Preguntas y respuestas de la encuesta -> Lista de retorno, vacia por defecto -> Contador 0 por defecto
responderEncuestas :: [[String]]->[[[String]]] -> Int ->IO [[[String]]]
responderEncuestas listaPR listaRetorno contador = do
    listaAContatenar <- responderEncuestasAux listaPR [] contador
    let listaNueva = listaRetorno ++ [listaAContatenar] 
    
    return (listaNueva)
    

responderEncuestasAux :: [[String]]->[[String]] -> Int ->IO [[String]]
responderEncuestasAux listaPR listaRetorno contador = do
    if (listaPR /= [])
        then do
            if (length(head listaPR) /= 1)
                then do
                    print("Digite el indice a responder para la pregunta "++ (show contador))
                    print(head listaPR)
                    indice <- input
                    let indiceInt = read indice :: Int  

                    let listaResultados = head listaPR !! indiceInt
                    let listaRetornoAux = (listaRetorno ++ [[listaResultados]])
                    responderEncuestasAux (tail listaPR) listaRetornoAux (contador+1)
                else do
                    print("Pregunta")
                    print(head listaPR)
                    let listaResultados = head listaPR !! 0
                    let listaRetornoAux = (listaRetorno ++ [[listaResultados]])
                    responderEncuestasAux (tail listaPR) listaRetornoAux (contador)
    else return(listaRetorno)


--[[["pregunta1"],["r1","r2","r3","r4","r5","r6"],["preguntaEscalar"],["poco","regular","mucho","muchisimo","un vergazo"]]]


main :: IO()
main = do
    generarEncuestas [] []
    --a <- agregarRespuestasEscalar [] 0
    --print(a)
