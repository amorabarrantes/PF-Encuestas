--Funcion para pedir inputs al usuario.
input ::  IO String
input  = do
    getLine


generarEncuestas :: [[String]] -> [[[String]]] -> IO()
generarEncuestas listaEncuestas listaPreguntasyRespuestas = do

    print("Quiere agregar otra Encuesta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int  

    if (variableCondicion /= 0)
        then do
            a <- agregarEncuestas listaEncuestas
            b <- agregarPreguntas listaPreguntasyRespuestas
            putStrLn $ ""
            print("Lista de encuestas")
            print(a)

            putStrLn $ ""
            print("Lista de preguntas y respuestas")
            print(b)

            generarEncuestas a b

        else print("Programa finalizado")

--Funcion para agregar preguntas.
--[["Encuesta1"],["Encuesta2"], ["etc"]]
agregarEncuestas :: [[String]] ->IO [[String]]
agregarEncuestas listaVacia = do
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
            pregunta <- input
            let a = pregunta
            let listaRespuestas = agregarRespuestas []
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





responderEncuestas :: [[String]]->[[String]] -> Int ->IO [[String]]
responderEncuestas listaPR listaRetorno contador = do
    
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
                    responderEncuestas (tail listaPR) listaRetornoAux (contador+1)
                else do
                    let listaResultados = head listaPR !! 0
                    let listaRetornoAux = (listaRetorno ++ [[listaResultados]])
                    responderEncuestas (tail listaPR) listaRetornoAux (contador)
            
       else return(listaRetorno)


main :: IO()
main = do
    generarEncuestas [] []



    
    