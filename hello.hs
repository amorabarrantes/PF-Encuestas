
listaEncuestas = ["Encuesta1", "Encuesta2"]

listaPreguntas = [["pregunta1", "pregunta2", "pregunta3"],["xd"]]

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
agregarPreguntas :: [[String]] ->IO [[String]]
agregarPreguntas listaVacia = do
    retornoPreguntas <- agregarPreguntasAux listaVacia
    return(retornoPreguntas)

--Auxiliar de agregacion de preguntas.   
agregarPreguntasAux :: [[String]] ->IO [[String]]
agregarPreguntasAux listaP = do
    retornoAux2 <- agregarPreguntasAux2 []
    let a = listaP ++ retornoAux2
    return(a)

--Auxiliar para crear la lista con las preguntas.
agregarPreguntasAux2:: [String] -> IO [[String]]
agregarPreguntasAux2 listavacia = do
    print("Quiere agregar otra pregunta? (1 si, 0 no)")
    variableCondicionAux <- input
    let variableCondicion = read variableCondicionAux :: Int  
    if (variableCondicion /= 0)
        then do
            pregunta <- input
            let a = pregunta
            let listaNueva = listavacia ++ [a]
            agregarPreguntasAux2 listaNueva
       else return([listavacia])




--Funcion para agregar respuestas
--[ [ ["Respuesta1","Respuesta2"], ["Respuesta1","Respuesta2"] ] ]
--Dentro de los morados estan las respuestas para cada cuestionario.
agregarRespuestas :: [[String]] ->IO [[String]]
agregarRespuestas listaVacia = do
    pregunta <- input
    let lista2 = listaVacia ++ [[pregunta]]

    pregunta2 <- input
    let lista3 = lista2 ++ [[pregunta2]]
    return(lista3)



main :: IO()
main = do
    --(mapM_) print listaEncuestas
    --print((listaRespuestas !! 0))
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

    
    