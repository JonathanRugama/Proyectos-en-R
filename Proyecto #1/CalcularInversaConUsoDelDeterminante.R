InversaConUsoDelDeterminante <- function(matriz){

  determinante <- Funcion_Det_OpElementalesE(matriz) #obtiene el determinante de la matriz original


  if(determinante == 0){  #si el determinante es igual a 0 no posee inversa

    print("No posee inversa")
  } else {

    matrizAdjunta <- matrix(1,nrow(matriz),ncol(matriz))   #matriz vacia para guardar la adjunta

    for(fila in seq_len(nrow(matriz))){      #recorre filas
      for(columna in seq_len(ncol(matriz))){  #recorre columnas
        exponente <- fila+columna             #obtiene la suma de la fila y columna para realizar la elevacion
        matrizAdjunta[fila,columna] <- matriz[fila,columna]*((-1)^exponente)  # obtiene la matriz adjunta

      }
    }

    matrizAdjuntaTranspuesta <- t(matrizAdjunta)   #transpone la matriz adjunta
    View(matrizAdjuntaTranspuesta)


    matrizInversa <- matrix(1,nrow(matrizAdjuntaTranspuesta),ncol(matrizAdjuntaTranspuesta))  #matriz vacia que va a guardar la matriz inversa
    variableDivididaEntreElDeterminante <- 1/determinante   # variable de 1 entre el determinante
    for(fila in seq_len(nrow(matrizAdjuntaTranspuesta))){    #recorre filas
      for(columna in seq_len(ncol(matrizAdjuntaTranspuesta))){  #recorre columnas
        #multiplica la variable por cada posicion de la matriz para obtener la inversa
        matrizInversa[fila,columna] <- variableDivididaEntreElDeterminante * matrizAdjuntaTranspuesta[fila,columna]

      }
    }

    return (matrizInversa)
  }

}
