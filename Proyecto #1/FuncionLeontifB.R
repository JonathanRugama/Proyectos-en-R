FuncionLeontifB <- function(Matriz1, Matriz2){

  f <- 1
  c <- 1
  entrada <- 0
  #creacion de las respectivas matrices
  MatrizInsumoProducto <- matrix(0, nrow(Matriz1), ncol(Matriz1))
  MatrizProduccion <- matrix(0, nrow(Matriz1), ncol(Matriz1))
  MatrizB <- matrix(0, nrow(Matriz1), ncol(Matriz1))
  MatrizIn <- diag(nrow(Matriz1))


  for(i in seq_len(nrow(Matriz1))){

    for(j in seq_len(ncol(Matriz1))){

      entrada <-  Matriz1[i,j] + Matriz1[i,j] + Matriz2[i,1] #a la variable entrada se le asigna la suma de si misma y lo que está en la posicion ij

    }
    MatrizInsumoProducto[f,c] = Matriz1[f,c]/entrada#A la matriz A le asigna lo que tiene la matriz1 en la posicion fc dividido entre la entrada
    entrada <- 0#regresa el contador a 0

    #aumentan los contadores
    f <- f+1
    c <- c+1

  }

  View(MatrizInsumoProducto)
  MatrizB <- MatrizIn - MatrizInsumoProducto#a la matriz B aplaca la resta de la identidad menos la matriz insumo
  View(Matriz2)#MATRIZ DEMANDA
  MatrizProduccion <- solve(MatrizB) %*% Matriz2#aplica la multiplicacion entre la inversa y la matriz demanda
  #imprime y muesta la matriz resultante de las operaciones
  View(MatrizProduccion )
}#fin de la funcion
