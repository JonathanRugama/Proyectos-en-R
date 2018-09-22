
#Se reciben dos matrices de igual tamaño
FuncionResta<- function(matrizA,matrizB)
{

     #Si el número de filas y columnas de la Matriz A es igual al número de filas y columnas
   # de la matriz B, entonces ingresa al IF
  if(nrow(matrizA)==nrow(matrizB)&&ncol(matrizA)==ncol(matrizB)){
    #Crea una matriz nula con el numero de filas de la matrizA y el numero de columnas
    #de la matrizB
    MatrizC <- matrix(data = 0,nrow(matrizA),ncol(matrizB))
    #Se recorren filas y columnas con una secuencia de 1 al numero de filas y columnas
    for(f in seq_len(nrow(matrizA))){
      for(c in seq_len(ncol(matrizA)))
      {
        #Se le asigna a la MatrizC en la posicion fc la resta de lo que posee la
        #matrizA en fc y la matriz b en fc
        MatrizC[f,c]=matrizA[f,c]-matrizB[f,c]
      }
    }
    print("Operacion exitosa")
    View(MatrizC,"Resultado")
  }else{
    print("Las matrices son de diferente dimension, por lo tanto, no es posible sumarlas")
  }
}
