

FuncionMultiplicar<- function(matrizA,matrizB)

  {
  #Si el numero de columnas de la matrizA es igual al numero de filas de la matrizB,
  #la operacion se puede realizar, de lo contrario  no se podra realizar

   if(ncol(matrizA)==nrow(matrizB)){
    #Crea una matriz nula con el numero de filas de matrizA y columnas de matrizB
    matrizC <- matrix(0,nrow(matrizA),ncol(matrizB))

    for(f in seq_len(nrow(matrizA)))
    {#Este for controla las posiciones del numero de filas de la matrizA

       for(c in seq_len(ncol(matrizB)))
         {#Este for controla las posiciones de los numeros de columna de la matrizB
           for(i in seq_len(nrow(matrizB)))
             {#Este for controla las posiciones de los numeros de fila de la matrizB
             matrizC[f,c]=matrizC[f,c]+(matrizA[f,i]*matrizB[i,c]) #Se le asigna la operacion a
             #la matrizC.
           }

         #NOTA: El tercer for es el que se ejecuta mas veces, debido a que tiene que recorrer
         #todas las posiciones de forma individual, es decir: si la matrizB es de 3x4, el for
         #se ejecuta 3 veces en cada columna, para un total de 12 posiciones.
       }


    }

   }else{
     print("No se puede ejecutar, las matrices poseen dimensiones diferentes, ERROR")
   }




return(matrizC)
  }
