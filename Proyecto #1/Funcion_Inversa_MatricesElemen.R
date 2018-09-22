Funcion_Inversa_MatricesElemen<- function(matrizA)
{
  Matrices <- vector('list', 20)
  k <- 1
  matrixIn <- diag(nrow(matrizA))
  matrixInE <- diag(nrow(matrizA))
  if(nrow(matrizA)==ncol(matrizA)){
    if(Funcion_Det_OpElementalesE(matrizA)!=0) {


      #Crea las matriz identidad

      #Dirección de pivote
      dicPivote<-1

      f<-1
      c<-1
      i<-0
      j<-0

      aux<-0

      while(dicPivote<=ncol(matrizA))
      { #Se inicia el bucle

        #NOTA: SI EN LA POSICION [1,1] hay un 0, se tiene que hacer intercambio de fila.
        if(matrizA[dicPivote,dicPivote]==0)#verificar si el pivote es 0
        {#Si la MatrizA en la posicion 1,1 es igual a 0
          #Entonces i y j pasan a valer 1
          i<-dicPivote
          j<-dicPivote

          while(i<=nrow(matrizA)&&matrizA[i,j]==0)#busca el la misma columana del pivote, algun valor !=0
          {
            i<-i+1
            #El ciclo termina cuando encuentra en la columna un valor que no es igual a 0
          }

          if(matrizA[i,j]!=0)#si lo encuentra, hace el intercambio
          {

            while(j<=ncol(matrizA))
              # Mientras J sea menor o igual al numero de columnas de la MatrizA
            {
              aux <- matrizA[i,j] # El valor a intercambiar se almacena en la variable auxilar
              auxIn<-matrixIn[i,j] # Se hace con la matriz identidad
              matrizA[i,j]<-matrizA[f,c]# Se aplica la operación elemental de intercambio de filas
              matrixIn[i,j]<-matrixIn[f,c] # Se hace lo mismo con la matriz identidad
              matrizA[f,c]<-aux # Auxiliar asigna el valor intercambiado en la posición [f,c]
              matrixIn[f,c]<-auxIn #Se hace lo mismo con la matriz identidad
              j<-j+1
              c<-c+1
              #Aumentan los valores de las columnas
              #IMPORTANTE: El ciclo termina hasta cuando se haya realizado toda la operacion elemental
              #en cada columna
            }
            # Es fácil asignar nombres antes y después de crear una lista. En el primer
            # caso, al usar la función list podemos cambiar ligeramente la sintaxis para
            # asignarle los nombres 'jose' y 'carlos' a los elementos de x, de la
            # siguiente manera:

            Matrices[[k]] <- list(Intercambio_De_Filas = matrixIn)
            k <- k+1




          }

        }#Fin de intercambio de filas

        f <- dicPivote+1 #Se aumenta el valor de F

        while(f<=nrow(matrizA))
          #Mientras F sea menor o igual al numero de filas, entra al ciclo
        {

          if(matrizA[dicPivote,dicPivote]!= 1)#verifica si el pivote es un 1, si no, lo hace 1
          {
            c <- 1
            pivote<-matrizA[dicPivote,dicPivote] #Se le asigna el valor de la posicion determinada
            while(c<=ncol(matrizA))
              #Se recorre mientras la variable c sea menor o igual al numero de colmnas de la MatrizA
            {
              aux <- 1 #Se le asigna el valor de 1 a la variable aux
              valor <- matrizA[dicPivote,c] # A valor se le asigna lo que esta en la posicion determinada
              valorIn<- matrixIn[dicPivote,c] # Se le aplica lo anterior a la matriz

              # Una vez que se tienen los valores, se aplica la OPERACION ELEMENTAL de multiplicar
              # un escalar por una fila con el fin de hacerla valer 1 en la posicion del pivote
              matrizA[dicPivote,c] <-  (  aux / pivote  * valor )
              matrixIn[dicPivote,c]<-(aux/pivote*valorIn)
              c<-c+1
              #Aumenta c , porque todas las columnas tienen que ser recorridas para aplicarle
              # la operacion elemental.
            }
            Matrices[[k]] <- list(Multiplicacion_De_Escalar_ParaUno = matrixIn)
            k <- k+1
          }

          #*****************************************
          #Se le asigna el valor de 1 otra vez a C (se reinicia el contador)
          c <- 1
          q <- matrizA[f,dicPivote] #Se le asigna lo que tiene en la posicion f, dicPivote

          while(c<=ncol(matrizA))#convertir en cero los valores debajo del pivote
          {
            #Guarda el contenido de la posicion del pivote en la variable pivote.
            pivote<-matrizA[dicPivote,dicPivote] #[1,1] en la primera ronda
            pivoteIn  <- matrixIn[dicPivote,c] #Se repitel lo mismo en la Matriz Identidad

            aux<- matrizA[dicPivote,c] #Se asigna el valor de la posicion [dicPivote, c] en los auxiliares
            auxIn<- matrixIn[dicPivote,c]
            #Se aplica la operacion elemental de la suma de filas "n" veces
            matrizA[f,c] <- matrizA[f,c] - (q/pivote*aux)
            matrixIn[f,c] <- matrixIn[f,c] - (q/pivote*auxIn)
            c<-c+1
          }
          Matrices[[k]] <- list(SumaFilas_Nveces = matrixIn)
          k <- k+1
          f<-f+1 #Aumenta para cambiar de fila
        }

        dicPivote <- dicPivote+1
      } # FIN DE PRIMER BLOQUE

      #****************************************

      dicPivote<-ncol(matrizA) # Se le asigna el numero de columnas de la MatrizA
      while(dicPivote>0){
        #Mientras dicPivote sea mayor a 0

        f <- dicPivote-1
        while(f>0)
        {
          if(matrizA[dicPivote,dicPivote]!= 1)
            #Si en la posicion 2,2 es distinto de 1,
          {
            c<-1
            pivote<-matrizA[dicPivote,dicPivote]
            while(c<=ncol(matrizA))
            {
              aux<-1
              valor<- matrizA[dicPivote,c]
              valorIn<- matrixIn[dicPivote,c]
              #F1
              matrizA[dicPivote,c] <-  (  aux / pivote  * valor )
              matrixIn[dicPivote,c]<-(aux/pivote*valorIn)
              c<-c+1
            }
            Matrices[[k]] <- list(MatrizSup_Multiplicacion_De_Escalar_ParaUno = matrixIn)
            k <- k+1
          }

          #*****************************************
          c<-ncol(matrizA)
          q<-matrizA[f,dicPivote]

          while(c>0)
          {
            aux<- matrizA[dicPivote,c]
            auxIn<- matrixIn[dicPivote,c]

            matrizA[f,c] <- matrizA[f,c] - ( q* aux )
            matrixIn[f,c] <- matrixIn[f,c] - ( q * auxIn )
            c<-c-1
          }
          Matrices[[k]] <- list(MatrizSup_SumaFilas_Nveces = matrixIn)
          k <- k+1
          f<-f-1
        }

        dicPivote<-dicPivote-1
      }

    }else
    {
      print("El determinante de esta matriz es 0, por lo tanto no posee inversa")
    }

    View(matrizA)
    View(matrixIn)
    z <- 1
    w <- 0
    print(is.matrix(Matrices[[z]]))
    print(is.vector(Matrices[[z]]))
    print(is.list(Matrices))
    while (!is.null(Matrices[[z]])){
      print( Matrices[[z]])
      z <- z+1

    }
    posMatrizUltima <- z - 2
    print("Impresion en orden ascendente \n")
    while (posMatrizUltima>=1){
      print(Matrices[[posMatrizUltima]])
      print(posMatrizUltima)



      posMatrizUltima <- posMatrizUltima-1
    }
    print(str(Matrices))

    posMatrizUltima <- z - 2
     x_matrix <- matrix(as.vector(Matrices[[posMatrizUltima]]),nrow(matrixIn), ncol(matrixIn))
     print(x_matrix)
    matrizFinal <-matrix(0,ncol(matrixIn),nrow (matrixIn))
    matrizFinal <-  matrizFinal + x_matrix
    print("Esta es la matriz final")

    while(posMatrizUltima>=1){

      matrizFinal <-  matrizFinal%*% x_matrix(Matrices[[posMatrizUltima-1]],nrow(matrixIn),ncol(matrixIn))
      posMatrizUltima <- posMatrizUltima -1
    }
    print("Matriz inversa por matrices elementales")
    print(matrizFinal)


  }else
  {
    print("Esta matriz no es de orden n, por lo tanto no se puede ejecutar")
  }
}
