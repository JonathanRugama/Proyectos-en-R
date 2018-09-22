Funcion_Det_OpElementalesE<- function(matrizA)
{

  if(nrow(matrizA)==ncol(matrizA))
  {
    t <- proc.time()
    dicPivote <- 1

    f <- 1
    c <- 1
    i <- 0
    j <- 0
    signo <- 1

    aux <- 0

      while(dicPivote <= ncol(matrizA))
      {
        if(matrizA[dicPivote,dicPivote]==0)#verificamos si en esa posicion hay un 0
        {
          #Guardamos esa posicion en [i, j] , la cual seria [1,1]
          i <- dicPivote
          j <- dicPivote

          while(i<=nrow(matrizA)&&matrizA[i,j]==0)
            #mientras i sea menor o igual al npumero de filas de la MatrizA y la matrizA en la posicion
            # i, j (1,1) sea igual a 0, el valor de i va a aumentar en 1
          {
            i <- i+1 #buscamos las filas, donde la posicion en la misma columna del pivote, sea distinta de 0
          }

          if(matrizA[i,j]!=0)# si encuentra una != de 0, se intercambian filas
          {

            while(j <= ncol(matrizA)) # Este while hace el intercambio de filas
            {
              aux <- matrizA[i,j] #El vlor de la posicion [i,j] se le asigna aux para guardarlo
              matrizA[i,j] <- matrizA[f,c] #Se cambian los valores
              matrizA[f,c] <- aux  #El valor guardado anteriormente se le asigna a la posicion [f,c]
              j <- j+1
              c <- c+1
              #Aumentan los valores de las columnas (j y c), porque la operación se realiza en cada
              #columna.
            }
            signo <- signo*-1
          }
        }

      f <- dicPivote+1

      while(f <= nrow(matrizA))#empieza hacer las operaciones apartir de la columan del pivote a la derecha
      {
        c <- 1
        q <- matrizA[f,dicPivote] #Se le asigna a q el valor de la posicion [f, dicPivote]

        while(c <= ncol(matrizA))
          {#Mientras c sea menor o igual al numero de columnas de la matrizA

           pivote <- matrizA[dicPivote,dicPivote] #Se asigna el valor almacenado en la posicion pivote (diagonal)
           aux <- matrizA[dicPivote,c] #Se le asigna a aux la el valor almecenado en la posicion
                                      #[dicPivote, c]
           matrizA[f,c] <- matrizA[f,c] - ( q / pivote  * aux )# hacemos 0 la fila deseada
           c <- c+1 #Se aumenta la columna en 1, en cada ciclo de  while(c <= ncol(matrizA))
          }
       f <- f+1 #Se le asigna a f el valor aumentado de f, en cada ciclo completado
       #de  while(f <= nrow(matrizA))
      }

      #Cada vez que termina un ciclo del  while(f <= nrow(matrizA)), dicpovote aumenta en 1
      dicPivote <- dicPivote+1
      }

    x <- 1
    Determinante <- 1
    while(x <= ncol(matrizA))
    {
     Determinante<-Determinante*matrizA[x,x]
     x <- x+1
    }
View(matrizA)
    Determinante<-Determinante*signo


    print(t)
    return(Determinante)
  }



  }




