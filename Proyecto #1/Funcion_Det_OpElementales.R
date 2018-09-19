Funcion_Det_OpElementalesE<- function(matrizA)
{

  if(nrow(matrizA)==ncol(matrizA))
  {
    t <- proc.time(0)
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
              aux=matrizA[i,j]
              matrizA[i,j] = matrizA[f,c]
              matrizA[f,c]=aux
              j <- j+1
              c <- c+1
            }
            signo <- signo*-1
          }
        }

      f <- dicPivote+1

      while(f <= nrow(matrizA))#empieza hacer las operaciones apartir de la columan del pivote a la derecha
      {
        c <- 1
        q <- matrizA[f,dicPivote]

        while(c <= ncol(matrizA))
          {
           pivote <- matrizA[dicPivote,dicPivote]
           aux <- matrizA[dicPivote,c]
           matrizA[f,c] <- matrizA[f,c] - ( q / pivote  * aux )# hacemos 0 la fila deseada
           c <- c+1
          }
       f <- f+1
      }


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




