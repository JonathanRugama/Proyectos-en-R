

Funcion_Rango<-function(matrizA)
{
   #Se uso el metodo de Eliminación de Gauss
    dicPivote<-1

    f<-1
    c<-1
    i<-0
    j<-0
    aux<-0

    condicion<-0
    if(nrow(matrizA)>ncol(matrizA))
    {#Si el numero de filas de la matrizA es mayor a su numero de columnas
     condicion<-ncol(matrizA) #Se le asigna el numerode columnas a condicion
    }else if(nrow(matrizA)<ncol(matrizA))
    {#De lo contrario, si el numero de filas de la matriz es menor a su numero de columnas
     condicion<-nrow(matrizA)#Se le asgina a condicion su numero de filas
    }else if(nrow(matrizA)==ncol(matrizA))
    {#De lo contrario, si el numero de filas de la matrizA es igual a su numero de columnas
     condicion<-ncol(matrizA) #Entonces el numero de columnas de la matrizA se le asigna a condicion
    }


 #dicPivote = direccion de pivote
    while(dicPivote<=condicion)
    {#Mientras dicPivote sea menor o igual a condicion
      if(matrizA[dicPivote,dicPivote]==0)#Se verifica si en esa posicion hay un 0
      {
        #Se la asigna el valor de dicPivote a  i y j
        i<-dicPivote
        j<-dicPivote

        while(i<=nrow(matrizA))
        {#Mientras el numero de filas de la matrizA sea mayor o igual a i
          if(matrizA[i,j]!=0) #Verifica si esa posicion es distinta de 0
          {#buscamos las filas, donde la posicion en la misma columna del pivote,
            #sea distinta de 0
           break()
          }
          i<-i+1 #
        }

        if(matrizA[i,j]!=0)# si encuentra una != de 0, se intercambian filas
        {

          while(j<=ncol(matrizA))
          {#Mientras j sea menor o igual al numero de columnas de la matrizA
            #Se hace el intercambio de filas.
            aux=matrizA[i,j]
            matrizA[i,j]=matrizA[f,c]
            matrizA[f,c]=aux
            j<-j+1
            c<-c+1
          }

        }
      }

      f<-dicPivote+1 #Se le asigna a f la suma de dicPivote1+1

      while(f<=nrow(matrizA))#empieza hacer las operaciones apartir de la columan del pivote a la derecha
      {
        c<-dicPivote #Se le asigna a c el valor de dicPivote
        #Supongamos que en la primera iteracion f tiene un valor de 2 y dicPivote sigue
        #teniendo un valor de 1
        q<-matrizA[f,dicPivote] #Entonces se tomara el valor de la posicion [2,1]
                                #y le será asignado a q

        while(c<=ncol(matrizA))
        {#Mientras c sea menor o igual al nmero de columnas de la matrizA
          pivote<-matrizA[dicPivote,dicPivote] #Se asigna en la variable pivote el pivote ubicado
                                                #en esa posicion de [dicPivote, dicPivote]
          aux<- matrizA[dicPivote,c]  #El numero de columnas va a variar, por lo tanto
           #se le asigna a aux el valor contenido en la posicion [dicPivote,c],
          #y cada vez que c aumente en 1, el valor de aux va a variar.
          matrizA[f,c] <- matrizA[f,c] - ( q / pivote  * aux )# hacemos 0 la fila deseada
          c<-c+1 #c aumenta en 1
        }
        f<-f+1 #f aumenta en 1
      }

      #La direccion de pivote aumenta en 1
      dicPivote<-dicPivote+1
    }

    i<-1

    FilasNulas<-0 #Se le asigna el valor de 0
    Rango<-0 #Se le asigna el valor de 0

    #Sacar el rango de la matriz
    while(i<=nrow(matrizA))
    { # Mientras i sea menor o igual al numero de filas de la MatrizA
      j <- 1 #se le asigna a j el valor de 1
      CantidadCeros <- 0  #Se le asigna a CantidadCeros el valor de 0
      while(j<=ncol(matrizA))
      { #Mientras j sea menor o igual al numero de columnas de la MatrizA
       if(matrizA[i,j]==0) #Verifica que esa posicion sea igual a 0
       {
         #Si lo es, la CantidadCeros aumenta en 1
         CantidadCeros<-CantidadCeros+1
       }
        j<-j+1 #j aumenta en 1 en cada ciclo del while(j<=ncol(matrizA))
      }

      if(CantidadCeros==ncol(matrizA)){ #Verifica si la cantidadCeros es igual al numero de
        #columnas de la matrizA
        FilasNulas<-FilasNulas+1 #Las filas aumentan en cada ciclo del while si la
        #cantidadCeros es igual al numero de columnas.
      }
      i<-i+1  #i aumenta en cada ciclo del while(i<=nrow(matrizA))
    }
    Rango<-nrow(matrizA)-FilasNulas #El rango es el numero de filas de la MatrizA
    #menos la cantidad de filas nulas encontradas.


    View(matrizA)
    print("Rango")
    return(Rango)
}
