

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
    {
     condicion<-ncol(matrizA)
    }else if(nrow(matrizA)<ncol(matrizA))
    {
     condicion<-nrow(matrizA)
    }else if(nrow(matrizA)==ncol(matrizA))
    {
     condicion<-ncol(matrizA)
    }



    while(dicPivote<=condicion)
    {
      if(matrizA[dicPivote,dicPivote]==0)#verificamos si en esa posicion hay un 0
      {
        i<-dicPivote
        j<-dicPivote

        while(i<=nrow(matrizA))
        {
          if(matrizA[i,j]!=0)
          {
           break()
          }
          i<-i+1 #buscamos las filas, donde la posicion en la misma columna del pivote, sea distinta de 0
        }

        if(matrizA[i,j]!=0)# si encuentra una != de 0, se intercambian filas
        {

          while(j<=ncol(matrizA))
          {
            aux=matrizA[i,j]
            matrizA[i,j]=matrizA[f,c]
            matrizA[f,c]=aux
            j<-j+1
            c<-c+1
          }

        }
      }

      f<-dicPivote+1

      while(f<=nrow(matrizA))#empieza hacer las operaciones apartir de la columan del pivote a la derecha
      {
        c<-dicPivote
        q<-matrizA[f,dicPivote]

        while(c<=ncol(matrizA))
        {
          pivote<-matrizA[dicPivote,dicPivote]
          aux<- matrizA[dicPivote,c]
          matrizA[f,c] <- matrizA[f,c] - ( q / pivote  * aux )# hacemos 0 la fila deseada
          c<-c+1
        }
        f<-f+1
      }


      dicPivote<-dicPivote+1
    }

    i<-1

    FilasNulas<-0
    Rango<-0

    #Sacar el rango de la matriz
    while(i<=nrow(matrizA))
    {
      j<-1
      CantidadCeros<-0
      while(j<=ncol(matrizA))
      {
       if(matrizA[i,j]==0)
       {
         CantidadCeros<-CantidadCeros+1
       }
        j<-j+1
      }

      if(CantidadCeros==ncol(matrizA))
      {
        FilasNulas<-FilasNulas+1
      }
      i<-i+1
    }
    Rango<-nrow(matrizA)-FilasNulas


    View(matrizA)
    print("Rango")
    return(Rango)
}
