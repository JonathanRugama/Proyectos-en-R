
Funcion_Det_Recursivo<-function(matrizA)
{
  matrizB<-matrix(0,nrow(matrizA)-1,ncol(matrizA)-1)
  Determinante<-0
  Det<-1

  if(nrow(matrizA)==ncol(matrizA))
    {

      for(c in seq_len(ncol(matrizA)))#maneja la columna que tachamos.
      {
        f<-2
        while(f<=nrow(matrizA))#maneja las filas de la matriz A
        {
          j<-1
          a<-1
          while(j<=ncol(matrizA))#verifica los datos que se pueden pasar a la matriz n-1 y a la vez los pasa a la otra matriz
           {
            if(j!=c)
            {
             matrizB[f-1,a]<-matrizA[f,j]
             a<-a+1
            }
            j<-j+1
           }
          f<-f+1
        }

       Det<-Funcion_Det_OpElementalesE(matrizB)#calcula el determinante de la matriz n-1

       Determinante<-Determinante+((matrizA[1,c]*(-1)^(1+c))*Det)#lleva la cuenta o suma de los calculos de cada columan para luego sumarlos

      }

  }
return(Determinante)
}










