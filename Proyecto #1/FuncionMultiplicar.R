

FuncionMultiplicar<- function(matrizA,matrizB)

  {

   if(ncol(matrizA)==nrow(matrizB)){

    matrizC <- matrix(0,nrow(matrizA),ncol(matrizB))

    for(f in seq_len(nrow(matrizA)))
    {

       for(c in seq_len(ncol(matrizB)))
         {
           for(i in seq_len(nrow(matrizB)))
             {
             matrizC[f,c]=matrizC[f,c]+(matrizA[f,i]*matrizB[i,c])
             }
       }


    }




   }else{
     print("No se puede ejecutar la operación, porque las matrices no son del mismo tamaño, ERROR")
   }




return(matrizC)
  }
