
FuncionPotencia<- function(matrizA,Potencia)

{
contador<-0
P<-Potencia

        if(ncol(matrizA)==nrow(matrizA))
{

          matrizC<-matrix(1,nrow(matrizA),ncol(matrizA))


         if(Potencia==2)#si la potencia ingresada es dos, se llama la funcion multiplicarrecursiva, para realizar la operacion
         {
          matrizC=FuncionMultiplicarRecursiva(matrizA,matrizA)

         }else# si no a la potencia le restamos dos valores, para llevar el conttrol excato de la multiplicacion
           {
           P<-P-2

           while(contador<=P)#cuando el contador es igual a P(potencia), se termina las multiplicaciones
             {
             if(contador==0)
             {
               matrizC<-FuncionMultiplicarRecursiva(matrizA,matrizA)
             }else
             {
              matrizC<-FuncionMultiplicarRecursiva(matrizC,matrizA)
             }
             contador<-contador+1
           }

           }

        View(matrizC,"Resultado de matriz elevada")#nos devuelve el resultado final

}else{
      print("No se puede ejecutar la operación, por diferencia en dimenciones, ERROR")
     }
}










FuncionMultiplicarRecursiva<- function(matrizA,matrizB)
{

  if(ncol(matrizA)==nrow(matrizB)){

    matrizC<-matrix(0,nrow(matrizA),ncol(matrizB))

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
    print("No se puede ejecutar la operación, por diferencia en dimenciones, ERROR")
  }
return(matrizC)
}
