

FuncionSuma<- function(matrizA,matrizB)
{


  if(nrow(matrizA)==nrow(matrizB)&&ncol(matrizA)==ncol(matrizB)){

     MatrizC <- matrix(data = 0,nrow(matrizA),ncol(matrizB))

    for(f in seq_len(nrow(matrizA))){
      for(c in seq_len(ncol(matrizA)))
      {
        MatrizC[f,c]=matrizA[f,c]+matrizB[f,c]
      }
    }
    print("Operación realizada con éxito")
    View(MatrizC,"Resultado")
  }else{
    print("Las matrices son de diferente dimención, por lo tanto no se pueden sumar")
  }
}
