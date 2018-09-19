Funcion_Inversa_MatricesElementalesE<- function(matrizA)
{

  if(nrow(matrizA)==ncol(matrizA)){
    if(Funcion_Det_OpElementalesE(matrizA)!=0) {



    }else{
      print("El determinante de esta matriz es 0, por lo tanto no posee inversa")
    }
    }
}
