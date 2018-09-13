
matriz = function(filas, columnas) {


  
  am <- array(c(1,2,3,4,5,6,7,8,10,11,12),dim=c(filas,columnas ))
  return (print(am))                                                          
  
  
}
matriz(3,6)


#Llenar un vector i del 1 al 4
for (i in 1:5) {
  if (i==5){
    break
  }
  print (i)
}

sample(1:30,10,replace = F)

m1 <- matrix(sample(1:30,10,replace = F),filas,columnas)