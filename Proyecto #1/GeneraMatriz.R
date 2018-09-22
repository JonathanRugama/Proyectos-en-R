
GenerarMatriz<- function(m,n){
#Se crea una matriz con valores aleatorios de -99 a 99, los cuales se pueden repetir.
  matrixA<- matrix(sample(-99:99,m*n,replace = T),m,n)
  write.table(matrixA, file = "Matriz.txt") #Genera el archivo txt
  View(matrixA)# Visualiza la matriz creada
  return (matrixA) #Retorna la matrixA

}#funcion para crear una matriz


