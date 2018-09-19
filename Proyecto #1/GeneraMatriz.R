
GenerarMatriz<- function(m,n){

  matrixA<- matrix(sample(-10:10,m*n,replace = T),m,n)
  write.table(matrixA, file = "Matriz.txt")
  return (matrixA)

}#funcion para crear una matriz


GenerarMatrizManual<-function(m,n)
{
  matrixA<- matrix(scan(),m*n,m,n)
  write.table(matrixA, file = "Matriz.txt")
  return (matrixA)
}
