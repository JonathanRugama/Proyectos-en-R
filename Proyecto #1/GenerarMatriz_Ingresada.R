
GenerarMatrizManual<-function(m,n)
{
  CantidadValores<-m*n
  matrixA<- matrix(scan(file = "",what = ,nmax= CantidadValores),m,n,byrow = T)
  write.table(matrixA, file = "Matriz.txt")
  return (matrixA)
}



