
GenerarMatrizManual<-function(m,n)
{
  CantidadValores<-m*n #Se multipica las filas por columnas para obtener la cantidad de valores
                      # que se pueden ingresar.

  #Se ingresa la matriz por filas
  matrixA<- matrix(scan(file = "",what = ,nmax= CantidadValores),m,n,byrow = T)
  #Y luego se genera el archivo de tipo TXT
  write.table(matrixA, file = "Matriz.txt")
  #Se muestra la matriz
  view(matrixA)
  #Retorna la matriz
  return (matrixA)
}



