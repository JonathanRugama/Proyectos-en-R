


1#Permite generar la matriz
# source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/GeneraMatriz.R")
Matriz1<- GenerarMatriz(4,4)#Fucion para generar datos aleatorios en la matriz
Matriz2<- GenerarMatriz(4,4)#Fucion para generar datos aleatorios en la matriz

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/GenerarMatriz_Ingresada.R")
Matriz1<-GenerarMatrizManual(2,2)#Funcion para ingresar datos a la matriz
Matriz2<-GenerarMatrizManual(2,2)#Funcion para ingresar datos a la matriz

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/FuncionSuma.R")
FuncionSuma#Funcion para sumar dos matrices

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/FuncionResta.R")
FuncionResta(Matriz1,Matriz2)#Funcion para restar dos matrices

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/FuncionMultiplicar.R")
FuncionMultiplicar(Matriz1,Matriz2)#Funcion para multiplicar dos matrices

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/Funcion_Det_OpElementales.R")
Funcion_Det_OpElementalesE(Matriz1) #Funcion para calcular el determinante de una funcion de nxn mediante operaciones elementales.

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/Funcion_Inversa_OpElementales.R")
Funcion_Inversa_OpElementales(Matriz1)#Funcion para calcular la inversa de matriz de nxn mediante operaciones elementales.

source("R/Funcion_CalculoPotencia.R")
FuncionPotencia(Matriz1,5)#Funcion para calcular la potencia de una matriz a la n

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/Funcion_Det_Recursivo.R")
Funcion_Det_Recursivo(Matriz1)#Funcion para calcular el determinante de matriz de orden n con el metodo de definicion(Recursividad)

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/CalcularInversaConUsoDelDeterminante.R")
InversaConUsoDelDeterminante(Matriz1)#Funcion para calcular la inversa de una funcion mediante la Adjunta

source("C:/Users/Jonathan/OneDrive/Informática Empresarial/Algebra Lineal/Trabajos/Proyecto1/Codigo_R/R/Funcion_Rango.R")
Funcion_Rango(Matriz1)#Funcion para calcular el rango de una matriz
