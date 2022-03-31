## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------
ak <- 150
nk <- 350
N <- 10000
n <- 500


## -------------------------------------------------------------------
# "p" gorro:
pg <- round(ak/nk,2)


## -------------------------------------------------------------------
# Varianza estimada:
var_p <- (N-n)/N * pg*(1-pg)/(nk-1)
print("# Varianza estimada:")
print(var_p)


## ----echo=FALSE-----------------------------------------------------
# IC al 95%:
LI <- pg - qt(0.025,nk-1, lower.tail = F)*sqrt(var_p)
LU <- pg + qt(0.025,nk-1, lower.tail = F)*sqrt(var_p)

print("# IC para P_k")
print("#       Limite Inf ,   Limite Sup")
paste("(", LI, ",", LU, ")")


## -------------------------------------------------------------------
#Valores necesitados:
nk <- 6
N <- 1000
n <- 10

# proporción estimada
pg <- nk/n
pg

#Tiempo en meses sin trabajar
tm <- c(3,12,5,7,8,2)


## -------------------------------------------------------------------
# Varianza estimada:
var_p <- (N-n)/N * pg*(1-pg)/(n-1)
print("# Varianza estimada:")
print(var_p)


## -------------------------------------------------------------------
# error de estimación:
b <- qt(0.025,n, lower.tail = F)*sqrt(var_p)
b


## -------------------------------------------------------------------
#Calculando el IC
print("#       Limite Inf ,   Limite Sup")
paste("(", pg-b, ",", pg+b, ")")


## -------------------------------------------------------------------
#Media estimada
yk <- mean(tm)
yk


## -------------------------------------------------------------------
# Varianza estimada
sk <- var(tm)
sk


## -------------------------------------------------------------------
#Funcion de varianza estimada de la media:
varmu <- function(N,n,sk,nk){
  "Ya que no se conoce Nk..."
  (N-n)/N * (sk)/nk
}


## ----echo=FALSE-----------------------------------------------------
print("# Calculando la varianza estimada de la media con N_k desconocida: ")
varyk <- varmu(N,n,sk,nk)
varyk


## -------------------------------------------------------------------
LI <- yk - qt(0.025,nk-1,lower.tail = F)*sqrt(varyk)


LU <- yk + qt(0.025,nk-1,lower.tail = F)*sqrt(varyk)

print("# IC para y_k")
print("#       Limite Inf ,   Limite Sup")
paste("(", LI, ",", LU, ")")


## -------------------------------------------------------------------
library(readxl)
df20 <- read_excel("data_sets20.xlsx")


## -------------------------------------------------------------------
# La función mean() hace lo descrito anteriormente:
## Podemos suponer que el ingreso esta escalonado en dolares

mug <- mean(df20$ingreso)
mug


## -------------------------------------------------------------------
# varianza muestral:
s2 <- var(df20$ingreso)
s2

#tamaño de la muestra:
n <- length(df20$ingreso)
n

#Tamaño de la población
N <- 1000

# Error
## Utilizamos una normal ya que el n > 30
B <- qnorm(0.025,lower.tail = F)*sqrt((s2*(N-n))/(N*n))
B


## -------------------------------------------------------------------
LI <- mug-B
LU <- mug+B
print("#       Limite Inf ,   Limite Sup")
paste("(", LI, ",", LU, ")")


## -------------------------------------------------------------------
# total poblacional tau:
taug <- mug*N
taug


## -------------------------------------------------------------------
print("#       Limite Inf ,   Limite Sup")
paste("(", LI*N, ",", LU*N, ")")


## ----include=FALSE--------------------------------------------------
library(magrittr)
library(tidyverse)


## -------------------------------------------------------------------
# Convertiremos en factor la columna genero
df20$genero %<>% as.factor()

# Conteo de las mujeres de la muestra:
## El dato que nos interesa es el primero ya que el otro representa número de columnas
df20 %>% filter(genero == "M") %>% dim(.)

fem <- 25


## -------------------------------------------------------------------
# Proporción estimada:
pg <- fem/n
pg


## -------------------------------------------------------------------
# Total poblacional:
N*pg


## -------------------------------------------------------------------
# Hallar el límite en el error de estimación
B <- qnorm(0.025,lower.tail = F)*sqrt(((pg*(1-pg))*(N-n))/((n-1)*N))
B

LI <- pg-B
LU <- pg+B

print("# IC para p")
print("#       Limite Inf ,   Limite Sup")
paste("(", LI, ",", LU, ")")


## -------------------------------------------------------------------
print("# IC para tau")
print("#       Limite Inf ,   Limite Sup")
paste("(", LI*N, ",", LU*N, ")")


## -------------------------------------------------------------------
boxplot(ingreso ~ genero, data = df20, col =c("red", "blue"))
legend("topright", inset = .02,legend=c("Hombres", "Mujeres"),
       col=c("red", "blue"), fill=c("red", "blue"), cex=0.8)


## -------------------------------------------------------------------
# Estimar el ingreso promedio de las subpoblaciones:

# Filtrar las bases de datos por hombre y mujer
dfH <- df20 %>% filter(genero == "H")
dfH
dfM <- df20 %>% filter(genero == "M")
dfM

# mirar promedios por genero:

## Promedio del ingreso Mujer:
mean(dfM$ingreso)

## Promedio del ingreso Hombre:
mean(dfH$ingreso)


## -------------------------------------------------------------------
# Datos a utilizar:
skH <- var(dfH$ingreso)
nkH <- length(dfH$ingreso)
skM <- var(dfM$ingreso)
nkM <- length(dfM$ingreso)


# Varianza de la media muestral:

## Hombres
varmuH <- varmu(N,n,skH,nkH)
varmuH

## Mujeres:
varmuM <- varmu(N,n,skM,nkM)
varmuM

