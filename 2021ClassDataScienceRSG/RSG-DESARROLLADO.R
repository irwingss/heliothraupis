# ----------------------------------------- -#
### ------------- MÓDULO I -------------- ####
# ----------------------------------------- -#
# Introducción al lenguaje de programación R #
# ----------------------------------------- -#

# 1.1. SHORCUTS A TENER EN CUENTA -----------------------------
# Control + Shift + N  " Nuevo script "
# Control + Shift + M  " |> " (Crea un pipe. Configuramos antes)
# Control + Enter      " Ejecutar el código seleccionado "
# Alt + -              " <- " (Crear el operador de asignación)

# Instalar algunos paquetes
install.packages("ggpubr") # COMENTARIO ADICIONAL
install.packages("tidyverse")
install.packages("Rlab")
install.packages("latticeExtra")
install.packages("rstatix")
install.packages("broom")
install.packages("nortest")
install.packages("tidyverse")
install.packages("HH")
install.packages("sjPlot")
install.packages("effects")
install.packages("moments")
install.packages("psych")

# 1.2. CREACIÓN DE UNA VARIABLE -------------------------------
num <- 123
numeros <- c(123, 120, 119, 110, 111, 130)
texto <- c("A","A","T","C","C","A")
logico <- c(TRUE, T, FALSE, F, T, F)

# 1.3. CLASES DE VARIABLES: FUNCIONES DE PREGUNTAS LÓGICAS ---
is(numeros)
is.numeric(numeros)
is.numeric(texto)
is.logical(numeros)

# 1.4. CLASES DE VARIABLES: FUNCIONES DE COERCIÓN LÓGICAS ---
logico
as.numeric(logico)
as.character(numeros)

# 1.5. ESTRUCTURAS DE VARIABLES ------------------------------
# Vector
numeros <- c(123, 120, 119, 110, 111, 130)
numeros[6]
texto <- c("A","A","T","C","C","A")

# Factor
factor <- factor(texto)
levels(factor)
factor <- relevel(factor, "T")
factor
factor <- factor(texto, labels = c("B","C","T"))
factor

# Matrices
matrix(1:100, nrow = 10, ncol=10)
matrix(1:100, nrow = 10, ncol=10, byrow=TRUE)

# Data.Frame
DF <- data.frame(numeros, texto, logico)
is(DF)
str(DF)
names(DF)
nrow(DF)
ncol(DF)

# DF[FILA , COLUMNA]
DF[1,2]
DF[,2] # RESULTA EN UN VECTOR 
DF[1,] # RESULTA EN UNA TABLA DE 1 FILA

DF$texto
DF$texto <- factor(DF$texto)
str(DF)

# Tibble
library(tidyverse)
tibble(numeros, texto, logico)

data("iris")
iris
iris <- as_tibble(iris)

View(iris)

# Listas
lista <- list(numeros, texto, iris, DF, factor)
str(lista)
lista
lista[[1]]
lista[[3]]
lista[[3]][,5]

lista2 <- dplyr::lst(DF, texto, lista)
lista2
lista2$DF

# 1.6. CREACIÓN DE FUNCIONES ---------------------------------
FarCel <- function(x){
  (x-32)*5/9
}

FarCel(72)

FarCel <- function(x) (x-32)*5/9
FarCel(90)

FarCel <- function(x){
  centigrados <<- (x-32)*5/9
  return(centigrados)
}
FarCel(100)
ifelse()

#for()
#if()
#else()
#while()

View(FarCel)
View(rstatix::shapiro_test)

# 1.7. LEER ARCHIVOS EXCEL EN R  -------------------------------
# openxlsx
difer <- openxlsx::read.xlsx(file.choose())
View(difer)

openxlsx::write.xlsx(iris, "tabla iris.xlsx")
getwd()

# ----------------------------------------- -#
### ------------ MÓDULO II -------------- ####
# ----------------------------------------- -#
#         Estadística descriptiva            #
# ----------------------------------------- -#

# 2.1. PROBABILIDADES DISCRETAS (SIMULACIÓN DE MONTECARLO)
1/6
set.seed(123)
muestra <- sample(1:6, size=10, replace=TRUE)
prop.table(table(muestra))

muestra <- sample(1:6, size=100000, replace=TRUE)
prop.table(table(muestra))

muestra |> 
  table() |> 
  prop.table()

# 2.2. PROBABILIDADES CONTINUAS
# Distribución Normal
library(Rlab)
data("earthq")
View(earthq)
str(earthq)

earthq$site <- factor(earthq$site)
str(earthq)

as.numeric(TRUE)
as.numeric(FALSE)

mean(c(TRUE, FALSE, FALSE, FALSE)) # me devuelve prop. de 1s
1/4
range(earthq$mag)

earthq$mag <= 5
mean(earthq$mag <= 5)
1 - mean(earthq$mag <= 5)

ecdf(CONJUNTODATOS, VALOR)
ecdf(earthq$mag, 5)

ecdf <-  function(data, condicion) {
  proporcion1 <- mean(data <= condicion)
  return(proporcion1)
}

ecdf <- function(data, condicion){mean(data <= condicion)}

range(earthq$depth)
ecdf(earthq$depth, 50)
1-ecdf(earthq$depth, 50)

library(latticeExtra)
ecdfplot(earthq$mag)
ecdf(earthq$mag, 6)

dnorm() # Función de densidad
pnorm() # Función de masa (ECDF)
qnorm() # El valor normal que corresponde a un cuantil
rnorm() # Números aleatorios

rnorm(100, mean= 25.1, sd= 1.63)
rpois(100, lambda=10)

mean(earthq$mag)
sd(earthq$mag)
length(earthq$mag)

pnorm(6, mean=4.891892, sd=sd(earthq$mag))


plot(density(earthq$mag))
earthq$mag |> density() |> plot()
dnorm(5, mean=mean(earthq$mag), sd=sd(earthq$mag))

# 2.3. PRUEBAS DE NORMALIDAD
library(ggpubr)
ggqqplot(earthq$mag)

# H0: No existen dife. signif. entre la ECDF de mis datos
# y la distribución teórica normal (mis datos siguen la normalidad)

# Ha: Existen dife. signif. entre la ECDF de mis datos
# y la distribución teórica normal (mis datos NO siguen la normalidad)

length(earthq$mag)
shapiro.test(earthq$mag) # >0.05 HAY NORMALIDAD
ks.test(earthq$mag, "pnorm", mean(earthq$mag), sd(earthq$mag))  
# >0.05 HAY NORMALIDAD 

nortest::lillie.test(earthq$mag) # <0.05 NO HAY NORMALIDAD
nortest::ad.test(earthq$mag) # <0.05 NO HAY NORMALIDAD

moments::skewness(earthq$mag) # NORMAL SI ESTÁ ENTE -0.5 +0.5
moments::kurtosis(earthq$mag) # ~3 MESOCURTICA (NORMAL)
                              # >3 LEPTOCURTICA (MUY ALTA)
                              # <3 PLATICURTICA (MUY BAJA)

# 2.4. MEDIDAS DE TENDENCIA CENTRAL
mean() # promedio aritmético (-inf, +inf)
psych::geometric.mean() #promedio geométrico (>0)
psych::harmonic.mean() #promedio armónico (>0)

earthq$mag
sort(earthq$mag)
mean(earthq$mag == 0)

range(earthq$mag)

mean(earthq$mag)
mean(c(earthq$mag, 98, 100,110,120,115))
mean(c(earthq$mag, 550))

psych::geometric.mean(c(earthq$mag, 98, 100,110,120,115))

median(earthq$mag)
median(c(earthq$mag, 98, 100,110,120,115)) # NO SE SESGA POR VALORES ATÍPICOS

table(earthq$mag) |> sort(decreasing=TRUE) |> head(1)

# 2.5. MEDIDAS DE DISPERSIÓN
sd(earthq$mag) # desviación est.
sd(earthq$mag)**2
sd(earthq$mag)^2

var(earthq$mag) # varianza
min(earthq$mag)
max(earthq$mag)
range(earthq$mag)

summary(earthq$mag)
boxplot(earthq$mag)

# 2.6. ANÁLISIS EXPLORATORIO GRÁFICO
library(ggpubr)
data("earthq")
earthq$site <- factor(earthq$site)

help(ggboxplot)
ggboxplot(y="mag", x="site", fill="site",
          palette=c("orange","pink"),
          add= "jitter",
          data=earthq)

ggscatter(x="freq", y="mag", data=earthq)
ggscatter(x="freq", y="mag", data=earthq,
          color="site", facet.by="site",
          add="reg.line",   conf.int = TRUE,
          add.params=list(fill="gray70"))

ggscatter(x="freq", y="mag", data=earthq,
          color="site",
          ellipse = TRUE,
          ellipse.type = "convex")

help(ggscatter)

ggscatterhist(x="freq", y="mag", data=earthq,
              color="site", fill="site")

gghistogram(x="mag", data=earthq,
            color="site", fill="site",
            binwidth=0.1)

ggdensity(x="mag", data=earthq,
          color="site", fill="site")

# ----------------------------------------- -#
### ------------ MÓDULO III ------------- ####
# ----------------------------------------- -#
#         Estadística inferencial            #
# ----------------------------------------- -#

# 3.1. REGRESIONES LINEALES
# Fórmulas a usar
names(earthq)
data("earthq")
earthq$depth <- sqrt(earthq$depth)

full <- formula(mag~site+freq+depth+dist)
null <- formula(mag~1)
full

# Crear modelos lineales
mod1 <- lm(full, data=earthq)
summary(mod1)

# 3.1.1. Asunciones teóricas
# A1: respuesta numérica continua
earthq$mag

# A2: linealidad entre explicativas y respuesta
ggscatter(x="freq", y="mag", data=earthq,
          color="site", facet.by="site")

ggscatter(x="depth", y="mag", data=earthq,
          color="site", facet.by="site")

ggscatter(x="dist", y="mag", data=earthq,
          color="site", facet.by="site")

# A3: residuales independientes
# A4: no outliers en la variable respuesta
out <- rstatix::identify_outliers(earthq, mag)
earthq2 <- anti_join(earthq,out)

# A5: residuales normales
# A6: residuales homocedásticos
par(mfrow=c(2,2))
plot(mod1)

# 3.1.2. Selección de modelos
nulo <- lm(null, data=earthq)
anova(mod1, nulo)
anova(mod1)

mod2 <- lm(mag ~ site+freq+depth, data=earthq)
anova(mod1, mod2, nulo)

AIC(mod1)
AIC(mod2)

step(nulo, scope=formula(mod1))

modFINAL <- lm(formula = mag ~ freq + site + dist + depth, data = earthq)

# 3.1.3. Interpretación del modelo final
summary(modFINAL)

# 3.1.4. Gráficos de efectos
library(effects)
plot(allEffects(modFINAL))

library(sjPlot)
plot_model(modFINAL, show.values=TRUE)
plot_model(modFINAL, type="eff", terms=c("freq", "site"))
plot_model(modFINAL, type="eff", terms=c("freq", "depth"))

# 3.2. ANOVA
# 3.2.1. Crear un ANOVA es sencillo
t.test(y~CATEGORICA, data=earthq)
t.test(mag~site, data=earthq)

data("iris")
names(iris)
levels(iris$Species)
anova <- aov(Petal.Length ~ Species, data=iris)
summary(anova)

# 3.2.2. Prueba Post Hoc
TukeyHSD(anova)

# 3.2.3. Asunciones teóricas
# A1: respuesta continua
# A2: residuales independientes
# A3: no outliers en var. respuesta (POR GRUPO)
library(tidyverse)
iris %>%
  group_by(Species) %>%
  rstatix::identify_outliers(Petal.Length) -> out2
table(iris$Species)

iris2 <- anti_join(iris, out2)
table(iris2$Species)

# A4: residuales distribución normal
# A5: residuales homocedásticos
nortest::ad.test(resid(anova))
bartlett.test(Petal.Length ~ Species, data=iris)

# 3.2.4. Gráfico de efectos
library(effects)
plot(allEffects(anova))

library(ggstatsplot)
ggbetweenstats()