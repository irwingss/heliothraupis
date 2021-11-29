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
install.packages("ggpubr")
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

# 1.2. CREACIÓN DE UNA VARIABLE -------------------------------

variable1 <- 2021
numeros <- c(12.5, 11.9, 17.5, 15.1)
caracteres <- c("gen1", "gen2", "gen3", "gen4")
caract.logicos <- c(T, F, T, F)

# 1.3. CLASES DE VARIABLES: FUNCIONES DE PREGUNTAS LÓGICAS ---
is.numeric(numeros)
is.numeric(caracteres)

is.character(numeros)
is.character(caracteres)

is.logical(caract.logicos)

# 1.4. CLASES DE VARIABLES: FUNCIONES DE COERCIÓN LÓGICAS ---
as.numeric(caract.logicos)
as.numeric(caracteres)

as.character(numeros)
as.logical(numeros)

# 1.5. ESTRUCTURAS DE VARIABLES ------------------------------
vector <- c("A","B","A","C","A","A","B","A")
vnum <- 1:15

factor <- factor(vector)
table(factor)
levels(factor)
levels(factor) <- c("Z","B","C")
levels(factor)
relevel(factor, "C")

matriz <- matrix(vnum, ncol=3, nrow=5, byrow=TRUE)
matriz

data.frame <- data.frame(numeros, caracteres, caract.logicos)
data.frame

lista <- list(numeros, caracteres, caract.logicos)
lista

lista2 <- dplyr::lst(numeros, caracteres, caract.logicos, 
                     data.frame, matriz, factor, 
                     vector, lista)
lista2

tibble <- dplyr::tibble(numeros, caracteres, caract.logicos)
tibble

# 1.6. CREACIÓN DE FUNCIONES ---------------------------------
FarCel <- function(x){
  Centigrados <- (x-32)*5/9
  return(Centigrados)
}

FarCel(78)

rcub <- function(x,a=10){
  x**(1/3)/a
}
rcub(10,a=1000)

# 1.7. LEER ARCHIVOS EXCEL EN R  -------------------------------
difer <- openxlsx::read.xlsx(file.choose())
str(difer)

difer$detection_call <- as.factor(difer$detection_call)
str(difer)

# ----------------------------------------- -#
### ------------ MÓDULO II -------------- ####
# ----------------------------------------- -#
#         Estadística descriptiva            #
# ----------------------------------------- -#

# 2.1. PROBABILIDADES DISCRETAS (SIMULACIÓN DE MONTECARLO)
x <- 1:10
1/10
n <- 100000
rand <- sample(x, size=n, replace=TRUE)
table(rand)
prop.table(table(rand)) |> round(2)

# 2.2. PROBABILIDADES CONTINUAS
# Distribución Normal
library(Rlab)
data("earthq")
View(earthq)
str(earthq)

ecdf <- function(x,a) mean(x<=a)
range(earthq$mag)

library(latticeExtra)
ecdfplot(earthq$mag, 
         main="Función de Distribución Acumulativa ECDF")
ecdf(earthq$mag, 5)
1-ecdf(earthq$mag, 5)
pnorm(5, mean(earthq$mag), sd(earthq$mag))

library(ggpubr)

ggdensity(x="mag",data=earthq,
          title="Función de Densidad de Probabilidades")
dnorm(5, mean(earthq$mag), sd(earthq$mag))

earthq$site <- factor(earthq$site)
ggdensity(x="mag",data=earthq, fill="site",color="site",
          alpha=0.35,
          title="Función de Densidad de Probabilidades")

# 2.3. PRUEBAS DE NORMALIDAD
ggqqplot(x="mag",data=earthq)

length(earthq$mag)
shapiro.test(earthq$mag)
ks.test(earthq$mag, "pnorm", mean(earthq$mag), sd(earthq$mag))

nortest::ad.test(earthq$mag)
nortest::lillie.test(earthq$mag)
nortest::cvm.test(earthq$mag)

moments::skewness(earthq$mag)
moments::kurtosis(earthq$mag)

# 2.4. MEDIDAS DE TENDENCIA CENTRAL
summary(earthq)
range(earthq$mag)
mean(earthq$mag)
atipicos <-c(earthq$mag,100,125,119,105,101)
mean(atipicos)

psych::geometric.mean(atipicos)
psych::harmonic.mean(atipicos)

median(atipicos)

table(atipicos) |> sort(decreasing=TRUE) |> head(2)
table(earthq$freq) |> sort(decreasing=TRUE) |> head(2)

# 2.5. MEDIDAS DE DISPERSIÓN
sd(earthq$mag)
sd(earthq$mag)**2
var(earthq$mag)
range(earthq$mag)

# 2.6. ANÁLISIS EXPLORATORIO GRÁFICO
help(ggbarplot)
ggboxplot(x="site", y="mag", data=earthq, fill="site",
          add=c("jitter"), add.params=list(color="blue"))

ggscatter(x="freq", y="mag", data=earthq)
ggscatter(x="freq", y="mag", data=earthq, color="site",
          add="reg.line", conf.int = TRUE, cor.coef = TRUE,
          add.params = list(fill = "site", color = "site"),
          cor.coeff.args = list(method = "pearson", 
                                label.x = 14, label.sep = "\n"))

ggscatterhist(x="freq", y="mag", data=earthq, fill="site",color="site",
              add="reg.line", conf.int = TRUE, cor.coef = TRUE,
              add.params = list(fill = "site", color = "site"),
              cor.coeff.args = list(method = "pearson", 
                                    label.x = 14, label.sep = "\n"))

gghistogram(x="mag", data=earthq, fill = "site", color = "site")

ggdensity(x="mag", data=earthq, fill = "site", color = "site")

# ----------------------------------------- -#
### ------------ MÓDULO III ------------- ####
# ----------------------------------------- -#
#         Estadística inferencial            #
# ----------------------------------------- -#

# 3.1. REGRESIONES LINEALES
# Fórmulas a usar
full <- formula(mag ~ depth + site + freq + dist)
null <- formula(mag ~ 1)

# Crear modelos lineales
modF <- lm(mag ~ depth + site + freq + dist, data=earthq)
modF <- lm(full, data=earthq)
summary(mod)

modF <- lm(mag ~ ., data=earthq)
summary(mod)

modN <- lm(mag~1, data=earthq)
modN <- lm(null, data=earthq)
  
# 3.1.1. Asunciones teóricas
# A1: respuesta numérica continua
head(earthq$mag)

# A2: linealidad entre explicativas y respuesta
ggscatter(x="freq", y="mag", data=earthq)

# A3: residuales independientes
# A4: no outliers en la variable respuesta
out <- rstatix::identify_outliers(earthq,mag)
nueva_DF <- dplyr::anti_join(earthq, out)

# A5: residuales normales
# A6: residuales homocedásticos
par(mfrow=c(2,2))
plot(modF)

# 3.1.2. Selección de modelos
anova(modF)

summary(mod)
anova(modF,modN)

drop1(modF)
add1(lm(null, data=earthq), modF)

mod2 <- lm(mag ~ depth + site + freq, data=earthq)
anova(modF,mod2)
AIC(modF)
AIC(mod2)

# 3.1.3. Interpretación del modelo final
modFINAL <- lm(mag ~ ., data=earthq)
coef(modFINAL)
broom::tidy(modFINAL, conf.int=TRUE)

# 3.1.4. Gráficos de efectos
library(effects)
plot(allEffects(modFINAL))
plot(predictorEffects(modFINAL, ~ depth),
     confint=list(style="bars"))

library(sjPlot)
plot_model(modFINAL)
plot_model(modFINAL, type="eff")
plot_model(modFINAL, type="eff", terms=c("depth","site"))

# 3.2. ANOVA
# 3.2.1. Crear un ANOVA es sencilo
data("iris")
ava <- aov(Sepal.Length~Species, data=iris)
summary(ava)

# 3.2.2. Prueba Post Hoc
TukeyHSD(ava)

# 3.2.3. Asunciones teóricas
# A1: respuesta continua
# A2: residuales independientes
# A3: no outliers en var. respuesta
library(tidyverse)
 out2 <- iris %>% 
  group_by(Species) %>% 
  rstatix::identify_outliers(Sepal.Length)
iris2 <- dplyr::anti_join(iris,out2)

# A4: residuales distribución normal
# A5: residuales homocedásticos
nortest::ad.test(resid(ava))
library(HH)
hov(Sepal.Length~Species, data=iris2)
bartlett.test(Sepal.Length~Species, data=iris2)
rstatix::welch_anova_test(Sepal.Length~Species, data=iris)

# 3.2.4. Gráfico de efectos
library(effects)
plot(allEffects(ava))
