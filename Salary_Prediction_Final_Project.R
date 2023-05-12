library(tidyverse)
library(dplyr)
library(xts)
library(psych)
library(gmodels)
library(MASS)
library(fitdistrplus)
library(lmtest)
library(fdth)
library(readxl)
library(ggplot2)
library(plotly)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)
library(kableExtra)
library(pastecs)
library(readr)



#Leer la base de datos Daniel
#Salary_Prediction <- read.csv("C:/Users/Danie/OneDrive/Semestre4/Inferencia estadistica/ProyectoR/eda_data.csv")
#View(Salary_Prediction)

#Base de datos de la India Daniel
#Data_Science_Jobs_in_India <- read_csv("C:/Users/Danie/OneDrive/Semestre4/Inferencia estadistica/ProyectoR/Data_Science_Jobs_in_India.csv")
#View(Data_Science_Jobs_in_India)

#Base de datos de Estados Unidos Daniel
#data_cleaned_2021 <- read_csv("C:/Users/Danie/OneDrive/Semestre4/Inferencia estadistica/ProyectoR/data_cleaned_2021.csv")
#View(data_cleaned_2021)

#Base de datos de la India Jacobo
Data_Science_Jobs_in_India <- read_csv("C:/Users/Jacobo Ossa/OneDrive - Universidad Icesi (@icesi.edu.co)/Escritorio/Semestre_4/Inferencia Estadistica/Proyecto-Final-Salary-Prediction/Data Science Jobs in India/Data_Science_Jobs_in_India.csv")
View(Data_Science_Jobs_in_India)

#Base de Datos de Estados Unidos Jacobo
data_cleaned_2021 <- read.csv("C:/Users/Jacobo Ossa/OneDrive - Universidad Icesi (@icesi.edu.co)/Escritorio/Semestre_4/Inferencia Estadistica/Proyecto-Final-Salary-Prediction/Data Science Jobs in USA (2021)/data_cleaned_2021.csv")
View(data_cleaned_2021)

#Importar Datos Salary Prediction 
Salary_Prediction <- read_csv("C:/Users/Jacobo Ossa/OneDrive - Universidad Icesi (@icesi.edu.co)/Escritorio/Semestre_4/Inferencia Estadistica/Proyecto-Final-Salary-Prediction/eda_data.csv")
View(Salary_Prediction)




# AGRUPAR LOS ESTADOS POR REGIONES
Salary_Prediction <- Salary_Prediction %>%
  mutate(job_region = case_when(job_state == "WA" | job_state == "OR" | job_state == "CA" | job_state == "NV" 
                                | job_state == "AZ" | job_state == "UT" | job_state == "ID" | job_state == "MT" 
                                | job_state == "WY" | job_state == "CO" | job_state == "NM" | job_state == "AK" 
                                ~ "WEST",
                                job_state == "ND" | job_state == "SD" | job_state == "NE" | job_state == "KS" 
                                | job_state == "MN" | job_state == "IA" | job_state == "MO" | job_state == "WI" 
                                | job_state == "IL" | job_state == "IN" | job_state == "MI" | job_state == "OH" 
                                ~ "MIDWEST",
                                job_state == "PA" | job_state == "NY" | job_state == "NJ" | job_state == "CT" 
                                | job_state == "MA" | job_state == "NH" | job_state == "VT" | job_state == "RI" 
                                | job_state == "ME"
                                ~ "NORTHEAST",
                                job_state == "TX" | job_state == "OK" | job_state == "AR" | job_state == "LA" 
                                | job_state == "MS" | job_state == "AL" | job_state == "TN" | job_state == "KY" 
                                | job_state == "GA" | job_state == "FL" | job_state == "SC" | job_state == "NC" | job_state == "VA" 
                                | job_state == "DC" | job_state == "WV" | job_state == "DE" | job_state == "MD" 
                                ~ "SOUTH"))


#AGRUPAR EL RATING POR INTERVALO
Salary_Prediction <- Salary_Prediction %>% 
  mutate(intervalos = cut(Salary_Prediction$Rating, breaks = c(0,1,2,3,4,5)))

#AGRUPAR EL SALARIO POR INTERVALO
Salary_Prediction <- Salary_Prediction %>% 
  mutate(avg_salary_Intervalos = cut(Salary_Prediction$avg_salary, breaks = c(13.5,73.5,133.5,193.5,253.5,254)))

#Na.omit en el nuevo dataframe
Salary_Prediction<- na.omit(Salary_Prediction)

#PROMEDIO DE AVG_SALARY CON TIPO DE EMPRESA
meanOfSalary<-aggregate(Salary_Prediction$avg_salary ~ Salary_Prediction$Type.of.ownership, data = Salary_Prediction, FUN = mean)

#PROMEDIO DE RATING CON TIPO DE EMPRESA
meanOfRating<-aggregate(Salary_Prediction$Rating ~ Salary_Prediction$Type.of.ownership, data = Salary_Prediction, FUN = mean)






#VARIABLES CUALITATIVAS:
#ANALISIS PARA LA VARIABLE job_simp O NOMBRE DE EMPLEO
####
#Tabla Frecuencia Relativa
Frecuencia_absoluta<- table(Salary_Prediction$job_simp)
Frecuencia_absoluta
####
#Tabla Frecuencia Relativa porcentual
Frecuencia_relativa <- prop.table(table(Salary_Prediction$job_simp))
Frecuencia_relativa
####
###Tabla de frecuencias
####
tabla_frec_job_simp = Salary_Prediction %>% group_by(job_simp) %>%
  summarize(Frecuencia_absoluta=n())%>%
  mutate(Frecuencia_relativa=Frecuencia_absoluta/sum(Frecuencia_absoluta)*100)

tabla_frec_job_simp

kable(tabla_frec_job_simp)%>%
  kable_styling(full_width = F)
####
#Diagrama De Barras
####
barplot(table(Salary_Prediction$job_simp), col = c("lightblue"),
        main = "Diagrama de barras de las frecuencias absolutas\n de la variable \"Job_Simp\"")




#ANALISIS PARA LA VARIABLE Type.of.ownership O TIPO DE EMPRESA
####
#Tabla Frecuencia Relativa
####
Frecuencia_absoluta<- table(Salary_Prediction$Type.of.ownership)
Frecuencia_absoluta
####
#Tabla Frecuencia Relativa porcentual
####
Frecuencia_relativa <- prop.table(table(Salary_Prediction$Type.of.ownership))
Frecuencia_relativa
####
###Tabla de frecuencias
####
tabla_frec_Type.of.ownership = Salary_Prediction %>% group_by(Type.of.ownership) %>%
  summarize(Frecuencia_absoluta=n())%>%
  mutate(Frecuencia_relativa=Frecuencia_absoluta/sum(Frecuencia_absoluta)*100)

tabla_frec_Type.of.ownership

kable(tabla_frec_Type.of.ownership)%>%
  kable_styling(full_width = F)
####
#Diagrama De Barras
####
barplot(table(Salary_Prediction$Type.of.ownership), col = c("lightblue"),
        main = "Diagrama de barras de las frecuencias absolutas\n de la variable \"type_of_ownership\"")


#Tabla Frecuencia Relativa
####
Frecuencia_absoluta<- table(Salary_Prediction$job_region)
Frecuencia_absoluta
####

#Tabla Frecuencia Relativa porcentual
####
Frecuencia_relativa <- prop.table(table(Salary_Prediction$job_region))
Frecuencia_relativa
####
###Tabla de frecuencias
####
tabla_frec_job_region = Salary_Prediction %>% group_by(job_region) %>%
  summarize(Frecuencia_absoluta=n())%>%
  mutate(Frecuencia_relativa=Frecuencia_absoluta/sum(Frecuencia_absoluta)*100)

tabla_frec_job_region

kable(tabla_frec_job_region)%>%
  kable_styling(full_width = F)
####
#Diagrama De Barras
####
barplot(table(Salary_Prediction$job_region), col = c("lightblue"),
        main = "Diagrama de barras de las frecuencias absolutas\n de la variable \"Job_State\"")



#ANALISIS PARA LA VARIABLE job.state o ESTADO DE EMPLEO
###
#Tabla Frecuencia Relativa
####
Frecuencia_absoluta<- table(Salary_Prediction$job_state)
Frecuencia_absoluta
####

#Tabla Frecuencia Relativa porcentual
####
Frecuencia_relativa <- prop.table(table(Salary_Prediction$job_state))
Frecuencia_relativa
####
###Tabla de frecuencias
####
tabla_frec_job_state = Salary_Prediction %>% group_by(job_state) %>%
  summarize(Frecuencia_absoluta=n())%>%
  mutate(Frecuencia_relativa=Frecuencia_absoluta/sum(Frecuencia_absoluta)*100)

tabla_frec_job_state

kable(tabla_frec_job_state)%>%
  kable_styling(full_width = F)
####
#Diagrama De Barras
####
barplot(table(Salary_Prediction$job_state), col = c("lightblue"),
        main = "Diagrama de barras de las frecuencias absolutas\n de la variable \"Job_State\"")



####
#Tablas Cruzadas
####
ratingVSempresa <- cut(Salary_Prediction$Rating, c(seq(from = 0, to = 5, by = 1),10), include.lowest=TRUE)
CrossTab_Rating_Vs_Empresa <- crosstab(Salary_Prediction$Type.of.ownership, ratingVSempresa, prop.c = TRUE, plot=FALSE)
CrossTab_Rating_Vs_Empresa
####
competidoresVSempleo <- cut(Salary_Prediction$num_comp, c(seq(from = 0, to = 4, by = 1),8), include.lowest=TRUE)
CrossTab_Competidores_Vs_Empleo <- crosstab(Salary_Prediction$job_simp, competidoresVSempleo, prop.c = TRUE, plot=FALSE)
CrossTab_Competidores_Vs_Empleo
####
SalarioPromedioVSempleo<-cut(Salary_Prediction$avg_salary, c(seq(from = 13, to = 255, by = 48.1),5), include.lowest=TRUE)
CrossTab_SalarioPromedio_Vs_Empleo <- crosstab(Salary_Prediction$job_simp, SalarioPromedioVSempleo, prop.c = TRUE, plot=FALSE)
CrossTab_SalarioPromedio_Vs_Empleo 
####
SalarioPromedioVSestado<-cut(Salary_Prediction$avg_salary, c(seq(from = 13, to = 255, by = 48.1),5), include.lowest=TRUE)
CrossTab_SalarioPromedio_Vs_Estado <- crosstab(Salary_Prediction$job_state, SalarioPromedioVSestado, prop.c = TRUE, plot=FALSE)
CrossTab_SalarioPromedio_Vs_Estado

SalarioPromedioVSregion<-cut(Salary_Prediction$avg_salary, c(seq(from = 13, to = 255, by = 48.1),5), include.lowest=TRUE)
CrossTab_SalarioPromedio_Vs_Estado <- crosstab(Salary_Prediction$job_region, SalarioPromedioVSestado, prop.c = TRUE, plot=FALSE)
CrossTab_SalarioPromedio_Vs_Estado 




#Diagrama de Barras Agrupado
####
Barplot_Rating_VS_Empresa <- freq(ordered(ratingVSempresa ), plot = TRUE)
####
Barplot_Competidores_VS_Empleo <- freq(ordered(competidoresVSempleo ), plot = TRUE)
####
Barplot_SalarioPromedio_VS_Empleo <- freq(ordered(SalarioPromedioVSempleo ), plot = TRUE)
####




##VARIABLES CUANTITATIVAS:

#ANALISIS PARA LA VARIABLE num_comp O NUMERO DE COMPETIDORES
#Calculamos la estadistica descriptiva
desc <- summary(Salary_Prediction, Salary_Prediction$num_comp)
desc

attach(Salary_Prediction)

#Calculamos el valor minimo para la varible num_comp o numero de competidores
min <- min(`num_comp`)
min

#Calculamos el valor maximo para la varible num_comp o numero de competidores
max <- max(`num_comp`)
max

#Calculamos el rango para la varibale num_comp o numero de competidores
rango <- max-min
rango

#Calculamos la media para la varibale num_comp o numero de competidores
media <- mean(`num_comp`)
media

#Calculamos la desviacion estandar para la variable num_comp o numero de competidores
desv_estandar <- sd(`num_comp`)
desv_estandar

#Calculamos el coeficiente de variacion para la variable num_comp o numero de competidores
coef_var <- desv_estandar/media
coef_var

#Calculamos el cuartil 1 para la variable num_comp o numero de competidores
q1 <- unname(quantile(`num_comp`, 0.25))
q1

#Calculamos el cuartil 3 para la variable num_comp o numero de competidores
q3 <- unname(quantile(`num_comp`, 0.75))
q3

#Calculamos el rango intercuartil para la variable num_comp o numero de competidores
ric <- q3 - q1
ric

#Calculamos el cerco inferior para la variable num_comp o numero de competidores
cerco_inf <- q1 - 1.5*ric
cerco_inf

#Calculamos el cerco superior para la varible num_comp o numero de competidores
cerco_sup <- q3 + 1.5*ric
cerco_sup

#Vemos que valores se salen de los cercos para la variable num_comp o numero de competidores
atipicos <- filter(Salary_Prediction, `num_comp`<cerco_inf | `num_comp`>cerco_sup)
kable(atipicos) %>% kable_styling(full_width = F)

datos_sinatipicos <- filter(Salary_Prediction,`num_comp`>=cerco_inf & `num_comp`<=cerco_sup)

max <- max(datos_sinatipicos$`num_comp`)
max

attach(datos_sinatipicos)

#Grafico de cajas para la variable num_comp o numero de competidores
boxplot_num_comp = ggplot(datos_sinatipicos, aes(x='', y=num_comp)) +
  geom_boxplot(color='grey10', width=0.5) +
  labs(x='', y='Numero de competidores', title='Grafico de cajas para el numero de competidores') +
  theme_minimal()
ggplotly(boxplot_num_comp)

# estadisticas descriptivas sin atipicos para la variable num_comp o numero de competidores

summary(datos_sinatipicos)

descriptivas <- stat.desc(datos_sinatipicos$num_comp)
descriptivas

options(scipen = 999999)

#Exploracion de datos para la variable num_comp o numero de competidores
eda(num_comp)



#ANALISIS PARA LA VARIABLE avg_salary O SALARIO PROMEDIO
#Calculamos la estadistica descriptiva
desc <- summary(Salary_Prediction, Salary_Prediction$avg_salary)
desc

attach(Salary_Prediction)

#Calculamos el valor minimo para la varible avg_salary o salario promedio
min <- min(`avg_salary`)
min

#Calculamos el valor maximo para la varible avg_salary o salario promedio
max <- max(`avg_salary`)
max

#Calculamos el rango para la varibale avg_salary o salario promedio
rango <- max-min
rango

#Calculamos la media para la varibale avg_salary o salario promedio
media <- mean(`avg_salary`)
media

#Calculamos la desviacion estandar para la variable avg_salary o salario promedio
desv_estandar <- sd(`avg_salary`)
desv_estandar

#Calculamos el coeficiente de variacion para la variable avg_salary o salario promedio
coef_var <- desv_estandar/media
coef_var

#Calculamos el cuartil 1 para la variable avg_salary o salario promedio
q1 <- unname(quantile(`avg_salary`, 0.25))
q1

#Calculamos el cuartil 3 para la variable avg_salary o salario promedio
q3 <- unname(quantile(`avg_salary`, 0.75))
q3

#Calculamos el rango intercuartil para la variable avg_salary o salario promedio
ric <- q3 - q1
ric

#Calculamos el cerco inferior para la variable avg_salary o salario promedio
cerco_inf <- q1 - 1.5*ric
cerco_inf

#Calculamos el cerco superior para la varible avg_salary o salario promedio
cerco_sup <- q3 + 1.5*ric
cerco_sup

#Vemos que valores se salen de los cercos para la variable avg_salary o salario promedio
atipicos <- filter(Salary_Prediction, `avg_salary`<cerco_inf | `avg_salary`>cerco_sup)
kable(atipicos) %>% kable_styling(full_width = F)

datos_sinatipicos <- filter(Salary_Prediction,`avg_salary`>=cerco_inf & `avg_salary`<=cerco_sup)

max <- max(datos_sinatipicos$`avg_salary`)
max

attach(datos_sinatipicos)

#Grafico de cajas para la variable avg_salary o salario promedio
boxplot_avg_salary = ggplot(datos_sinatipicos, aes(x='', y=avg_salary)) +
  geom_boxplot(color='grey10', width=0.5) +
  labs(x='', y='Salario promedio', title='Grafico de cajas para el salario promedio') +
  theme_minimal()
ggplotly(boxplot_avg_salary)

# estadisticas descriptivas sin atipicos para la variable avg_salary o salario promedio

summary(datos_sinatipicos)

descriptivas <- stat.desc(datos_sinatipicos$avg_salary)
descriptivas

options(scipen = 999999)

#Exploracion de datos para la variable avg_salary o salario promedio
eda(avg_salary)


#ANALISIS PARA LA VARIABLE rating O PUNTUACION
#Calculamos la estadistica descriptiva
desc <- summary(Salary_Prediction, Salary_Prediction$Rating)
desc

attach(Salary_Prediction)

#Calculamos el valor minimo para la variable rating o puntuacion
min <- min(`Rating`)
min

#Calculamos el valor maximo para la variable rating o puntuacion
max <- max(`Rating`)
max

#Calculamos el rango para la variable rating o puntuacion
rango <- max-min
rango

#Calculamos la media para la variable rating o puntuacion
media <- mean(`Rating`)
media

#Calculamos la desviacion estandar para la variable rating o puntuacion
desv_estandar <- sd(`Rating`)
desv_estandar

#Calculamos el coeficiente de variacion para la variable rating o puntuacion
coef_var <- desv_estandar/media
coef_var

#Calculamos el cuartil 1 para la variable rating o puntuacion
q1 <- unname(quantile(`Rating`, 0.25))
q1

#Calculamos el cuartil 3 para la variable rating o puntuacion
q3 <- unname(quantile(`Rating`, 0.75))
q3

#Calculamos el rango intercuartil para la variable rating o puntuacion
ric <- q3 - q1
ric

#Calculamos el cerco inferior para la variable rating o puntuacion
cerco_inf <- q1 - 1.5*ric
cerco_inf

#Calculamos el cerco superior para la variable rating o puntuacion
cerco_sup <- q3 + 1.5*ric
cerco_sup

#Vemos que valores se salen de los cercos para la variable rating o puntuacion
atipicos <- filter(Salary_Prediction, `Rating`<cerco_inf | `Rating`>cerco_sup)
kable(atipicos) %>% kable_styling(full_width = F)

datos_sinatipicos <- filter(Salary_Prediction,`Rating`>=cerco_inf & `Rating`<=cerco_sup)

max <- max(datos_sinatipicos$`Rating`)
max

attach(datos_sinatipicos)

#Grafico de cajas para la variable rating o puntuacion
boxplot_Rating = ggplot(datos_sinatipicos, aes(x='', y=Rating)) +
  geom_boxplot(color='grey10', width=0.5) +
  labs(x='', y='Rating', title='Grafico de cajas para la puntuacion') +
  theme_minimal()
ggplotly(boxplot_Rating)

# estadisticas descriptivas sin atipicos para la variable rating o puntuacion

summary(datos_sinatipicos)

descriptivas <- stat.desc(datos_sinatipicos$Rating)
descriptivas

options(scipen = 999999)

#Exploracion de datos para la variable rating o puntuacion
eda(Rating)



#INTERVALOS DE CONFIANZA

## Intervalo de confianza para la variable num_comp o numero de competidores
### Queremos ver dentro de que intervalos se encuentra la media de la variable numero de competidores
t.test(Salary_Prediction$num_comp, mu=1.081)
##Con un nivel de confianza del 5% podemos decir que el promedio de competidores por un puesto de trabajo en data science
##en Estados Unidos esta entre 0.9649991 y 1.1962912


## Intervalo de confianza para la variable avg_salary o salario promedio
### Queremos ver dentro de que intervalos se encuentra la media de la variable promedio
t.test(Salary_Prediction$avg_salary, mu=103.351)
## Podemos afirmar con un nivel de confianza del 5% que el salario promedio para un trabajo en el area de 
## data science en Estados Unidos esta entre 100.451 y 106.251 dolares

## Intervalo de confianza para la variable rating o puntuacion
### Queremos ver dentro de que intervalos se encuentra la media de la variable puntuacion
t.test(Salary_Prediction$Rating)
## Luego de realizar el intervalo de confianza, podemos afirmar con un nivel de confianza del 5% que el rating o la 
## puntuaciono promedioi de las empresas en data science en Estados Unidos se encuentra entre 
## 3.699533 y 3.796628
## 

##Intervalo de confianza para las personas que trabajan en el sur del pais
### Queremos saaber dentro de que intervalos se encuentra la media de personas que trabajan en data science en el sur
## de Estados Unidos
prop.test(139, 558, p = NULL,
          conf.level = 0.95, correct = FALSE, alternative = "two.sided")
#Codigo para Grafica de distribucion Z
# mean: media de la variable normal
# sd: desviación típica de la variable normal
# lb: límite inferior del área
# ub: límite superior del área
# acolor: color del área
# ...: argumentos adicionales para ser pasados a la función lines

normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...,z) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)

  
  arrows(0, 0.1, z, 0, lwd = 2, length = 0.2)
  text(0, 0.13, z, cex = 1.5)
  
}

#PRUEBAS DE HIPOTESIS

#HIPOTESIS PARA LA PROPORCION

#de personas en la muestra que trabajan como data analyst
## Queremos comprobar si la proporcion de personas que trabajan como data analyst dentro del area de 
## data science en Estados Unidos es menor que en la India

##Encontramos la proporcion de personas que trabajan como data analyst en India:
#Tabla Frecuencia Relativa
####
Frecuencia_absoluta<- table(Data_Science_Jobs_in_India$job_title)
Frecuencia_absoluta

## Ahora probamos que la proporcion en Estados Unidos es menor

prop.test(187, 1602, p = NULL,
          alternative = c("less"),
          conf.level = 0.95, correct = FALSE)

normal_area(mean = 0, sd = 1, lb = -3, ub = -1.96, lwd = 2, acolor = "lightblue",z=-30.68)


## Vemos como nuestro valor p es menor que nuestro alpha, por tanto, rechazamos h0, la proporcion de personas que
## trabajan como data analyst en el area de data science en Estados Unidos es menor que en la India

#HIPOTESIS PARA LA MEDIA DEL RATING:

# Deseamos comprobar si el rating promedio de las empresas en el area de data science en Estados Unidos
##ha cambiado con respecto al
## promedio en 2021

#Encontramos el promedio de rating para las empresas en data science en Estados Unidos:
promRating <- mean(data_cleaned_2021$Rating)
promRating

## Ahora comprobamos si ha cambiado
t.test(Salary_Prediction$Rating, 
       alternative = "two.sided", paired = FALSE, var.equal = TRUE,
       mu = 3.618868, conf.level = 0.95)

## Nuestro valor p es menor que el alpha utilizado, rechazamos h0 y concluimos que hay evidencia para afirmar que
## el rating de las empresas en data science en Estados Unidos ha cambiado con respecto al rating en el año 2021

#HIPOTESIS PARA LA RELACION DE MUESTRAS INDEPENDIENTES

#Deseamos saber si el salario de los trbajadores en data science son mayores en una empresa privada o en una empresa 
#publica

#Agrupamos los datos de los salarios para la compañia privada y Sacamos las variznas de la compañia privada:

salarios_compañia_Priv <- Salary_Prediction %>%
  filter(Type.of.ownership == "Company - Private") %>%
  select(avg_salary)

# Imprimir los salarios
print(salarios_compañia_Priv)

#Sacamos la varianza de los datos de los salarios de la compañia privada 
varianzaPriv <- var(salarios_compañia_Priv)
varianzaPriv

##Como sacar la desviacion:
salarios_compañia_Priv <- unlist(salarios_compañia_Priv)

# Calcular la desviación estándar de los salarios de la compañía A
#desviacion_compañia_Priv <- sd(salarios_compañia_Priv)

# Imprimir el resultado
#print(desviacion_compañia_Priv)

#Agrupamos los datos de los salarios para la compañia publica y Sacamos las varianzas de la compañia publica:

salarios_compañia_Pub <- Salary_Prediction %>%
  filter(Type.of.ownership == "Company - Public") %>%
  select(avg_salary)

# Imprimir los salarios
print(salarios_compañia_Pub)

#Sacamos la varianza de los datos de los salarios de la compañia privada 
varianzaPub <- var(salarios_compañia_Pub)
varianzaPub

##Como sacar la desviacion:
salarios_compañia_Pub <- unlist(salarios_compañia_Pub)

# Calcular la desviación estándar de los salarios de la compañía A
#salarios_compañia_Pub <- sd(salarios_compañia_Pub)

# Imprimir el resultado
#print(salarios_compañia_Pub)

x=salarios_compañia_Priv
y=salarios_compañia_Pub

#Hacemos prueba de varianza

var.test(salarios_compañia_Priv,salarios_compañia_Pub, alternative = "two.sided",conf.level = 0.95)

#Dado que el valor p es menor que el alpha utilizado del 5%, rechazamos h0 y asumimos varianzas diferentes

#En caso de varianzas distintas hacemos prueba T

t.test(salarios_compañia_Priv,salarios_compañia_Pub,
       alternative = "greater",
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

#Nuestro valor p es mayor que el alpha utilizado, no rechazamos h0, no hay suficiente evidencia para concluir que
# el salario en las empresas privadas es mayor

#Valor crítico t.
#qt (p = 0.95, df = 14960, lower.tail = TRUE)




#Crear nuevo dataframe con unicamente 3 tipos de compañia
# Filtrar las tres compañías de interés (A, B y C) utilizando la función subset()
df_typeCompany_vs_avgSalary<- subset(Salary_Prediction, Salary_Prediction$Type.of.ownership %in% c("Company - Private", "Company - Public", "Nonprofit Organization"))

#
#
#

#ANOVA 3 TIPOS DE COMPAÑIA vs SALARIO
company <-as.factor(df_typeCompany_vs_avgSalary$Type.of.ownership)
boxplot(df_typeCompany_vs_avgSalary$avg_salary~df_typeCompany_vs_avgSalary$Type.of.ownership)
tapply(df_typeCompany_vs_avgSalary$avg_salary,df_typeCompany_vs_avgSalary$Type.of.ownership, mean)
anova<-aov(lm(df_typeCompany_vs_avgSalary$avg_salary~company))
summary(anova)
TukeyHSD(anova)


#ANOVA 3 TIPOS DE COMPAÑIA vs RATING
company <-as.factor(df_typeCompany_vs_avgSalary$Type.of.ownership)
boxplot(df_typeCompany_vs_avgSalary$Rating~df_typeCompany_vs_avgSalary$Type.of.ownership)
tapply(df_typeCompany_vs_avgSalary$Rating,df_typeCompany_vs_avgSalary$Type.of.ownership, mean)
anova<-aov(lm(df_typeCompany_vs_avgSalary$Rating~company))
summary(anova)
TukeyHSD(anova)

#
#
#

#CHI CUADRADO REGION vs RATING (Intervalos)
resultado_Region_vs_Rating <- chisq.test(Salary_Prediction$intervalos, Salary_Prediction$job_region)
print(resultado_Region_vs_Rating)

#CHI CUADRADO REGION VS SALARIO PROMEDIO (intervalos)
resultado_Region_avg_Salary <- chisq.test(Salary_Prediction$avg_salary_Intervalos, Salary_Prediction$job_region)
print(resultado_Region_avg_Salary)

#REGRESION RATING vs SALARIO
#Formulación de hipótesis:
#probar si hay relación entre el salario y el Rating
attach(Salary_Prediction)
#Grafico de Dispersion
plot(Rating,num_comp)
#Test de Correlacion
cor.test(Rating,num_comp)
qt (p = 0.95, df = 551, lower.tail = TRUE)
#Prueba de Linealidad
Regresion <- lm(Rating ~ num_comp)
summary(Regresion)
#Prueba de homoscedasticidad.
bptest(Regresion)
#Prueba de Normalidad
Residuos<-residuals(Regresion)
shapiro.test(Residuos)
#Prueba de Independencia
dwtest(Regresion)
#Intervalo de Prediccion
Y_estim<-predict(Regresion,list(area=10), interval = "confidence")
Y_estim
#Intervalo de Confianza
Y_estim2<-predict(Regresion,list(area=10), interval = "confidence",level = 0.95)
Y_estim2


