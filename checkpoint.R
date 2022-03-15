setwd("C:/Users/user/Google Drive/Clases/BEDU/Data science/2 - R/CHECKPOINT/WD")
RAWDATA <- read.csv("SeoulBikeData.csv")
View(RAWDATA) #vemos la tabla del dataset
# dataset descargado de: https://www.kaggle.com/saurabhshahane/seoul-bike-sharing-demand-prediction
# Este dataset agrupa información diaria de condiciones de tiempo atmosférico de renta de bicicletas e intenta conocer la incidencia que tiene las condiciones del tiempo atmosférico en la demanda del servicio 

#La estructura del dataset es la siguiente:
str(RAWDATA) #la variable fecha no se eencuentra en el formato correcto para que sean interpretadas por R
dim(RAWDATA) #el dataset cuenta con 8760 observaciones en la muestra y 14 variables
tail(RAWDATA)
summary(RAWDATA)

DATA <- RAWDATA #creamos un espejo del dataset para poder trabajar en él

library(dplyr) #se llama la librería dplyr para poder manipular los datos

DATA <- mutate(RAWDATA, Date = as.Date(Date, format = "%d/%m/%Y"))
str(DATA)
summary(DATA$Date)#ahora el summary nos indica que este dataset se generó desde el 1 de dic del 2017 hasta el 30 de Noviembre del 2018


#a continuación podemos ver el desarrollo de rentas de bicicletas en el tiempo
plot(x=DATA$Date, y = DATA$Rented.Bike.Count, ylab = "Bicicletas rentadas diarias", xlab = "Tiempo", main = "Cantidad de bicicletas rentadas en el tiempo", 
     sub = "desde el 1/12/2017 al 30/11/2018")

#
library(ggplot2) # llamamos la biblioteca ggplot2 para tener más personalización de gráficos

names(DATA) 

#acá podemos observar la demanda del servicio en las diferentes horas del día durante las estaciones del año 
ggplot(DATA, aes(x=Hour, y = Rented.Bike.Count)) +
  labs(x = "Horas del día", y = "Conteo de Bicicletas rentadas",
    title ="Bicicletas rentadas en diferentes estaciones del año",
    subtitle = "renta de bicicletas por hora",
    caption = "fuente: https://www.kaggle.com/hardikjain10/seoul-bike-rented",
    alt = "Add alt text to the plot") +
  geom_point() +   
  theme_gray() +
  facet_wrap("Seasons")

#a continuación haremos una prueba gráfica para verificar la calidad de la información, comparando la radiación solar y la temperatura diaria en las diferentes estaciones anuales para demostrar que los datos son coherentes
#acá podemos observar una clara 
ggplot(DATA, aes(x=Temperature..C., y = log(Solar.Radiation..MJ.m2.))) + #se agregó logaritmo para visualizar la dispersión
  labs(x = "Temperatura °C", y = "Log de Radiación Solar MJm2",
       title ="Bicicletas rentadas en diferentes estaciones del año",
       subtitle = "renta de bicicletas por hora",
       caption = "fuente: https://www.kaggle.com/hardikjain10/seoul-bike-rented",
       alt = "Add alt text to the plot") +
  geom_bin2d(binwidth = c(1, 0.1)) +
  scale_fill_gradient(low="green", high = "red") +
  theme_gray() +
  facet_wrap("Seasons")
  

# como es de esperarse, las temperaturas más altas se ecuentran en verano y las más bajas en invierto, otoño y primavera tienen temperaturas similares
# también podemos observar cierta relación de la variable log(Solar.Radiation..MJ.m2.) con la temperatura

#verificamos que no exista ningún dato de renta de bicicleta en los días no laborados
ggplot(DATA, aes(x=Hour, y = Rented.Bike.Count )) + 
  scale_fill_distiller(palette = "Blues") +
  geom_col(position = "dodge") +
  theme_gray() +
  facet_wrap("Functioning.Day")
#

#creamos un mirror del DATA para hacer análisis con gráficas de ggplot
DATA1 <- DATA

#cambiamos algunas variables continuas a categóricas para podelas agrupar
for(i in 1:nrow(DATA) ) {
  DATA1$llueve[i] <- if(DATA$Rainfall.mm.[i]>0){"llueve"}else{"no llueve"} #si llueve = 1, si no, 0
  DATA1$neva[i] <- if(DATA$ Snowfall..cm.[i]>0){"neva"}else {"no neva"}#si neva = 1, si no, 0
  DATA1$radiación[i] <- if(DATA$ Solar.Radiation..MJ.m2.[i]>0.3){"Alta radiación"}else{"Baja radiación"} #si hay radiación solar = 1, si no, 0
  DATA1$temp[i] <- if(DATA$ Temperature..C.[i]>20){"Mayor a 20°C"}else{if(DATA$ Temperature..C.[i]>5){"De 5°C a 20°C"}else{"Menor a 5°C"}}
}
DATA1 <- mutate(DATA1, llueve = as.factor(llueve))
DATA1 <- mutate(DATA1, neva = as.factor(neva))
DATA1 <- mutate(DATA1, radiación = as.factor(radiación))
DATA1 <- mutate(DATA1, temp = as.factor(temp))

str(DATA1)

#graficamos con los agrupamientos anteriores


#en esta gráfica podemos observar una diferencia notable entre la precipitación y el uso del servicio
ggplot(DATA1, aes(x=Hour, y = Rented.Bike.Count, fill = llueve)) + 
  scale_fill_manual(values = c("blue",  "orange")) +
  geom_bar(stat="identity",  position = "dodge")+
  labs(x = "Horas", y = "Bicicletas rentadas",
    title ="Bicicletas rentadas cuando llueve y cuando no llueve",
    caption = "fuente: https://www.kaggle.com/hardikjain10/seoul-bike-rented")

# también es posible ver una notable diferencia en días nevados y días no nevados
ggplot(DATA1, aes(x=Hour, y = Rented.Bike.Count, fill = neva)) + 
  scale_fill_manual(values = c("skyblue",  "gray")) +
  geom_bar(stat="identity",  position = "dodge")+
  labs(x = "Horas", y = "Bicicletas rentadas",
       title ="Bicicletas rentadas cuando neva y cuando no neva",
       caption = "fuente: https://www.kaggle.com/hardikjain10/seoul-bike-rented") 

# tambien pudiera ser afectada la variable dependiente con respecto a la radiación solar
ggplot(DATA1, aes(x=Hour, y = Rented.Bike.Count, fill = radiación)) + 
  scale_fill_manual(values = c("red",  "skyblue")) +
  scale_fill_discrete(labels=c("Alta radiación solar (mayor a 0.3 MJ por m2)","Baja radiación solar (menor a 0.3 MJ por m2)"))+
  geom_bar(stat="identity",  position = "dodge")+
  labs(x = "Horas", y = "Bicicletas rentadas",
       title ="Bicicletas rentadas cuando hay o no radiación solar",
       caption = "fuente: https://www.kaggle.com/hardikjain10/seoul-bike-rented") 

#también se aprecia la afectación del frío a la variable dependiente
ggplot(DATA1, aes(x=Hour, y = Rented.Bike.Count, fill = temp)) + 
  scale_fill_manual(values = c("green", "orange","blue")) +
  geom_bar(stat="identity",  position = "dodge") +
  labs(x = "Horas", y = "Bicicletas rentadas",
       title ="Bicicletas rentadas en relación a la temperatura del día",
       caption = "fuente: https://www.kaggle.com/hardikjain10/seoul-bike-rented") 

#en este apartado, creamos un mirror del dataset pero solo mantenemos variables contínuas para realizarle rápidamente una revisión de correlaciones de pearson
DATAnumeric <- select(DATA, -Date, -Seasons,  -Holiday, -Functioning.Day)
attach(DATAnumeric)
pairs(DATAnumeric) #por lo visto no se puede apreciar ninguna variable con una correlación visible, salvo por Temperature..C. y Dew.point.temperature..C. (punto de condensación) "
CORtable <- cor(DATAnumeric)
View(CORtable)

str(DATA)
#cambiamos las variables categóricas a "factors"
DATA <- mutate(DATA, Seasons = as.factor(Seasons))
DATA <- mutate(DATA, Holiday = as.factor(Holiday))
DATA <- mutate(DATA, Functioning.Day = as.factor(Functioning.Day))

#procedemos a realizar la regresión lineal
attach(DATA)
DATAreg1 <- lm(Rented.Bike.Count~Hour+Temperature..C.+Humidity...+Wind.speed..m.s.+Visibility..10m.+Dew.point.temperature..C.+Solar.Radiation..MJ.m2.+Rainfall.mm.+Snowfall..cm.+Seasons+Holiday)
summary(DATAreg1)
#se observa que el pvalue implica que el modelo es muy significativo, sin embargo la Rcuadrada nos da 0.4876
#las variables de visibilidad, la temperatura de condensación y la velocidad del viento tienen bajo pvalue, por lo que se hace una segunda regresión esperand un aumento del Rcuadrada
DATAreg2 <- lm(Rented.Bike.Count~Hour+Temperature..C.+Humidity...+Solar.Radiation..MJ.m2.+Rainfall.mm.+Snowfall..cm.+Seasons+Holiday)
summary(DATAreg2)

#a pesar de quitar las variables con menor pvalue, la rcuadrada no mejoró, mas bien, disminuyó
#por lo tanto se procede a hacer un SVM para generar el mejor modelo


#primero creamos una muestra de un cuarto de los datos actuales llamada "train"
set.seed(666)
learn = sample(nrow(DATA),round(nrow(DATA)/4))
tail(DATA[learn, ])

#observemos la dispersión de la muestra con el conjunto de entrenamiento y el conjunto de prueba
ggplot(DATA[learn, ], 
       aes(x = Rented.Bike.Count, y = Temperature..C., colour = Hour)) + 
  geom_point() + facet_wrap('Seasons') + 
  theme_dark() + ggtitle("Conjunto de entrenamiento")
ggplot(DATA[-learn, ], 
       aes(x = Rented.Bike.Count, y = Temperature..C., colour = Hour)) + 
  geom_point() + facet_wrap('Seasons') + 
  theme_light() + ggtitle("Conjunto de prueba")

#Modificamos el dataset para ponerlo listo para la  SVM, eliminando la información de la fecha y las variables menos significativas  (Visibility..10m. y Dew.point.temperature..C.) de la primera regresión
#esto es para reducir el número de variables y que el cálculo sea más eficiente
DATAsvm <- select(DATA, -Date, -Visibility..10m., -Dew.point.temperature..C.)

library(e1071)

# Ahora utilizamos la función `tune` junto con la función `svm` para 
# seleccionar el mejor modelo de un conjunto de modelos, los modelos 
# considerados son aquellos obtenidos al variar los valores de los 
# parámetros `cost` y `gamma`. Kernel Radial

tune.rad = tune(svm, Rented.Bike.Count~., data = DATAsvm[learn,], 
                kernel = "radial", 
                ranges = list(
                  cost = c(0.1, 1, 10, 100, 1000), 
                  gamma = seq(0.01, 10, 0.8)
                ) 
)
#interacciones
summary(tune.rad)
#mejor modelo de éstas
summary(tune.rad$best.model)

# A continuación solo usamos los valores de cost y gamma que producen el menor error de prueba estimado, considerando los conjuntos de valores en el código anterior

best <- svm(Rented.Bike.Count~.,  data = DATAsvm[learn,],
            kernel = "radial",
            cost = 1,
            gamma = 0.81
)
# Con el mejor modelo seleccionado y utilizando el conjunto de prueba, obtengamos una matriz de confusión, para observar el número de aciertos y errores cometidos por el modelo. También obtengamos la proporción total de aciertos y la matriz que muestre las proporciones de aciertos y errores cometidos pero por categorías.

mc <- table(true = DATAsvm[-learn, "Rented.Bike.Count"], 
            pred = predict(best, 
                           newdata = DATAsvm[-learn,]))
View(mc)
summary(best)

table(DATAsvm[learn,"Rented.Bike.Count"], fitted(best), dnn = c("Actual", "Predicho"))
