# Análisis de deportes de resistencia

#librerias
```{r librerias}
library(tidyverse)
library(tidymodels)
library(pROC)
library(e1071)
library(dplyr)
library(cluster)
```

## Cargar datos
el data frame consta de 167615 datos con 17 variables los cuales seran almacenados en data.
  ```{r cargando datos}
  getwd()
  setwd("D:/U/mineria de datos")
data <- readRDS("endurance.rds")
summary(data)
```

## Limpieza de datos
Antes de evaluar los datos hay que hacer una limpieza de estos.eliminaremos las variables id,athlete, device_name, Start_date_local, Records y has_heartrate 

```{r limpiando datos}

data$id <- NULL
data$athlete <- NULL
data$device_name <- NULL
data$start_date_local <- NULL
data$records <- NULL
data$has_heartrate <- NULL
```

existen variables tipo character que hay que cambiar a numéricas.utilizaremos un buleano para definir como 1 las actividades en bicicleta y 0 las que son a pie.

```{r pre procesamiento}
data$elev_low <- as.numeric(data$elev_low)
data$elev_high <- as.numeric(data$elev_high)
data$max_speed <- as.numeric(data$max_speed)
data$average_speed <- as.numeric(data$average_speed)
data$type_code <- (data$type == "Ride" | data$type == "EBikeRide") %>% as.numeric()
data$type <- NULL
```

a continuación buscaremos los NA
```{r NAs}
# Para las observaciones incompletas, le asignamos el valor NA para eliminarlos en el siguiente paso
data[data == ""] <- NA

# Verificamos donde hay valores NAs
data %>% 
  summarise_all(funs(sum(is.na(.))))

# Eliminamos todas las observaciones que presenten NA
data_pre <- data %>% 
  filter(!(is.na(calories)|is.na(elev_low)|is.na(elev_high)|is.na(total_elevation_gain)))

# Corroboramos que no queden datos NA
data_pre %>% 
  summarise_all(funs(sum(is.na(.))))

```
Ademas se puede ver que existen entradas en distance con valores = 0 e igual lo haremos con los demas datos
```{r eliminar distancias 0}
data_pre$distance[data_pre$distance == 0] <- NA
data_pre$calories[data_pre$calories == 0] <- NA
data_pre$elev_high[data_pre$elev_high == 0] <- NA
data_pre$elev_low[data_pre$elev_low == 0] <- NA
data_pre$total_elevation_gain[data_pre$total_elevation_gain == 0] <- NA
data_pre$moving_time[data_pre$moving_time == 0] <- NA
data_pre$elapsed_time[data_pre$elapsed_time == 0] <- NA
data_pre$average_speed[data_pre$average_speed == 0] <- NA

data_pre <- data_pre %>% filter(!(is.na(distance)|is.na(calories)|is.na(elev_high)|is.na(elev_low)|is.na(total_elevation_gain)|is.na(moving_time)|is.na(elapsed_time)|is.na(average_speed)|is.na(type_code)))

```


como sabemos que existen datos atipicos realizaremos boxplot para cada una de las variables.
```{r boxplot }
calories=boxplot(data$calories, horizontal = TRUE)
distance=boxplot(data$distance, horizontal = TRUE)  
elev_low=boxplot(data$elev_low, horizontal = TRUE)
elev_high=boxplot(data$elev_high, horizontal = TRUE)
moving_time=boxplot(data$moving_time, horizontal = TRUE)
max_speed=boxplot(data$max_speed, horizontal = TRUE)
elapsed_time=boxplot(data$elapsed_time, horizontal = TRUE)
average_speed=boxplot(data$average_speed, horizontal = TRUE)
total_elevation_gain=boxplot(data$total_elevation_gain, horizontal = TRUE)
```

Los gráficos de boxplot comprueban lo mencionado acerca de la presencia de datos atípicos en cada una de las variables, por lo que se aplicarán filtros a cada una de estas para así eliminar las data que sean anormales.  

```{r eliminando datos atipicos}
data_pre <- filter(data ,data$calories < 2000)
data_pre <- filter(data ,data$distance < 50000)
data_pre <- filter(data ,data$elev_low < 2000)
data_pre <- filter(data ,data$elev_low > -1000)
data_pre <- filter(data ,data$elev_high < 5000)
data_pre <- filter(data ,data$moving_time < 25000)
data_pre <- filter(data ,data$max_speed < 60)
data_pre <- filter(data ,data$elapsed_time < 15000)
data_pre <- filter(data ,data$average_speed < 30)
data_pre <- filter(data ,data$total_elevation_gain < 2000)
```
borramos ...... 
#escalamiento
```{r escalamiento}
set.seed(500)
library(rsample)
data_scal <-data.frame(scale(data_pre[0:9]))
data_scal <- cbind(data_scal,data_pre[10])
data_scal$type_code <- data_scal$type_code %>% as.factor()
datasplit <- initial_split(data_scal,prop= 0.8, strata = NULL)
data_train <- training(datasplit)
data_test <- testing(datasplit)
data_train2 <- training(datasplit)
data_test2 <- testing(datasplit)
```
#regresion multiple
utilizaremos una regresion multiple para ver cuales son las variables mas relevantes.
```{r reg}
library(regclass)
reg_mult <- lm(type_code %>% as.numeric() ~ calories + distance + elev_low +
                 elev_high + max_speed + moving_time + elapsed_time +
                 average_speed + total_elevation_gain, data = data_scal)
summary(reg_mult)
VIF(reg_mult)
```
los resultados de la regresion multiple nos muestra un coeficiente de determinacion de un 40% y se puede ver que existen variables que no son tan significativas
como elev_low, elev_hig, elapsed_time

# Modelo Naive Bayes
utilizaremos el método Naive Bayes
```{r naive bayes modelo}

DP_model <-  naiveBayes(type_code ~calories + distance + elev_low +
                          elev_high + max_speed + moving_time + elapsed_time +
                          average_speed + total_elevation_gain,
                        data = data_train)
```

Luego corresponde evaluar el modelo, obtener su curva ROC y posteriormente su AUC.

```{r naive bayes predictions}
PredictionModel <- predict(DP_model, newdata = data_test, type = "class") 
data_test$Predclass <- PredictionModel
curvaROC <- roc(data_test$type_code %>% as.numeric(), data_test$Predclass %>% as.numeric())
plot(curvaROC)
```

```{r naive bayes roc auc}
auc(curvaROC)
data_test %>% 
  conf_mat(type_code, Predclass) %>% 
  autoplot(type = "heatmap")
```
el modelo nos entrega un AUC bastante bajo con un valor de de 74.29%
por esto buscaremos otro modelo

#arbol de decisión
```{r receta}
receta <- 
  recipe(type_code ~ ., data = data_train2)
receta
```
ahora procedemos a crear nuestro modelo de arbol de decision con 5 capas de  decision, y un minimo numero de entides por hoja de 10
```{r modelo_trees}
library(rpart)
modelo_tree <-  decision_tree(tree_depth = 5, min_n = 10) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")
modelo_tree
```

```{r AUC modelo arbol de decisión}
fit_mod <- function(mod){
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(receta) %>% 
    fit(data = data_train2)
  
  model_pred <- 
    predict(modelo_fit, data_test2, type = "prob") %>% 
    bind_cols(data_test2)
  
  return(model_pred %>% 
           roc_auc(truth = type_code, .pred_0))
}

fit_mod(modelo_tree)
  
```
el modelo nos entrega un AUC bastante alto con un valor de de 92.2% por lo que lo utilizaremos para extraer nuestra data. de esto extraeremos la data que nos explicara la probabilidad de que pertenezca al grupo 0 o 1

```{r extraccion de la data del arbol de decision}
modelo_fit <- 
  workflow() %>% 
  add_model(modelo_tree) %>% 
  add_recipe(receta) %>% 
  fit(data = data_train2)
model_pred <- 
  predict(modelo_fit, data_test2, type = "prob") %>% 
  bind_cols(data_test2)

```
al ya tener asignada las probabilidades de que la actividad pertenezca a cada grupo veremos a cual grupo corresonde esto comparando las probabilidades dejando la mayor entre 0 y 1 en su grupo correspondiente
```{r asignacion categoria }
data_test2$prediccion <- ifelse(model_pred$.pred_0 >= model_pred$.pred_1, 0, 1)

```
```{r seleccion de data mal clasificada}
data_error <- data_test2 %>% filter(type_code != prediccion)
nrow(data_error)
```
nos dieron un total de 2284 actividades que estaban mal etiquetadas segun nuestro modelo.