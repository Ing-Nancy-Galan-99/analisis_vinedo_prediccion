# Cargar las librerías necesarias
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ROSE)

# Cargar los datos
datos <- read.csv("C:\\datos_vinedo_tratados1.csv")
print(summary(datos))

# Crear variable categórica de calidad basada en la mediana
mediana_calidad <- median(datos$Calidad)
print(paste("Mediana de calidad:", mediana_calidad))

# Crear variable categórica: Alta (>= mediana) y Baja (< mediana)
datos$Calidad_Cat <- ifelse(datos$Calidad >= mediana_calidad, "Alta", "Baja")
datos$Calidad_Cat <- as.factor(datos$Calidad_Cat)

# Verificar la distribución
print("Distribución de calidad categórica:")
print(table(datos$Calidad_Cat))

# Seleccionar variables predictoras relevantes
variables_predictoras <- c("Temperatura", "Lluvia", "pH", "Poda", "Plaga", 
                           "GradoBrix", "ConteoRacimos", "PesoRacimos", 
                           "Rendimiento", "ciclo_Cuajado", "ciclo_Envero", 
                           "Ciclo_Meseta", "vino_Merlot", "vino_Cabernet")

# Dividir datos
set.seed(123)
trainIndex <- createDataPartition(datos$Calidad_Cat, p = 0.7, list = FALSE)
train_data <- datos[trainIndex, ]
test_data  <- datos[-trainIndex, ]

# Balancear entrenamiento con ROSE
set.seed(123)
train_data_bal <- ROSE(Calidad_Cat ~ ., data = train_data, seed = 123)$data

# Modelo Random Forest
modelo_rf <- randomForest(Calidad_Cat ~ ., data = train_data_bal, ntree = 500, mtry = 3, importance = TRUE)

# Matriz de confusión
predicciones <- predict(modelo_rf, test_data)
matriz_confusion <- confusionMatrix(predicciones, test_data$Calidad_Cat)
print(matriz_confusion)

print("Modelo Random Forest entrenado:")
print(modelo_rf)

# Métricas adicionales
accuracy <- matriz_confusion$overall['Accuracy']
precision_alta <- matriz_confusion$byClass['Pos Pred Value']
recall_alta <- matriz_confusion$byClass['Sensitivity']
f1_score <- 2 * (precision_alta * recall_alta) / (precision_alta + recall_alta)

cat("\nMétricas de Rendimiento:\n")
cat("Exactitud (Accuracy):", round(accuracy, 4), "\n")
cat("Precisión para Calidad Alta:", round(precision_alta, 4), "\n")
cat("Recall para Calidad Alta:", round(recall_alta, 4), "\n")
cat("F1-Score:", round(f1_score, 4), "\n")

cat("Datos de entrenamiento:", nrow(train_data_bal), "\n")
cat("Datos de prueba:", nrow(test_data), "\n")

# Importancia de variables
importancia <- importance(modelo_rf)
importancia_df <- data.frame(
  Variable = rownames(importancia),
  MeanDecreaseAccuracy = importancia[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importancia[, "MeanDecreaseGini"]
)
importancia_df <- importancia_df[order(importancia_df$MeanDecreaseAccuracy, decreasing = TRUE), ]

cat("\nImportancia de Variables (Top 10):\n")
print(head(importancia_df, 10))

# Gráfico de importancia
p1 <- ggplot(head(importancia_df, 10), aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de Variables en Random Forest",
       x = "Variables",
       y = "Mean Decrease Accuracy") +
  theme_minimal()
print(p1)

# Proyección fin de año
parcelas_unicas <- unique(datos$id_parcela)
cat("Número de parcelas únicas:", length(parcelas_unicas), "\n")

set.seed(123)
datos_proyectados <- data.frame()

for(parcela in parcelas_unicas) {
  datos_parcela <- datos[datos$id_parcela == parcela, ]
  
  temp_promedio <- mean(datos_parcela$Temperatura)
  lluvia_promedio <- mean(datos_parcela$Lluvia)
  ph_promedio <- mean(datos_parcela$pH)
  
  proyeccion <- data.frame(
    id_parcela = parcela,
    Temperatura = temp_promedio * runif(1, 0.8, 1.1),
    Lluvia = lluvia_promedio * runif(1, 1.2, 1.8),
    pH = ph_promedio + runif(1, -0.2, 0.2),
    Poda = 1,
    Plaga = rbinom(1, 1, 0.15),
    GradoBrix = runif(1, 12, 18),
    ConteoRacimos = round(mean(datos_parcela$ConteoRacimos) * runif(1, 0.9, 1.1)),
    PesoRacimos = mean(datos_parcela$PesoRacimos) * runif(1, 0.8, 1.2),
    Rendimiento = mean(datos_parcela$Rendimiento) * runif(1, 0.85, 1.15),
    ciclo_Cuajado = 0,
    ciclo_Envero = 0,
    Ciclo_Meseta = 1,
    vino_Merlot = ifelse(any(datos_parcela$vino_Merlot == 1), 1, 0),
    vino_Cabernet = ifelse(any(datos_parcela$vino_Cabernet == 1), 1, 0)
  )
  
  datos_proyectados <- rbind(datos_proyectados, proyeccion)
}

cat("Primeras filas de datos proyectados:\n")
print(head(datos_proyectados))

#Con variables como Poda, Plaga, Temperatura, se entrenara un árbol de decisión 
#que prediga si una parcela va a ser de "alta calidad"

library(rpart)
library(rpart.plot)

variables_arbol <- c("Poda", "Plaga", "Temperatura")
#datos para el árbol: sólo variables seleccionadas + target
datos_arbol <- train_data_bal[, c(variables_arbol, "Calidad_Cat")]

# Entrenar árbol de decisión
set.seed(123)
arbol_modelo <- rpart(Calidad_Cat ~ ., data = datos_arbol, method = "class", 
                      control = rpart.control(cp = 0.01))  # cp controla complejidad

printcp(arbol_modelo)

# Visualizar árbol
rpart.plot(arbol_modelo, main = "Árbol de Decisión para Calidad de Parcela", 
           extra = 106, box.palette = "Blues", shadow.col = "gray")


# Predecir en test (seleccionando variables)
test_arbol <- test_data[, c(variables_arbol, "Calidad_Cat")]
pred_arbol <- predict(arbol_modelo, test_arbol, type = "class")

# Matriz de confusión y métricas
conf_mat_arbol <- confusionMatrix(pred_arbol, test_arbol$Calidad_Cat)
print(conf_mat_arbol)


cat("\nAccuracy árbol de decisión:", round(conf_mat_arbol$overall["Accuracy"], 4), "\n")

#se agregan variables no tomada encuenta en el modelo 
datos_proyectados$Dia <- median(datos$Dia, na.rm = TRUE)
datos_proyectados$Mes <- median(datos$Mes, na.rm = TRUE)
datos_proyectados$Año <- median(datos$Año, na.rm = TRUE)
datos_proyectados$Calidad <- median(datos$Calidad, na.rm = TRUE)

# Predecir calidad en datos proyectados con tu modelo Random Forest
predicciones_proyeccion <- predict(modelo_rf, datos_proyectados)

# Calcular el porcentaje de parcelas con calidad Alta
porcentaje_alta <- mean(predicciones_proyeccion == "Alta") * 100

cat("Porcentaje estimado de parcelas con calidad alta al fin de año:", round(porcentaje_alta, 2), "%\n")

# Hacer predicciones sobre los datos proyectados
predicciones_proyectadas <- predict(modelo_rf, datos_proyectados, type = "response")
probabilidades_proyectadas <- predict(modelo_rf, datos_proyectados, type = "prob")

# Crear dataframe con resultados
resultados_prediccion <- data.frame(
  id_parcela = datos_proyectados$id_parcela,
  Prediccion_Calidad = predicciones_proyectadas,
  Prob_Alta = probabilidades_proyectadas[, "Alta"],
  Prob_Baja = probabilidades_proyectadas[, "Baja"],
  Temperatura = round(datos_proyectados$Temperatura, 2),
  Lluvia = round(datos_proyectados$Lluvia, 2),
  pH = round(datos_proyectados$pH, 2),
  GradoBrix = round(datos_proyectados$GradoBrix, 2),
  Rendimiento = round(datos_proyectados$Rendimiento, 2),
  Tipo_Vino = ifelse(datos_proyectados$vino_Merlot == 1, "Merlot", "Cabernet")
)

print("PREDICCIONES PARA FIN DE AÑO - TODAS LAS PARCELAS:")
print(resultados_prediccion)

# Resumen estadístico de las predicciones
resumen_predicciones <- table(resultados_prediccion$Prediccion_Calidad)
print("RESUMEN DE PREDICCIONES:")
print(resumen_predicciones)

# Estadísticas por tipo de vino
print("\
PREDICCIONES POR TIPO DE VINO:")
tabla_vino <- table(resultados_prediccion$Tipo_Vino, resultados_prediccion$Prediccion_Calidad)
print(tabla_vino)

# Parcelas con mayor probabilidad de calidad alta
parcelas_alta_prob <- resultados_prediccion[order(resultados_prediccion$Prob_Alta, decreasing = TRUE), ]
print("\
TOP 5 PARCELAS CON MAYOR PROBABILIDAD DE CALIDAD ALTA:")
print(head(parcelas_alta_prob[, c("id_parcela", "Prediccion_Calidad", "Prob_Alta", "Tipo_Vino")], 5))

# Parcelas con mayor riesgo (baja calidad)
parcelas_riesgo <- resultados_prediccion[order(resultados_prediccion$Prob_Baja, decreasing = TRUE), ]
print("\
TOP 5 PARCELAS CON MAYOR RIESGO (CALIDAD BAJA):")
print(head(parcelas_riesgo[, c("id_parcela", "Prediccion_Calidad", "Prob_Baja", "Tipo_Vino")], 5))

# Crear visualizaciones del informe
library(gridExtra)

# Gráfico 1: Distribución de predicciones
p1 <- ggplot(resultados_prediccion, aes(x = factor(Prediccion_Calidad, labels = c("Alta", "Baja")), fill = factor(Prediccion_Calidad))) +
  geom_bar() +
  labs(title = "Distribución de Predicciones de Calidad",
       x = "Calidad Predicha",
       y = "Número de Parcelas") +
  scale_fill_manual(values = c("1" = "green", "2" = "red"), name = "Calidad") +
  theme_minimal()

# Gráfico 2: Probabilidades por parcela
p2 <- ggplot(resultados_prediccion, aes(x = factor(id_parcela), y = Prob_Alta, fill = Tipo_Vino)) +
  geom_bar(stat = "identity") +
  labs(title = "Probabilidad de Calidad Alta por Parcela",
       x = "ID Parcela",
       y = "Probabilidad de Calidad Alta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Relación Temperatura vs Probabilidad
p3 <- ggplot(resultados_prediccion, aes(x = Temperatura, y = Prob_Alta, color = Tipo_Vino)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperatura vs Probabilidad de Calidad Alta",
       x = "Temperatura (°C)",
       y = "Probabilidad de Calidad Alta") +
  theme_minimal()

# Mostrar gráficos
print(p1)
print(p2)
print(p3)

print("   PARCELAS DE RIESGO (requieren intervención):")
parcelas_risk <- head(parcelas_riesgo, 3)
for(i in 1:nrow(parcelas_risk)) {
  print(paste("   - Parcela", parcelas_risk$id_parcela[i], ":", parcelas_risk$Tipo_Vino[i], 
              "- Prob. Baja:", round(parcelas_risk$Prob_Baja[i], 3)))
}


#tendencias

# Crear gráfico de tendencias de predicciones
library(ggplot2)
library(dplyr)

# Agrupar predicciones por parcela y calcular tendencias
tendencias <- resultados_prediccion %>%
  group_by(id_parcela, Tipo_Vino) %>%
  summarise(
    Prob_Alta_Media = mean(Prob_Alta),
    Prediccion_Final = ifelse(Prob_Alta_Media > 0.5, "Alta", "Baja"),
    .groups = 'drop'
  ) %>%
  arrange(Prob_Alta_Media)

# Gráfico de tendencias
p_tendencia <- ggplot(tendencias, aes(x = reorder(factor(id_parcela), Prob_Alta_Media), 
                                      y = Prob_Alta_Media, 
                                      fill = Tipo_Vino,
                                      color = Prediccion_Final)) +
  geom_col(alpha = 0.7, size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("Merlot" = "#8B0000", "Cabernet" = "#4B0082")) +
  scale_color_manual(values = c("Alta" = "darkgreen", "Baja" = "darkred")) +
  labs(title = "Tendencia de Predicciones de Calidad por Parcela",
       subtitle = "Línea roja indica umbral de calidad (0.5)",
       x = "ID Parcela (ordenado por probabilidad)",
       y = "Probabilidad de Calidad Alta",
       fill = "Tipo de Vino",
       color = "Predicción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom")

print(p_tendencia)

# Resumen de tendencias
print("\
=== RESUMEN DE TENDENCIAS ===")
print(paste("Parcelas con tendencia ALTA:", sum(tendencias$Prediccion_Final == "Alta")))
print(paste("Parcelas con tendencia BAJA:", sum(tendencias$Prediccion_Final == "Baja")))
print(paste("Probabilidad promedio Merlot:", round(mean(tendencias$Prob_Alta_Media[tendencias$Tipo_Vino == "Merlot"]), 3)))
print(paste("Probabilidad promedio Cabernet:", round(mean(tendencias$Prob_Alta_Media[tendencias$Tipo_Vino == "Cabernet"]), 3)))

# Verificar qué datos de predicciones tenemos disponibles
print("Variables con 'prediccion' en el nombre:")
prediccion_vars <- ls()[grepl("prediccion", ls(), ignore.case = TRUE)]
print(prediccion_vars)

# Verificar si tenemos datos_proyectados
if(exists("datos_proyectados")) {
  print("\
Estructura de datos_proyectados:")
  print(head(datos_proyectados))
}


# Verificar todas las variables disponibles y crear gráfico con los datos que tenemos
print("Variables disponibles:")
print(ls())

  
#analisis de tendencia
  
  # Crear variable categórica de calidad
  mediana_calidad <- median(datos$Calidad)
  datos$Calidad_Cat <- ifelse(datos$Calidad > mediana_calidad, "Alta", "Baja")
  
  # Calcular tendencia de calidad por parcela a lo largo del tiempo
  tendencias_parcela <- datos %>%
    group_by(id_parcela, Año, Mes) %>%
    summarise(
      Calidad_Promedio = mean(Calidad),
      Temperatura_Promedio = mean(Temperatura),
      .groups = 'drop'
    ) %>%
    arrange(id_parcela, Año, Mes)
  
  # Crear gráfico de tendencias
  p_tendencias <- ggplot(tendencias_parcela, aes(x = interaction(Año, Mes), y = Calidad_Promedio, 
                                                 group = id_parcela, color = factor(id_parcela))) +
    geom_line(alpha = 0.7, size = 0.8) +
    geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "red", size = 1.5) +
    geom_hline(yintercept = mediana_calidad, linetype = "dashed", color = "blue", size = 1) +
    labs(title = "Tendencias de Calidad por Parcela a lo Largo del Tiempo",
         subtitle = "Línea roja: tendencia general | Línea azul: mediana de calidad",
         x = "Período (Año.Mes)",
         y = "Calidad Promedio",
         color = "ID Parcela") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none") +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3)])
  
  print(p_tendencias)
  
  # Resumen de tendencias
  print("\
=== RESUMEN DE TENDENCIAS ===")
  print(paste("Mediana de calidad:", round(mediana_calidad, 2)))
  print(paste("Rango de calidad:", round(min(datos$Calidad), 2), "-", round(max(datos$Calidad), 2)))
  print(paste("Número de parcelas analizadas:", length(unique(datos$id_parcela))))
  
  # Parcelas con mejor y peor tendencia
  mejor_parcela <- tendencias_parcela %>%
    group_by(id_parcela) %>%
    summarise(Calidad_Media = mean(Calidad_Promedio)) %>%
    arrange(desc(Calidad_Media))
  
  print(paste("Parcela con mejor calidad promedio:", mejor_parcela$id_parcela[1], 
              "- Calidad:", round(mejor_parcela$Calidad_Media[1], 2)))
  print(paste("Parcela con menor calidad promedio:", tail(mejor_parcela$id_parcela, 1), 
              "- Calidad:", round(tail(mejor_parcela$Calidad_Media, 1), 2)))

















