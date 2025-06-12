# Cargar librerías
library(dplyr)
library(ggplot2)

# Leer el archivo CSV (ajusta la ruta si es necesario)
df <- read.csv("C:\\datos_vinedo_tratados1.csv")

# Definir el umbral de calidad para clasificar parcelas
quality_threshold <- quantile(df$Calidad, 0.25)  # Percentil 25

# Clasificar parcelas en "Baja" y "Alta" calidad
df <- df %>%
  mutate(calidad_grupo = ifelse(Calidad < quality_threshold, "Baja", "Alta"))

# Definir los factores a comparar (ajusta los nombres si es necesario)
factores <- c("Temperatura", "Lluvia", "pH", "Poda", "Plaga", "GradoBrix", "ConteoRacimos", "PesoRacimos", "Rendimiento")

# Crear un dataframe para almacenar resultados
resultados_t_test <- data.frame(
  Factor = factores,
  Valor_p = NA,
  Significativo = NA
)

# Realizar pruebas t para cada factor
for (i in seq_along(factores)) {
  factor_actual <- factores[i]
  prueba <- t.test(df[[factor_actual]] ~ df$calidad_grupo)
  p_value <- prueba$p.value
  resultados_t_test$Valor_p[i] <- round(p_value, 4)
  resultados_t_test$Significativo[i] <- ifelse(p_value < 0.05, "Sí", "No")
}

# Mostrar resultados
print("Resultados de las pruebas t por factor:")
print(resultados_t_test)

# Gráfico de barras de los valores p
ggplot(resultados_t_test, aes(x = reorder(Factor, Valor_p), y = Valor_p, fill = Significativo)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Sí" = "red", "No" = "skyblue")) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(title = "Resultados de Pruebas t por Factor",
       x = "Factor",
       y = "Valor p",
       fill = "Significativo (p < 0.05)") +
  theme_minimal(base_size = 14)
