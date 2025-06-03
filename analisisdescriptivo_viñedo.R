# Análisis estadísticos básicos para responder las preguntas
library(dplyr)
library(ggplot2)
library(gridExtra)
# Análisis de patrones que se repiten en grupos de parcelas
library(cluster)
#install.packages("factoextra")
library(factoextra)

df <- read.csv("C:\\Users\\NANCY\\Documents\\datos_viñedo\\data_simulacion\\datos_vinedo_tratados1.csv")

# Análisis de calidad por parcela y tipo de uva
# 1. Calidad promedio por parcela
calidad_parcela <- df %>%
  group_by(id_parcela) %>%
  summarise(
    calidad_promedio = mean(Calidad, na.rm = TRUE),
    calidad_mediana = median(Calidad, na.rm = TRUE),
    calidad_min = min(Calidad, na.rm = TRUE),
    calidad_max = max(Calidad, na.rm = TRUE),
    desviacion_std = sd(Calidad, na.rm = TRUE),
    n_observaciones = n()
  ) %>%
  arrange(desc(calidad_promedio))

print("CALIDAD ACTUAL POR PARCELA:")
print(calidad_parcela)

# Gráfico , Calidad promedio por parcela
p1 <- ggplot(calidad_parcela, aes(x = reorder(factor(id_parcela), calidad_promedio), y = calidad_promedio)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(calidad_promedio, 1)), vjust = -0.5, size = 3) +
  labs(title = "Calidad Promedio por Parcela",
       x = "Parcela",
       y = "Calidad Promedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# 2. Calidad por tipo de uva
calidad_tipo_uva <- df %>%
  mutate(tipo_uva = ifelse(vino_Merlot == 1, "Merlot", "Cabernet")) %>%
  group_by(tipo_uva) %>%
  summarise(
    calidad_promedio = mean(Calidad, na.rm = TRUE),
    calidad_mediana = median(Calidad, na.rm = TRUE),
    calidad_min = min(Calidad, na.rm = TRUE),
    calidad_max = max(Calidad, na.rm = TRUE),
    desviacion_std = sd(Calidad, na.rm = TRUE),
    n_observaciones = n()
  )

print("CALIDAD POR TIPO DE UVA:")
print(calidad_tipo_uva)

# Gráfico , Comparación entre tipos de uva
df_plot <- df %>%
  mutate(tipo_uva = ifelse(vino_Merlot == 1, "Merlot", "Cabernet"))

p2 <- ggplot(df_plot, aes(x = tipo_uva, y = Calidad, fill = tipo_uva)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  scale_fill_manual(values = c("Merlot" = "#8B0000", "Cabernet" = "#4B0082")) +
  labs(title = "Distribución de Calidad por Tipo de Uva",
       x = "Tipo de Uva",
       y = "Calidad",
       fill = "Tipo de Uva") +
  theme_minimal()

print(p2)


# 3. Calidad por parcela y tipo de uva combinado
calidad_parcela_uva <- df %>%
  mutate(tipo_uva = ifelse(vino_Merlot == 1, "Merlot", "Cabernet")) %>%
  group_by(id_parcela, tipo_uva) %>%
  summarise(
    calidad_promedio = mean(Calidad, na.rm = TRUE),
    n_observaciones = n(),
    .groups = 'drop'
  )

print("CALIDAD POR PARCELA Y TIPO DE UVA:")
print(head(calidad_parcela_uva, 15))

# Calidad por parcela separada por tipo de uva
p3 <- ggplot(calidad_parcela_uva, aes(x = factor(id_parcela), y = calidad_promedio, fill = tipo_uva)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(calidad_promedio, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = c("Merlot" = "#8B0000", "Cabernet" = "#4B0082")) +
  labs(title = "Calidad Promedio por Parcela y Tipo de Uva",
       x = "Parcela",
       y = "Calidad Promedio",
       fill = "Tipo de Uva") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)






