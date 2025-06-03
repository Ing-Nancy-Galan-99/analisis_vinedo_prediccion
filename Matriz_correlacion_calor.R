#libreria inicial
#install.packages("corrplot")

library(dplyr)
library(corrplot)
library(RColorBrewer)

df <- read.csv("C:\\Users\\NANCY\\Documents\\datos_viñedo\\data_simulacion\\datos_vinedo_tratados1.csv")
summary(df)

#verificacion de datos nuelos en la data
any(is.na(df))
colSums(is.na(df))

#Matriz para la correlacion de las variables
correlation_matrix <- cor(df)
print("Correlation matrix:")
print(correlation_matrix)
#Eliminamos la columna dia para la realizacion de la matriz ya que tiene una varianza 0 (todo los valores son 15)
df_for_corr <- df %>% select(-Dia)

correlation_matrix <- cor(df_for_corr)

# Crea el heatmap visualization
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")

#mapa de color donde donde el mas rojo muestra correlacion fuerte
corrplot(correlation_matrix, method = "color", type = "full", 
         col = colorRampPalette(c("white", "red"))(100),
         tl.cex = 0.7, tl.col = "black", tl.srt = 45,
         title = "Matriz de Correlación - Rojo: Correlación, Blanco: Sin Correlación",
         mar = c(0,0,2,0))
#guardar los datos de la matriz correlacion
#write.csv(correlation_matrix, "C:\\Users\\NANCY\\Documents\\datos_viñedo\\correlation_matrix.csv")
print(correlation_matrix)


