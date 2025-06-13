#Transformacion de los Datos iniciales 
#libreria inicial
library(dplyr)
file.choose() #busca laruta de los datos
#print(ruta_datos) #presenta la ruta de los datos

#leer data inicial 
df <- read.csv("C:\\datos_vinedo_simulados.csv")
summary(df)

#transformacion de los datos
df_final <- df %>%
  #Crea la columna id_parcela apartir de parcela 
  mutate(id_parcela = as.numeric(gsub("Parcela_", "", Parcela))) %>%
  #Extrae el dia de la Fecha(quincenas)
  mutate(Dia = as.numeric(format(as.Date(Fecha), "%d"))) %>%
  #crea las columnas de etapas del vino 
  mutate(
    ciclo_Cuajado = ifelse(EtapaCiclo == "Cuajado", 1, 0),
    ciclo_Envero = ifelse(EtapaCiclo == "Envero", 1, 0),
    Ciclo_Meseta = ifelse(EtapaCiclo == "Meseta", 1, 0)
  ) %>%
  mutate(Poda = ifelse(Poda == "Sí", 1, 0)) %>%
  mutate(Plaga = ifelse(Plaga == "Sí", 1, 0)) %>%
  #crea la columnas de los tipos de vinos
  mutate(
    vino_Merlot = ifelse(Vino == "Merlot", 1, 0),
    vino_Cabernet = ifelse(Vino == "Cabernet", 1, 0)
  ) %>%
  
  select(id_parcela, Dia, Año, Mes, ciclo_Cuajado, ciclo_Envero, Ciclo_Meseta, 
         Temperatura, Lluvia, pH, Poda, Plaga, GradoBrix, ConteoRacimos, 
         PesoRacimos, Rendimiento, Calidad, vino_Merlot, vino_Cabernet)
#Imprime la cabeceras de los nuevos datos
print(head(df_final))
#Guardar la nueva data en la ruta 
write.csv(df_final, "C:\\datos_vinedo_tratados1.csv", row.names = FALSE)





