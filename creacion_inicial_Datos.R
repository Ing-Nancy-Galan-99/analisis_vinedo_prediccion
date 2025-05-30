set.seed(123)  # Para reproducibilidad

# Parámetros
parcelas <- paste0("Parcela_", 1:14)
años <- 2020:2024
meses <- 1:12
etapas <- c("Cuajado", "Envero", "Meseta")

# Crear dataframe vacío
datos <- data.frame()

for (parcela in parcelas) {
  for (año in años) {
    for (mes in meses) {
      for (etapa in etapas) {
        
        # Fecha aproximada: usar día 15 para simplicidad
        fecha <- as.Date(paste(año, mes, 15, sep = "-"))
        
        # Simular variables con pequeñas variaciones por parcela y etapa
        temp_base <- 15 + 10 * sin((mes-1) * pi/6)  # patrón temperatura estacional
        temp <- rnorm(1, mean = temp_base + runif(1, -1, 1), sd = 2)
        
        lluvia <- rpois(1, lambda = 50 - 3 * (mes %in% c(6,7,8)))  # menos lluvia en verano
        
        pH <- rnorm(1, mean = 6.5 + runif(1, -0.3, 0.3), sd = 0.1)
        
        poda <- sample(c("Sí", "No"), 1, prob = c(0.7, 0.3))
        
        plaga <- sample(c("Sí", "No"), 1, prob = c(0.1, 0.9))
        
        brix <- rnorm(1, mean = ifelse(etapa == "Meseta", 22, ifelse(etapa == "Envero", 18, 14)), sd = 1)
        
        conteo_racimos <- round(rnorm(1, mean = 100 + as.numeric(gsub("Parcela_", "", parcela)) * 2, sd = 10))
        
        peso_racimos <- round(rnorm(1, mean = 1.5 + (mes/12)*0.5, sd = 0.3), 2)
        
        rendimiento <- round(conteo_racimos * peso_racimos * runif(1, 0.85, 1.15), 2)  # kg total
        
        calidad <- round(runif(1, 70, 100), 1)  # escala 70-100
        
        # Tipo de vino (Merlot o Cabernet) asignado aleatoriamente por parcela (por ejemplo)
        vino <- ifelse(as.numeric(gsub("Parcela_", "", parcela)) <= 7, "Merlot", "Cabernet")
        
        # Agregar fila
        fila <- data.frame(Parcela = parcela, Fecha = fecha, Año = año, Mes = mes, EtapaCiclo = etapa,
                           Temperatura = round(temp,1), Lluvia = lluvia, pH = round(pH,2), Poda = poda,
                           Plaga = plaga, GradoBrix = round(brix,1), ConteoRacimos = conteo_racimos,
                           PesoRacimos = peso_racimos, Rendimiento = rendimiento, Calidad = calidad, Vino = vino)
        
        datos <- rbind(datos, fila)
      }
    }
  }
}

# Chequeo rápido
head(datos)
dim(datos)  # Debería tener 2520 filas (14*5*12*3)

# Guardar CSV
write.csv(datos, "datos_vinedo_simulado.csv", row.names = FALSE)
getwd()

