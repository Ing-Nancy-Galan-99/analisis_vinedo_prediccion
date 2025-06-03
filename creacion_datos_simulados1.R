set.seed(456)  # Para reproducibilidad

parcelas <- paste0("Parcela_", 1:14)
años <- 2020:2024
meses <- 1:12
etapas <- c("Cuajado", "Envero", "Meseta")

datos <- data.frame()

for (parcela in parcelas) {
  for (año in años) {
    for (mes in meses) {
      for (etapa in etapas) {
        
        fecha <- as.Date(paste(año, mes, 15, sep = "-"))
        indice <- as.numeric(gsub("Parcela_", "", parcela))
        
        # Ajustes discretos según parcela
        temp_offset <- if (indice == 1) 1 else if (indice >= 12) -1 else 0
        lluvia_base <- if (indice == 1) 35 else if (indice >= 12) 55 else 45
        ph_base <- if (indice == 1) 6.8 else if (indice >= 12) 6.3 else 6.5
        poda_prob <- if (indice == 1) 0.95 else if (indice >= 12) 0.5 else 0.75
        plaga_prob <- if (indice == 1) 0.03 else if (indice >= 12) 0.3 else 0.12
        
        temp_base <- 15 + 10 * sin((mes-1) * pi/6) + temp_offset
        temp <- rnorm(1, mean = temp_base, sd = 2)
        
        lluvia_extra <- ifelse(año == 2022, 30, 0)
        lluvia <- rpois(1, lambda = lluvia_base - 3 * (mes %in% c(6,7,8)) + lluvia_extra)
        
        pH <- rnorm(1, mean = ph_base + runif(1, -0.15, 0.15), sd = 0.08)
        
        poda <- sample(c("Sí", "No"), 1, prob = c(poda_prob, 1 - poda_prob))
        plaga <- sample(c("Sí", "No"), 1, prob = c(plaga_prob, 1 - plaga_prob))
        
        brix_base <- ifelse(etapa == "Meseta", 22, ifelse(etapa == "Envero", 18, 14))
        brix_adjust <- if (indice == 1) 1.5 else if (indice >= 12) -1 else 0
        brix <- rnorm(1, mean = brix_base + brix_adjust, sd = 0.8)
        
        conteo_racimos <- round(rnorm(1, mean = 100 + indice * 2, sd = 8))
        peso_racimos <- round(rnorm(1, mean = 1.5 + (mes/12)*0.5, sd = 0.2), 2)
        
        eficiencia <- runif(1,
                            ifelse(indice == 1, 1.1, ifelse(indice >= 12, 0.75, 0.9)),
                            ifelse(indice == 1, 1.25, ifelse(indice >= 12, 0.85, 1.05)))
        
        rendimiento <- round(conteo_racimos * peso_racimos * eficiencia, 2)
        
        calidad <- round(runif(1,
                               ifelse(indice == 1, 88, ifelse(indice >= 12, 65, 75)),
                               ifelse(indice == 1, 100, ifelse(indice >= 12, 78, 88))
        ), 1)
        
        vino <- ifelse(indice <= 7, "Merlot", "Cabernet")
        
        fila <- data.frame(
          Parcela = parcela, Fecha = fecha, Año = año, Mes = mes,
          EtapaCiclo = etapa, Temperatura = round(temp,1), Lluvia = lluvia,
          pH = round(pH,2), Poda = poda, Plaga = plaga,
          GradoBrix = round(brix,1), ConteoRacimos = conteo_racimos,
          PesoRacimos = peso_racimos, Rendimiento = rendimiento,
          Calidad = calidad, Vino = vino
        )
        
        datos <- rbind(datos, fila)
      }
    }
  }
}


# Guardar CSV
write.csv(datos, "datos_vinedo_simulados.csv", row.names = FALSE)
getwd()
