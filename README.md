# Análisis y Predicción de la Calidad de Parcelas en Viñedos 🍇

Este proyecto realiza un análisis de datos simulados de un viñedo y utiliza modelos de aprendizaje automático en R para **predecir la calidad de las parcelas de cultivo**, basándose en factores agrícolas como clima, plagas, poda, tipo de vino, y más. Se desarrolla también una proyección de calidad hacia fin de año con visualización de riesgos y tendencias.

## 🚀 Demo Interactivo

Puedes revisar todos los resultados, gráficos y visualizaciones en el siguiente demo público:

👉 ** Ver demo en https://ing-nancy-galan-99.github.io/analisis_vinedo_prediccion/ **

donde encontraras los resultados


## 📊 Objetivo del Proyecto

El propósito principal fue explorar los factores que influyen en la **calidad del cultivo de uva** y, mediante modelos como **Random Forest y árboles de decisión**, clasificar las parcelas como de **calidad alta o baja**. Además, se realiza una **proyección futura** para predecir cómo evolucionarán estas parcelas al cierre del año.

## 🔍 Dataset

Los datos utilizados fueron simulados con base en variables reales que afectan el rendimiento agrícola:

- Variables climáticas: Temperatura, Lluvia, pH
- Variables agronómicas: Poda, Plaga, Grado Brix, Peso y Conteo de racimos, Rendimiento
- Información de cultivo: Ciclo fenológico (Cuajado, Envero, Meseta)
- Tipo de vino producido: Merlot o Cabernet
- Datos temporales: Día, Mes, Año
- Variable objetivo: Calidad (numérica), que luego se transformó en **Alta / Baja**

## ⚙️ Tecnología y Herramientas

- Lenguaje: **R**
- Librerías principales: `randomForest`, `caret`, `ROSE`, `rpart`, `ggplot2`, `dplyr`, `tidyr`
- Visualización: `ggplot2`, `rpart.plot`, `gridExtra`
- Balanceo de datos: **ROSE**
- Publicación de resultados: **GitHub Pages**

## 🔬 Proceso Analítico

### 1. **Clasificación de Calidad**
- Se definió un umbral basado en la mediana para clasificar las parcelas.
- Se compararon valores promedio de cada variable entre parcelas de **Alta** y **Baja calidad**.

### 2. **Entrenamiento de Modelos**
- Se entrenó un modelo de **Random Forest** con métricas:
  - Accuracy
  - Precision
  - Recall
  - F1-Score
- También se construyó un **árbol de decisión** con variables clave como Poda, Plaga y Temperatura.

### 3. **Importancia de Variables**
- Las variables más influyentes fueron:
  - Grado Brix
  - Conteo de Racimos
  - Temperatura
  - Peso de Racimos

### 4. **Proyección a Futuro**
- Se simularon condiciones climáticas y agronómicas para prever la calidad a fin de año.
- Se calcularon probabilidades de obtener una calidad Alta por parcela.
- Se identificaron:
  - Parcelas con **mayor probabilidad de éxito**
  - Parcelas con **alto riesgo** (calidad baja proyectada)

### 5. **Visualización de Tendencias**
- Se graficó la **tendencia de calidad por mes y por tipo de vino**.
- Se observaron:
  - Parcelas con trayectoria positiva.
  - Parcelas que requieren intervención.

## ✅ Resultados Relevantes

- La mayoría de las **parcelas Cabernet** mostraron menor probabilidad de alta calidad comparado con Merlot.
- Factores como **lluvia alta**, **presencia de plagas** y **menor poda** se asociaron con **baja calidad**.
- Se identificaron 3 parcelas críticas con riesgo, recomendando acciones correctivas.


## 📌 Conclusión

Este análisis permite entender qué factores influyen directamente en la calidad del viñedo y cómo se puede anticipar su rendimiento futuro. El uso de modelos predictivos ayuda a la **toma de decisiones agronómicas basadas en datos**, enfocándose en la mejora continua de cultivos.


