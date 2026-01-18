##############################################
#####STATISTICS IN R - MODUS PARAGUAYENSIS####
##############################################
#TRABAJO 1: DATOS AGRUPADOS###################
##############################################

## Librerias
library(tinytex)
library(readr)
library(modeest)
library(moments)
library(dplyr)
library(ggplot2)
library(knitr)
library(summarytools)
library(kableExtra)


## ----intro---------------------------------------------------------------------------------------------------------------------------------
library(readr)
# link "Raw" al archivo de datos
url_raw <- paste0("https://raw.githubusercontent.com/",
                  "elprofemarcosgh/Statistics-in-R-Modus-Paraguayensis/",
                  "main/trabajo_2/datos_para_tabla_est.csv")

df_indices <- read_csv(url_raw)

df_indices <- df_indices %>%
  mutate(IAAP = sqrt(Indice_Capacidad_General^2 + Indice_Especializacion^2))
# head() para mostrar solo los primeros 10 registros
# select() para organizar las columnas de forma lógica para el lector
library(dplyr)
vista_previa <- df_indices %>% 
  select(Indice_Capacidad_General, Indice_Especializacion, IAAP) %>% 
  head(10)

knitr::kable(vista_previa, 
             caption = "Muestra de los primeros 10 registros de la base de datos (Variables clave e Índices)",
             digits = 2)


## ----1-------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
# Calcula las 3 medidas principales para el IAAP
media_arit_iaap   <- mean(df_indices$IAAP, na.rm = TRUE)
mediana_iaap <- median(df_indices$IAAP, na.rm = TRUE)

# Para la moda usa la librería modeest
# mfv devuelve un vector (por si hay más de una moda)
moda_iaap    <- mfv(df_indices$IAAP)

# Media Geométrica. Se calcula como la exponencial de la media de los logaritmos
media_geom_iaap <- exp(mean(log(df_indices$IAAP[df_indices$IAAP > 0]), na.rm = TRUE))

# Se crea una tabla simple para mostrar los resultados
tabla_tendencia <- data.frame(
  Medida = c("Media Aritmética","Media Geométrica", "Mediana", "Moda"),
  Valor = c(media_arit_iaap, media_geom_iaap, mediana_iaap, moda_iaap[1])
)

print(tabla_tendencia)



## ----grafico_tendencia_no_agrupados--------------------------------------------------------------------------------------------------------
# Se define la altura máxima de la densidad para ubicar las etiquetas
densidad_max <- max(density(df_indices$IAAP)$y)

ggplot(df_indices, aes(x = IAAP)) +
  # 1. El Histograma de fondo (suave)
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", alpha = 0.2, color = "white") +
  
  # . Líneas de Tendencia Central
  geom_vline(xintercept = media_arit_iaap, color = "red", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = mediana_iaap, color = "darkgreen", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = media_geom_iaap, color = "black", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = moda_iaap[1], color = "darkorange", linetype = "dashed", size = 0.8) +
  
  #3 . Etiquetas (Ajustadas a la altura de la densidad)
  annotate("text", x = media_arit_iaap, y = densidad_max * 0.6, 
           label = paste("Media arit:", round(media_arit_iaap, 4)), 
           color = "red", angle = 90, vjust = -0.5, fontface = "bold") +
  
  annotate("text", x = mediana_iaap, y = densidad_max * 0.35, 
           label = paste("Mediana:", round(mediana_iaap, 4)), 
           color = "darkgreen", angle = 90, vjust = 1.5, fontface = "bold") +
  
  annotate("text", x = media_geom_iaap, y = densidad_max * 0.7, 
           label = paste("Media Geom:", round(media_geom_iaap, 4)), 
           color = "black", angle = 90, vjust = -0.5, fontface = "bold") +
  
  annotate("text", x = moda_iaap[1], y = densidad_max * 0.6, 
           label = paste("Moda:", round(moda_iaap[1], 4)), 
           color = "darkorange", angle = 90, vjust = 1.5, fontface = "bold") +
  
  # Estética y límites
  labs(title = "Gráfico 1. Distribución de Densidad de la variable IAAP",
       subtitle = "Análisis de tendencia central con datos no agrupados",
       x = "Índice de Articulación Académica Profesional (IAAP)",
       y = "Densidad") +
  theme_minimal()


## ----2-------------------------------------------------------------------------------------------------------------------------------------
# Cuartiles (Q1, Q2, Q3)
cuartiles_iaap <- quantile(df_indices$IAAP, probs = c(0.25, 0.50, 0.75))

# Deciles (D1 al D9)
deciles_iaap <- quantile(df_indices$IAAP, probs = seq(0.1, 0.9, by = 0.1))

# Percentiles (Seleccionamos los más representativos: 1, 5, 10, 90, 95, 99)
percentiles_iaap <- quantile(df_indices$IAAP, probs = c(0.01, 0.05, 0.10, 0.3, 0.5, 0.7, 0.90, 0.95, 0.99))

# --- Presentación de resultados ---

# Tabla de Cuartiles (Para el reporte)
knitr::kable(as.data.frame(t(cuartiles_iaap)), 
             caption = "Medidas de Posición: Cuartiles del IAAP.  Datos no agrupados",
             digits = 4)

#Tabla de deciles
df_deciles <- data.frame(
  Decil = names(deciles_iaap),
  Valor = as.numeric(deciles_iaap)
)

# Renombra las filas para que se lea "D1, D2..." en la tabla
df_deciles$Decil <- paste0("D", 1:9, " (", df_deciles$Decil, ")")

# 4. Genera la tabla con kable
knitr::kable(df_deciles, 
             col.names = c("Decil", "Valor IAAP"),
             caption = "Distribución por Deciles de la variable IAAP.  Datos no agrupados",
             digits = 4,
             align = 'lc')

# Tabla de Percentiles clave

df_percentiles <- data.frame(
  Percentil = names(percentiles_iaap),
  Valor = as.numeric(percentiles_iaap)
)

knitr::kable(df_percentiles, 
             caption = "Distribución por Percentiles de la variable IAAP.  Datos no agrupados",
             digits = 4)



## ----grafico_posicionamiento---------------------------------------------------------------------------------------------------------------
# 1. Cálculos base
media_val <- mean(df_indices$IAAP, na.rm = TRUE)
moda_val <- 0.05 
estats <- boxplot.stats(df_indices$IAAP)$stats
q1 <- estats[2]; mediana <- estats[3]; q3 <- estats[4]
# Límites según la regla de 1.5 * RIC
lim_inf_ric <- estats[1]
lim_sup_ric <- estats[5]

# 2. Gráfico con ajustes de "aire" y nomenclatura técnica
ggplot(df_indices, aes(y = IAAP, x = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.3, outlier.color = "red", outlier.shape = 16) +
  
  # Media (Diamante)
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred") +
  annotate("text", x = 1.45, y = media_val, label = paste0("Media: ", round(media_val, 3)), 
           color = "darkred", fontface = "bold") +
  
  # Moda (Línea y etiqueta inferior)
  geom_hline(yintercept = moda_val, linetype = "dotted", color = "purple", size = 1) +
  annotate("text", x = 1.4, y = moda_val, label = paste0("Moda: ", round(moda_val, 3)), 
           color = "purple", fontface = "italic", angle = 90, vjust = -0.5) +

  # Cuartiles y Mediana - Desplazados hacia arriba (x = 1.3) para separarlos de la caja
  annotate("text", x = 1.3, y = q1, label = paste0("Q1: ", round(q1, 3)), color = "blue", vjust = 2.2) +
  annotate("text", x = 1.3, y = mediana, label = paste0("Mediana: ", round(mediana, 3)), color = "darkgreen", fontface = "bold") +
  annotate("text", x = 1.3, y = q3, label = paste0("Q3: ", round(q3, 3)), color = "blue", vjust = 2.2) +
  
  # Límites del RIC (Bigotes) - Etiquetas en la parte inferior (x = 0.7)
  annotate("text", x = 0.5, y = lim_inf_ric+0.15, label = paste0("Lím. Inf (1.5*RIC): ", round(lim_inf_ric, 3)), size = 2.8) +
  annotate("text", x = 0.7, y = lim_sup_ric, label = paste0("Lím. Sup (1.5*RIC): ", round(lim_sup_ric, 3)), size = 2.8) +
  
  # Outlier - Etiqueta a la IZQUIERDA del punto (hjust = 1.5)
  geom_text(aes(label = ifelse(IAAP > lim_sup_ric | IAAP < lim_inf_ric, round(IAAP, 3), "")), 
            hjust = 1.5, color = "red", size = 3, fontface = "bold.italic") +

  labs(title = "Gráfico 2. Gráfico boxplot (cajas y bigotes)",
       y = "Índice IAAP", x = "") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) + 
  coord_flip()


## ----3-------------------------------------------------------------------------------------------------------------------------------------

# 0. Valores Base
min_iaap <- min(df_indices$IAAP)
max_iaap <- max(df_indices$IAAP)

# 1. Rango (Valor Máx - Valor Mín)
rango_iaap <- max(df_indices$IAAP) - min(df_indices$IAAP)

# 2. Varianza Poblacional y Muestral
# var() en R calcula la muestral (n-1)
varianza_muestral <- var(df_indices$IAAP)

# 3. Desviación Estándar
desv_std_iaap <- sd(df_indices$IAAP)

# 4. Coeficiente de Variación (CV)
cv_iaap <- (desv_std_iaap / media_arit_iaap) * 100

# 5. Error Estándar de la Media
error_est_iaap <- desv_std_iaap / sqrt(length(df_indices$IAAP))
# Consolida en una tabla para el reporte
tabla_variabilidad <- data.frame(
  Indicador = c("Valor Mínimo", "Valor Máximo","Rango", "Varianza (Muestral)", "Desviación Estándar", "C.V. (%)", "Error Estándar de la Media"),
  Valor = c(min_iaap, max_iaap, rango_iaap, varianza_muestral, desv_std_iaap, cv_iaap, error_est_iaap)
)



## ----3 tabla-------------------------------------------------------------------------------------------------------------------------------

knitr::kable(tabla_variabilidad, 
             caption = "Medidas de Variabilidad y Dispersión del IAAP",
             digits = 4)



## ----grafico_variabilidad------------------------------------------------------------------------------------------------------------------
ggplot(df_indices, aes(x = IAAP)) +
  # Histograma de fondo
  geom_histogram(aes(y = ..density..), bins = 20, fill = "gray", color = "white") +
  # Curva de densidad para ver la variabilidad suavizada
  geom_density(fill = "steelblue", alpha = 0.4, color = "steelblue", size = 1) +
  # Línea de la Media
  geom_vline(aes(xintercept = mean(IAAP)), color = "darkred", linetype = "dashed", size = 1) +
  # Sombreado de la Desviación Estándar (opcional para ver la dispersión)
  annotate("rect", xmin = mean(df_indices$IAAP) - sd(df_indices$IAAP), 
           xmax = mean(df_indices$IAAP) + sd(df_indices$IAAP), 
           ymin = 0, ymax = Inf, alpha = 0.1, fill = "red") +
  labs(title = "Gráfico 3. Distribución y Variabilidad del IAAP",
       x = "IAAP",
       y = "Densidad") +
  theme_minimal()


## ----5-------------------------------------------------------------------------------------------------------------------------------------

# 1. Asimetría de Fisher (G1)
# Usamos la librería moments
g1_fisher <- moments::skewness(df_indices$IAAP)

# 2. Asimetría de Pearson (Ap)
# Fórmula: (Media - Moda) / Desviación Estándar
# Usa la primera moda en caso de que sea multimodal
ap_pearson <- (media_arit_iaap - moda_iaap[1]) / desv_std_iaap

# 3. Asimetría de Bowley (Ab)
# Basada en cuartiles: (Q3 + Q1 - 2*Q2) / (Q3 - Q1)
# Es más robusta ante valores extremos
q1 <- cuartiles_iaap[1]
q2 <- cuartiles_iaap[2] # La mediana
q3 <- cuartiles_iaap[3]
ab_bowley <- (q3 + q1 - 2*q2) / (q3 - q1)

# 4. Curtosis (g2)
# El valor 3 representa la distribución Normal (Mesocúrtica)
g2_curtosis <- moments::kurtosis(df_indices$IAAP)

# Consolida para el reporte
tabla_forma <- data.frame(
  Medida = c("Asimetría de Fisher (G1)", 
             "Asimetría de Pearson (Ap)", 
             "Asimetría de Bowley (Ab)", 
             "Curtosis (g2)"),
  Valor = c(g1_fisher, ap_pearson, ab_bowley, g2_curtosis)
)

knitr::kable(tabla_forma, 
             caption = "Medidas de Forma: Asimetría y Curtosis del IAAP",
             digits = 4)




## ----6-------------------------------------------------------------------------------------------------------------------------------------
ggplot(df_indices, aes(x = IAAP)) +
  geom_histogram(aes(y = ..density..), bins = 15, 
                 fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  geom_vline(aes(xintercept = media_arit_iaap), color = "green", 
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mediana_iaap), color = "orange", 
             linetype = "dotted", size = 1) +
  labs(title = "Gráfico 4. Distribución del IAAP y Líneas de Tendencia Central",
       subtitle = "Verde: Media | Naranja: Mediana",
       x = "Índice (IAAP)", y = "Densidad") +
  theme_minimal()




## ----grafico 7-----------------------------------------------------------------------------------------------------------------------------

ggplot(df_indices, aes(x = IAAP)) +
  # 1. Histograma 
  geom_histogram(aes(y = after_stat(density)), bins = 15, 
                 fill = "steelblue", alpha = 0.3, color = "white") +
  
  # 2. Curva Kernel 
  geom_density(color = "darkblue", size = 1.2) +
  
  # 3. Curva Normal Teórica (Referencia)
  # Usamos las variables originales: media_arit_iaap y desv_std_iaap
  stat_function(fun = dnorm, 
                args = list(mean = media_arit_iaap, sd = desv_std_iaap), 
                color = "black", linetype = "dotted", size = 1.2) +
  
  # 4. Líneas de referencia de Tendencia Central
  
  geom_vline(aes(xintercept = media_arit_iaap), color = "red", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = mediana_iaap), color = "darkgreen", linetype = "longdash", size = 1) + 
  geom_vline(aes(xintercept = moda_iaap[1]), color = "darkorange", size = 1.2) + 
  
  # 5. Anotaciones de las líneas
  annotate("text", x = media_arit_iaap, y = 0.5, label = "Media", color = "red", 
           angle = 90, vjust = -0.5, size = 3.5, fontface = "bold") +
  annotate("text", x = moda_iaap[1], y = 0.5, label = "Moda", color = "darkorange", 
           angle = 90, vjust = 1.5, size = 3.5, fontface = "bold") +
  
  # Etiqueta para la curva normal de referencia
  annotate("text", x = 2.5, y = 0.15, label = "Referencia Normal", 
           color = "grey40", size = 3.5, fontface = "italic") +
  
  # Estética y subtítulo dinámico
  labs(title = "Gráfico 5. Silueta de la Distribución de Puntajes IAAP",
       subtitle = paste("Asimetría Fisher:", round(g1_fisher, 3), 
                        "| Curtosis:", round(g2_curtosis, 3), 
                        "| CV:", round(cv_iaap, 2), "%"),
       x = "Puntaje IAAP (Índice de Articulación)",
       y = "Densidad de Frecuencia") +
  theme_minimal()




## ----8, include=FALSE----------------------------------------------------------------------------------------------------------------------
# link "Raw" al archivo de datos
url_raw <- paste0("https://raw.githubusercontent.com/",
                  "elprofemarcosgh/Statistics-in-R-Modus-Paraguayensis/",
                  "main/trabajo%231/datos_para_tabla_est.csv")

df_indices <- read_csv(url_raw)

df_indices <- df_indices %>%
  mutate(IAAP = sqrt(Indice_Capacidad_General^2 + Indice_Especializacion^2))
# head() para mostrar solo los primeros 10 registros
# select() para organizar las columnas de forma lógica para el lector
library(dplyr)

# Parámetros básicos
n_obs <- nrow(df_indices)
val_min <- min(df_indices$IAAP)
val_max <- max(df_indices$IAAP)
rango <- val_max - val_min
cat("Rango:\n",
    "Rango:", round(rango, 2), "\n")
# Aplicar Sturges para obtener el número de intervalos (k)
k <- ceiling(1 + 3.322 * log10(n_obs)) # ceiling redondea hacia arriba
cat("Cantidad de intervalos necesarios:\n",
    "Sturges:", round(k, 2), "\n")
# Calcular la amplitud (h)
amplitud <- rango / k
cat("Amplitud de intervalos:\n",
    "Amplitud:", round(amplitud, 2), "\n")
# Generar los cortes (breaks) automáticos
cortes_sturges <- seq(val_min, val_max, by = amplitud)
cat("Cortes de intervalos:\n",
    "Cortes:", round(cortes_sturges, 2), "\n")
library(dplyr)

# 1. cut() para agrupar el IAAP según los cortes de Sturges calculado
# include.lowest = TRUE asegura que el valor mínimo entre en el primer intervalo
df_agrupado <- df_indices %>%
  mutate(Intervalo_Sturges = cut(IAAP, breaks = cortes_sturges, include.lowest = TRUE))

# 2. Construcción de la tabla dinámica
tabla_completa <- df_agrupado %>%
  group_by(Intervalo_Sturges) %>%
  summarise(Frecuencia_Absoluta = n(), .groups = 'drop') %>%
  mutate(
    # El nombre del intervalo viene de R (ej. [0.05, 0.4])
    Intervalo = as.character(Intervalo_Sturges),
    
    # Marca de Clase: punto medio entre cada par de cortes
    Marca_Clase = (cortes_sturges[-length(cortes_sturges)] + cortes_sturges[-1]) / 2,
    
    # Frecuencias y porcentajes
    Frec_Acumulada = cumsum(Frecuencia_Absoluta),
    Porcentaje = (Frecuencia_Absoluta / sum(Frecuencia_Absoluta)) * 100,
    Porc_Acumulado = cumsum(Porcentaje)
  ) %>%
  # Seleccionamos y ordenamos las columnas para el reporte
  select(Intervalo, Marca_Clase, Frecuencia_Absoluta, Frec_Acumulada, Porcentaje, Porc_Acumulado)
## ----tendencia---------------------------------------------------------------------------------------------------------
media_agrupada <- sum(tabla_completa$Marca_Clase * tabla_completa$Frecuencia_Absoluta) / sum(tabla_completa$Frecuencia_Absoluta)

n <- sum(tabla_completa$Frecuencia_Absoluta)
suma_log <- sum(tabla_completa$Frecuencia_Absoluta * log(tabla_completa$Marca_Clase))
media_geo_agrupada <- exp(suma_log / n)

intervalo_mediano <- tabla_completa %>% filter(Porc_Acumulado >= 50) %>% slice(1)
mediana_agrupada <- intervalo_mediano$Marca_Clase

intervalo_modal <- tabla_completa %>% filter(Frecuencia_Absoluta == max(Frecuencia_Absoluta))
moda_agrupada <- intervalo_modal$Marca_Clase

## ----variabilidad------------------------------------------------------------------------------------------------------
n <- sum(tabla_completa$Frecuencia_Absoluta)
# Variabilidad
varianza_agrupada <- sum(tabla_completa$Frecuencia_Absoluta * (tabla_completa$Marca_Clase - media_agrupada)^2) / (n - 1)
desviacion_agrupada <- sqrt(varianza_agrupada)
cv <- (desviacion_agrupada / media_agrupada) * 100

## ----silueta-----------------------------------------------------------------------------------------------------------
# --- CÁLCULO DE MOMENTOS (Para Fisher) ---
# Usa n calculado anteriormente y las marcas de clase de Sturges
m3 <- sum(tabla_completa$Frecuencia_Absoluta * (tabla_completa$Marca_Clase - media_agrupada)^3) / n
m4 <- sum(tabla_completa$Frecuencia_Absoluta * (tabla_completa$Marca_Clase - media_agrupada)^4) / n

# 1. ASIMETRÍA DE FISHER
asimetria_fisher <- m3 / (desviacion_agrupada^3)

# 2. ASIMETRÍA DE PEARSON
asimetria_pearson <- (media_agrupada - moda_agrupada) / desviacion_agrupada

# 3. ASIMETRÍA DE BOWLEY (Basada en Cuartiles)

# función para que use los cortes de Sturges y la amplitud real
calcular_Q_dinamico <- function(k) {
  objetivo <- n * k
  fila_idx <- which(tabla_completa$Frec_Acumulada >= objetivo)[1]
  
  # Usa los vectores que definiste en el bloque de Sturges
  L_inf <- cortes_sturges[fila_idx] 
  F_ant <- if(fila_idx == 1) 0 else tabla_completa$Frec_Acumulada[fila_idx - 1]
  f_i   <- tabla_completa$Frecuencia_Absoluta[fila_idx]
  # 'amplitud' es la variable calculada (0.35)
  
  return(L_inf + ((objetivo - F_ant) / f_i) * amplitud)
}

# Cálculos de Cuartiles para Bowley
Q1_b <- calcular_Q_dinamico(0.25)
Q2_b <- calcular_Q_dinamico(0.50) # Mediana interpolada
Q3_b <- calcular_Q_dinamico(0.75)

asimetria_bowley <- (Q3_b + Q1_b - 2 * Q2_b) / (Q3_b - Q1_b)

# 4. CURTOSIS (G2)
kurtosis_fisher <- (m4 / (desviacion_agrupada^4)) - 3




## ----tabla_agrupados9, include=FALSE-------------------------------------------------------------------------------------------------------

library(knitr)
library(kableExtra)

# Creamos un dataframe con los resultados que ya calculaste en tus chunks anteriores
tabla_agrupados_T1 <- data.frame(
  Estadístico = c("Media Aritmética", "Media Geométrica", "Mediana (Interpolada)", "Moda (Marca de Clase)",
                   "Varianza", "Desviación Estándar", "Coeficiente de Variación (%)",
                   "Asimetría de Fisher", "Asimetría de Pearson", "Asimetría de Bowley", 
                   "Curtosis (g2)"),
  Valor_Agrupado = c(media_agrupada, media_geo_agrupada, mediana_agrupada, moda_agrupada,
                     varianza_agrupada, desviacion_agrupada, cv,
                     asimetria_fisher, asimetria_pearson, asimetria_bowley, 
                     kurtosis_fisher)
)


## ----tabla_agrupados9_, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------
# tabla del Trabajo #1
knitr::kable(tabla_agrupados_T1, 
             digits = 4, 
             format = "simple", 
             caption = "Estadísticos IAAP - Metodología Agrupados (T1)")




## ----10, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------
library(knitr)
library(kableExtra)

# 1. CONSTRUCCIÓN DE LA TABLA COMPARATIVA

df_comparativo <- data.frame(
  Indicador = c("Media Aritmética", "Media Geométrica", "Mediana", "Moda", 
                "Varianza", "Desviación Estándar", "Coef. Variación (%)",
                "Asimetría Fisher (g1)", "Asimetría Pearson", "Asimetría Bowley", 
                "Curtosis (g2)"),
  
  # Variables de Datos Agrupados (T1)
  Datos_Agrupados = c(media_agrupada, media_geo_agrupada, mediana_agrupada, moda_agrupada,
                   varianza_agrupada, desviacion_agrupada, cv,
                   asimetria_fisher, asimetria_pearson, asimetria_bowley, 
                   kurtosis_fisher),
  
  # Variables de Datos No Agrupados (T2)
  Datos_no_agrupados = c(media_arit_iaap, media_geom_iaap, mediana_iaap, moda_iaap[1], 
                    varianza_muestral, desv_std_iaap, cv_iaap, 
                    g1_fisher, ap_pearson, ab_bowley, 
                    g2_curtosis)
)

# 2. CÁLCULO DE LA DESVIACIÓN (Error de Agrupación)
df_comparativo$Diferencia <- df_comparativo$Datos_no_agrupados - df_comparativo$Datos_Agrupados

# 3. tabla
knitr::kable(df_comparativo, digits = 4, format = "simple", booktabs = TRUE,
      caption = "Comparación entre cálculo de datos agrupados y no agrupados")


