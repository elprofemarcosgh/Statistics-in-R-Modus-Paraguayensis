##############################################
#####STATISTICS IN R - MODUS PARAGUAYENSIS####
##############################################
#TRABAJO 1: DATOS AGRUPADOS###################
##############################################

## Librerias

library(tinytex)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)


## ----tabla_datos-------------------------------------------------------------------------------------------------------
library(readr)
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
vista_previa <- df_indices %>% 
  select(Indice_Capacidad_General, Indice_Especializacion, IAAP) %>% 
  head(10)

kable(vista_previa, 
      format = "simple",
      digits = 2,
      caption = "Muestra de los primeros 10 registros de la base de datos (Variables clave e Índices)")


## ----tabla_resumen_esencial--------------------------------------------------------------------------------------------
library(dplyr)
library(knitr)

df_esencial <- df_indices %>%
  select(promedio_acum, horas_estudio, creditos_aprob, iq_academico, 
         horas_capacit, complej_tarea, salario_mensual, eval_desempeno,
         Indice_Capacidad_General, Indice_Especializacion, IAAP)

resumen_esencial <- data.frame(
  Variable = c(
    "Promedio Acumulado", "Horas de Estudio", "Créditos Aprobados", 
    "IQ Académico", "Horas Capacitación", "Complejidad Tarea", 
    "Salario Mensual", "Eval. Desempeño", 
    "Índice Cap. General (ICG)", "Índice Especialización (IE)", "IAAP"
  ),
  Minimo = sapply(df_esencial, min, na.rm = TRUE),
  Maximo = sapply(df_esencial, max, na.rm = TRUE),
  Media  = sapply(df_esencial, mean, na.rm = TRUE),
  n      = sapply(df_esencial, function(x) sum(!is.na(x)))
)

# tabla
kable(resumen_esencial, 
      format = "simple",
      digits = 2, 
      row.names = FALSE,
      align = c("l", "c", "c", "c", "c"),
      caption = "Estadísticos Descriptivos Básicos",
      format.args = list(big.mark = ","))

## ----carga_datos-------------------------------------------------------------------------------------------------------

##1. CONSTRUCCION DE INTERVALOS##
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

# 3. Mostrar tabla
kable(tabla_completa, 
      format = "simple",
      digits = 2, 
      caption = "Distribución de Frecuencias del IAAP basada en la Regla de Sturges (k=9)")


## ----tendencia---------------------------------------------------------------------------------------------------------
media_agrupada <- sum(tabla_completa$Marca_Clase * tabla_completa$Frecuencia_Absoluta) / sum(tabla_completa$Frecuencia_Absoluta)

n <- sum(tabla_completa$Frecuencia_Absoluta)
suma_log <- sum(tabla_completa$Frecuencia_Absoluta * log(tabla_completa$Marca_Clase))
media_geo_agrupada <- exp(suma_log / n)

intervalo_mediano <- tabla_completa %>% filter(Porc_Acumulado >= 50) %>% slice(1)
mediana_agrupada <- intervalo_mediano$Marca_Clase

intervalo_modal <- tabla_completa %>% filter(Frecuencia_Absoluta == max(Frecuencia_Absoluta))
moda_agrupada <- intervalo_modal$Marca_Clase
cat("Resultados de Tendencia Central (Datos Agrupados):\n",
    "Media aritmética:", round(media_agrupada, 2), "\n",
    "Mediana:", mediana_agrupada, "\n",
    "Moda:", moda_agrupada, "\n",
    "Media geométrica:", media_geo_agrupada, "\n")


## ----grafico central---------------------------------------------------------------------------------------------------
h_max <- max(tabla_completa$Frecuencia_Absoluta)

ggplot(tabla_completa, aes(x = Marca_Clase, y = Frecuencia_Absoluta)) +
  # Barras y Polígono
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.3, color = "white") +
  geom_line(color = "darkblue", size = 1.2) + 
  geom_point(color = "darkblue", size = 3) +
  
  # Líneas de Tendencia Central
  geom_vline(aes(xintercept = media_agrupada), color = "red") +
  geom_vline(aes(xintercept = mediana_agrupada), color = "darkgreen") +
  geom_vline(aes(xintercept = media_geo_agrupada), color = "black") +
  geom_vline(aes(xintercept = moda_agrupada), color = "darkorange") +
  
  # Media
  annotate("text", x = media_agrupada, y = h_max * 0.65, 
           label = paste("Media:", round(media_agrupada, 2)), 
           color = "red", angle = 90, vjust = -0.5, fontface = "bold") +
  
  # Mediana
  annotate("text", x = mediana_agrupada, y = h_max * 0.75, 
           label = paste("Mediana:", round(mediana_agrupada, 2)),
           color = "darkgreen", angle = 90, vjust = 1.5, fontface = "bold") +
  
  # Media Geométrica
  annotate("text", x = media_geo_agrupada, y = h_max * 0.60, 
           label = paste("Media Geom:", round(media_geo_agrupada, 2)),
           color = "black", angle = 90, vjust = -0.5, fontface = "bold") +
  
  # Moda
  annotate("text", x = moda_agrupada, y = h_max * 0.50, 
           label = paste("Moda:", round(moda_agrupada, 2)), 
           color = "darkorange", angle = 90, vjust = 1.5, fontface = "bold") +
  
  # Estética y límites
  scale_y_continuous(limits = c(0, h_max * 1.1)) + # un 10% de margen arriba
  labs(title = "Gráfico 1. Polígono de Frecuencias de la variable IAAP",
       subtitle = "La silueta une las marcas de clase de cada intervalo",
       x = "Indice de Articulación Académica Profesional (IAAP)",
       y = "Cantidad de Estudiantes") +
  theme_minimal()


## ----posicionamiento---------------------------------------------------------------------------------------------------
# n = total de la muestra
n <- sum(tabla_completa$Frecuencia_Absoluta)

# --- CÁLCULO DEL CUARTIL 3 (Q3 - Percentil 75) ---
f_C3 <- tabla_completa %>% filter(Porc_Acumulado >= 75) %>% slice(1)
idx3 <- which(tabla_completa$Intervalo == f_C3$Intervalo)
# F_ant es 0 si el índice es 1
F_ant3 <- if(idx3 == 1) 0 else tabla_completa$Frec_Acumulada[idx3-1]

C3 <- cortes_sturges[idx3] + (((n * 0.75) - F_ant3) / f_C3$Frecuencia_Absoluta) * amplitud


# --- CÁLCULO DEL DECIL 7 (D7 - Percentil 70) ---
f_D7 <- tabla_completa %>% filter(Porc_Acumulado >= 70) %>% slice(1)
idx7 <- which(tabla_completa$Intervalo == f_D7$Intervalo)
F_ant7 <- if(idx7 == 1) 0 else tabla_completa$Frec_Acumulada[idx7-1]

D7 <- cortes_sturges[idx7] + (((n * 0.70) - F_ant7) / f_D7$Frecuencia_Absoluta) * amplitud


# --- CÁLCULO DEL PERCENTIL 85 (P85) ---
f_P85 <- tabla_completa %>% filter(Porc_Acumulado >= 85) %>% slice(1)
idx85 <- which(tabla_completa$Intervalo == f_P85$Intervalo)
F_ant85 <- if(idx85 == 1) 0 else tabla_completa$Frec_Acumulada[idx85-1]

P85 <- cortes_sturges[idx85] + (((n * 0.85) - F_ant85) / f_P85$Frecuencia_Absoluta) * amplitud


# --- MOSTRAR RESULTADOS ---
cat("RESULTADOS DE POSICIÓN (IAAP):\n",
    "Cuartil 3 (Q3):  ", round(C3, 3), "\n",
    "Decil 7 (D7):    ", round(D7, 3), "\n",
    "Percentil 85 (P85):", round(P85, 3))


## ----variabilidad------------------------------------------------------------------------------------------------------
n <- sum(tabla_completa$Frecuencia_Absoluta)
# Variabilidad
varianza_agrupada <- sum(tabla_completa$Frecuencia_Absoluta * (tabla_completa$Marca_Clase - media_agrupada)^2) / (n - 1)
desviacion_agrupada <- sqrt(varianza_agrupada)
cv <- (desviacion_agrupada / media_agrupada) * 100

cat("MEDIDAS DE VARIABILIDAD:\n",
    "Varianza:", round(varianza_agrupada, 4), "\n",
    "Desviación Estándar:", round(desviacion_agrupada, 4), "puntos\n",
    "Coeficiente de Variación:", round(cv, 2), "%\n")


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

# --- MOSTRAR RESULTADOS ---
cat("CARACTERIZACIÓN MORFOLÓGICA (IAAP):\n",
    "----------------------------------\n",
    "Asimetría de Fisher: ", round(asimetria_fisher, 3), "\n",
    "Asimetría de Pearson:", round(asimetria_pearson, 3), "\n",
    "Asimetría de Bowley: ", round(asimetria_bowley, 3), "\n",
    "Curtosis:", round(kurtosis_fisher, 3))


## ----grafico ask-------------------------------------------------------------------------------------------------------
ggplot(df_indices, aes(x = IAAP)) +
  # 1. Histograma
  geom_histogram(aes(y = after_stat(density)), bins = 10,  
                 fill = "steelblue", alpha = 0.3, color = "white") +
  
  # 2. Curva Kernel (distribución real)
  geom_density(color = "darkblue", size = 1.2) +

  # 3. Curva Normal Teórica (Referencia en línea discontinua)
  stat_function(fun = dnorm, 
                args = list(mean = 1.28, sd = 0.6225), 
                color = "black", linetype = "dashed", size = 1.5) +
  
  # 4. Líneas de referencia (Tendencia Central)
  geom_vline(aes(xintercept = 1.28), color = "red", linetype = "dashed", size = 1) + # Media
  geom_vline(aes(xintercept = 1.27), color = "darkgreen", linetype = "longdash", size = 1) + # Mediana
  geom_vline(aes(xintercept = 0.92), color = "darkorange", size = 1.2) + # Moda
  
  # 5. Anotaciones
  annotate("text", x = 1.28, y = 0.5, label = "Media", color = "red", angle = 90, vjust = -0.5, size = 3.5) +
  annotate("text", x = 0.92, y = 0.5, label = "Moda", color = "darkorange", angle = 90, vjust = 1.5, size = 3.5) +
  # Etiqueta para la curva de referencia
  annotate("text", x = 2.8, y = 0.1, label = "Referencia Normal", color = "grey40", size = 3, fontface = "italic") +
  
  # Estética y subtítulo actualizado con los datos reales
  labs(title = "*Gráfico 2. Silueta de la Distribución de Puntajes IAAP*",
       subtitle = paste("Asimetría Fisher: 0.438 | Curtosis: -0.211 | CV: 48.53%"),
       x = "Puntaje IAAP",
       y = "Densidad") +
  theme_minimal()

