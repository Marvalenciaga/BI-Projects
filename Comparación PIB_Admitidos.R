# Cargar librerías necesarias
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(stringr)
library(scales)

# =============================
# 1. Cargar y procesar datos del PIB
# =============================

# Cargar la base del PIB con trimestres
pib_raw <- readxl::read_excel("~/Downloads/anex-PIB-EnfoqueCorriente-IVtrim2024.xlsx")

# Revisar los nombres de columnas
print("Nombres de columnas en PIB:")
print(names(pib_raw))
print("Primeras filas de PIB:")
print(head(pib_raw))

# Procesar datos del PIB
pib_limpio <- pib_raw %>%
  mutate(
    # Limpiar y extraer el año
    ANIO = as.numeric(substr(trimws(QUARTER), 1, 4)),
    # Extraer el trimestre
    T = substr(trimws(QUARTER), 6, 6),
    # Crear semestre académico
    SEMESTER = case_when(
      T %in% c("1", "2") ~ as.numeric(paste0(ANIO, "1")),
      T %in% c("3", "4") ~ as.numeric(paste0(ANIO, "2")),
      TRUE ~ NA_real_
    )
  ) %>%
  # Agrupar por semestre y sumar PIB
  group_by(SEMESTER) %>%
  summarise(PIB = sum(PIB, na.rm = TRUE), .groups = "drop") %>%
  arrange(SEMESTER) %>%
  # Crear diferentes formatos de semestre para hacer el join
  mutate(
    SEMESTER_CLEAN = case_when(
      SEMESTER >= 10000 ~ floor(SEMESTER / 10) + (SEMESTER %% 10) / 10,  # 20211 -> 2021.1
      TRUE ~ SEMESTER  # mantener como está si ya es formato simple
    ),
    SEMESTER_SIMPLE = case_when(
      SEMESTER >= 10000 ~ floor(SEMESTER / 10),  # 20211 -> 2021
      TRUE ~ SEMESTER  # mantener como está
    )
  )

print("PIB procesado:")
print(pib_limpio)
print("Valores únicos de SEMESTER en PIB:")
print(unique(pib_limpio$SEMESTER))
print("Valores únicos de SEMESTER_SIMPLE en PIB:")
print(unique(pib_limpio$SEMESTER_SIMPLE))

# =============================
# 2. Unir con datos de admitidos (asumiendo que data_aggregated ya existe)
# =============================

# Verificar que data_aggregated existe
if (!exists("data_aggregated")) {
  stop("La variable 'data_aggregated' no existe. Ejecuta primero el código de pronósticos.")
}

# Diagnóstico de los datos de admitidos
print("Valores únicos de SEMESTER en data_aggregated:")
print(unique(data_aggregated$SEMESTER))
print("Rango de SEMESTER en data_aggregated:")
print(range(data_aggregated$SEMESTER, na.rm = TRUE))

# Intentar diferentes estrategias de join
# Estrategia 1: Join directo con SEMESTER simple (año)
base_unida_opcion1 <- data_aggregated %>%
  left_join(pib_limpio %>% select(SEMESTER_SIMPLE, PIB), 
            by = c("SEMESTER" = "SEMESTER_SIMPLE"))

print("Opción 1 - Join por año simple:")
print(head(base_unida_opcion1))
print("¿Hay PIB no NA?", any(!is.na(base_unida_opcion1$PIB)))

# Estrategia 2: Crear correspondencia manual si es necesario
# Verificar si los semestres son años completos (2020, 2021, etc.)
if (all(data_aggregated$SEMESTER >= 2015 & data_aggregated$SEMESTER <= 2030, na.rm = TRUE)) {
  print("Los semestres parecen ser años. Usando join directo.")
  base_unida <- data_aggregated %>%
    left_join(pib_limpio %>% select(SEMESTER_SIMPLE, PIB), 
              by = c("SEMESTER" = "SEMESTER_SIMPLE"))
} else {
  print("Los semestres tienen formato complejo. Creando SEMESTER_CLEAN.")
  data_aggregated <- data_aggregated %>%
    mutate(
      SEMESTER_CLEAN = case_when(
        SEMESTER >= 10000 ~ floor(SEMESTER / 10),  # 20201 -> 2020
        TRUE ~ SEMESTER
      )
    )
  
  base_unida <- data_aggregated %>%
    left_join(pib_limpio %>% select(SEMESTER_SIMPLE, PIB), 
              by = c("SEMESTER_CLEAN" = "SEMESTER_SIMPLE"))
}

print("Base unida final:")
print(head(base_unida))
print("Resumen de PIB en base unida:")
print(summary(base_unida$PIB))

# =============================
# 3. Gráfico comparativo
# =============================

# Verificar que tenemos datos de PIB
if (all(is.na(base_unida$PIB))) {
  stop("No hay datos de PIB después del join. Revisar la correspondencia de semestres.")
}

# Usar la columna SEMESTER original para el eje X
semester_col <- ifelse("SEMESTER_CLEAN" %in% colnames(base_unida), "SEMESTER_CLEAN", "SEMESTER")

# Factor de escala para el PIB
factor_escala <- 1000  # Dividir PIB entre 1000 para mejor visualización

grafico_comparativo <- ggplot(base_unida, aes_string(x = semester_col)) +
  # Serie por género (eje primario)
  geom_line(aes(y = Total, color = GENDER), linewidth = 1.2) +
  geom_point(aes(y = Total, color = GENDER), size = 2) +
  
  # Serie PIB (eje secundario) - PIB dividido por factor de escala
  geom_line(aes(y = PIB / factor_escala), 
            color = "black", linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = PIB / factor_escala), 
             color = "black", size = 2, shape = 17) +
  
  # Escalas de los dos ejes
  scale_y_continuous(
    name = "Forecast Amount",
    sec.axis = sec_axis(~ . * factor_escala, 
                        name = "PIB (million COP)",
                        labels = label_number(scale = 1e-6, suffix = "M"))
  ) +
  
  # Escala del eje X
  scale_x_continuous(
    name = "Academic Semester",
    breaks = pretty(base_unida[[semester_col]], n = 8)
  ) +
  
  # Títulos y leyendas
  labs(
    title = "Comparison of Gender Forecasts with GDP in Colombia",
    subtitle = "Dotted line represents GDP (right axis)",
    color = "Gender"
  ) +
  
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(grafico_comparativo)

# =============================
# 4. Análisis de correlación
# =============================

# Usar la columna de semestre apropiada
semester_col <- ifelse("SEMESTER_CLEAN" %in% colnames(base_unida), "SEMESTER_CLEAN", "SEMESTER")

# Correlación total
correlacion_total <- base_unida %>%
  group_by(!!sym(semester_col)) %>%
  summarise(
    PRONOSTICO_TOTAL = sum(Total, na.rm = TRUE),
    PIB = first(PIB),
    .groups = "drop"
  ) %>%
  filter(!is.na(PIB) & !is.na(PRONOSTICO_TOTAL)) %>%
  summarise(COR = cor(PRONOSTICO_TOTAL, PIB, use = "complete.obs"))

print(paste("Correlation between total forecast and GDP:", 
            round(correlacion_total$COR, 3)))

# Datos para gráfico de correlación
datos_correlacion <- base_unida %>%
  group_by(!!sym(semester_col)) %>%
  summarise(
    PRONOSTICO_TOTAL = sum(Total, na.rm = TRUE),
    PIB = first(PIB),
    .groups = "drop"
  ) %>%
  filter(!is.na(PIB) & !is.na(PRONOSTICO_TOTAL))

# Gráfico de dispersión
grafico_dispersion <- ggplot(datos_correlacion, aes(x = PRONOSTICO_TOTAL, y = PIB)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3) +
  scale_y_continuous(
    name = "PIB (millones COP)",
    labels = label_number(scale = 1e-6, suffix = "M")
  ) +
  labs(
    title = "Relationship between Total Forecast and GDP by Semester",
    x = "Total Forecast",
    caption = paste("Correlation:", round(correlacion_total$COR, 3))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(grafico_dispersion)

# =============================
# 5. Análisis por género
# =============================

# Correlación por género
correlacion_por_genero <- base_unida %>%
  filter(!is.na(PIB) & !is.na(Total)) %>%
  group_by(GENDER) %>%
  summarise(
    COR = cor(Total, PIB, use = "complete.obs"),
    n_observaciones = n(),
    .groups = "drop"
  )

print("Correlation by gender:")
print(correlacion_por_genero)

# Gráfico de correlación por género
grafico_genero <- ggplot(base_unida %>% filter(!is.na(PIB)), 
                         aes(x = Total, y = PIB, color = GENDER)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  scale_y_continuous(
    name = "PIB (millions COP)",
    labels = label_number(scale = 1e-6, suffix = "M")
  ) +
  labs(
    title = "Relationship between Gender Forecast and GDP",
    x = "Gender Forecast",
    color = "Gender"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(grafico_genero)

# =============================
# 6. Análisis de diferencias y ratios
# =============================

# Datos para análisis de diferencias
analisis_diferencias <- base_unida %>%
  group_by(!!sym(semester_col)) %>%
  summarise(
    PRONOSTICO_TOTAL = sum(Total, na.rm = TRUE),
    PIB_ESCALADO = first(PIB) / factor_escala,
    .groups = "drop"
  ) %>%
  filter(!is.na(PIB_ESCALADO)) %>%
  mutate(
    DIFERENCIA = PRONOSTICO_TOTAL - PIB_ESCALADO,
    RATIO = PRONOSTICO_TOTAL / PIB_ESCALADO
  )

# Gráfico de diferencias
grafico_diferencias <- ggplot(analisis_diferencias, 
                              aes_string(x = semester_col, y = "DIFERENCIA")) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(
    name = "Academic Semester",
    breaks = pretty(analisis_diferencias[[semester_col]], n = 6)
  ) +
  labs(
    title = "Difference between Total Forecast and Scaled GDP",
    y = "Difference"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(grafico_diferencias)

# Gráfico de ratios
grafico_ratios <- ggplot(analisis_diferencias, 
                         aes_string(x = semester_col, y = "RATIO")) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_continuous(
    name = "Academic Semester",
    breaks = pretty(analisis_diferencias[[semester_col]], n = 6)
  ) +
  labs(
    title = "Ratio between Total Forecast and Scaled GDP",
    y = "Ratio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(grafico_ratios)

# =============================
# 7. Resumen de resultados
# =============================

cat("\n=== RESUMEN DE RESULTADOS ===\n")
cat("Correlación total PIB vs Pronóstico:", round(correlacion_total$COR, 3), "\n")
cat("Correlaciones por género:\n")
print(correlacion_por_genero)
cat("Número de observaciones en base unida:", nrow(base_unida), "\n")
cat("Rango de semestres:", min(base_unida[[semester_col]], na.rm = TRUE), 
    "a", max(base_unida[[semester_col]], na.rm = TRUE), "\n")
cat("Observaciones con PIB no NA:", sum(!is.na(base_unida$PIB)), "\n")

# Diagnóstico adicional si no hay datos de PIB
if (all(is.na(base_unida$PIB))) {
  cat("\n=== DIAGNÓSTICO DE PROBLEMA ===\n")
  cat("No se encontraron coincidencias entre las bases de datos.\n")
  cat("Semestres en data_aggregated:", paste(unique(data_aggregated$SEMESTER), collapse = ", "), "\n")
  cat("Semestres en pib_limpio:", paste(unique(pib_limpio$SEMESTER_SIMPLE), collapse = ", "), "\n")
  cat("Revisa el formato de las fechas en ambas bases de datos.\n")
}
