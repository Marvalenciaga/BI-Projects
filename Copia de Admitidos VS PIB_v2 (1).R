# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(forecast)

# Cargar los datos del archivo proporcionado
data <- readxl::read_excel("~/Downloads/Data_Admitted_anonimized.xlsx")
print(head(data))

# Asegurarse de que las columnas necesarias existan
if (!all(c("GENDER", "SEMESTER") %in% colnames(data))) {
  stop("El conjunto de datos debe contener las columnas 'GENDER' y 'SEMESTER'.")
}

# Crear una columna de cantidad si no existe
if (!"COUNT" %in% colnames(data)) {
  data <- data %>%
    group_by(SEMESTER, GENDER) %>%
    summarise(COUNT = n(), .groups = "drop")
}

print(head(data))

# Limpieza y conversión de la columna PERIODO
data <- data %>%
  mutate(
    SEMESTER = gsub("[^0-9]", "", SEMESTER), # Eliminar caracteres no numéricos
    SEMESTER = as.numeric(SEMESTER)         # Convertir a numérico
  )

# Verificar si hay NA en la columna PERIODO
if (any(is.na(data$SEMESTER))) {
  warning("Existen valores NA en la columna 'PERIODO' después de la conversión.")
}

# Convertir a factores y asegurar que las variables estén ordenadas correctamente
data <- data %>%
  mutate(
    GENDER = as.factor(GENDER),
    SEMESTER = as.numeric(SEMESTER),
    COUNT = as.numeric(COUNT)
  )

# Agrupar y sumar la cantidad por género y periodo académico
data_aggregated <- data %>%
  group_by(SEMESTER, GENDER) %>%
  summarise(Total = sum(COUNT, na.rm = TRUE)) %>%
  ungroup()

# Generar el pronóstico para cada género
forecast_list <- list() # Lista para almacenar los resultados

# Iterar sobre cada género único en los datos
for (gender_actual in unique(data_aggregated$GENDER)) {
  
  # Filtrar y preparar los datos para el género actual
  data_forecast_gender <- data_aggregated %>%
    filter(!is.na(SEMESTER) & GENDER == gender_actual) %>%
    arrange(SEMESTER)
  
  # Verificar que el filtro ha dejado datos
  if (nrow(data_forecast_gender) > 0) {
    # Crear la serie temporal
    ts_data <- ts(data_forecast_gender$Total, start = min(data_forecast_gender$SEMESTER), frequency = 1)
    
    # Ajustar el modelo NNAR
    model_nnar <- nnetar(ts_data)
    
    # Generar un pronóstico
    forecast_nnar <- forecast(model_nnar, h = 5)
    
    # Convertir el pronóstico a un data frame
    forecast_df <- data.frame(
      SEMESTER = seq(max(data_forecast_gender$SEMESTER) + 1, by = 1, length.out = 5),
      Total = forecast_nnar$mean,
      GENDER = as.character(gender_actual)
    )
    
    # Agregar los datos históricos para el gráfico
    historical_data <- data_forecast_gender %>%
      select(SEMESTER, Total) %>%
      mutate(GENDER = as.character(gender_actual))
    
    # Combinar los datos históricos y del pronóstico
    forecast_list[[as.character(gender_actual)]] <- bind_rows(historical_data, forecast_df)
  }
}

# Combinar todos los datos en un único data frame
combined_forecast <- bind_rows(forecast_list)

# Asegúrate de que combined_forecast contiene datos para ambos géneros
print(unique(combined_forecast$GENDER))

# Graficar el pronóstico para ambos géneros
ggplot(combined_forecast, aes(x = SEMESTER, y = Total, color = GENDER)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Growth Forecast by Academic Period and Gender",
    x = "Academic Semester",
    y = "Quantity",
    color = "Gender"
  ) +
  theme_minimal()

# Graficar la evolución de admitidos por semestre

ggplot(data, aes(x = SEMESTER, y = COUNT, fill = GENDER)) +
  geom_bar(stat = "identity", position = "dodge") + # Para barras lado a lado
  # geom_bar(stat = "identity", position = "stack") + # Para barras apiladas
  labs(
    title = "Distribución de Género por Semestre Académico",
    x = "Semestre Académico",
    y = "Cantidad de Admitidos",
    fill = "Género"
  ) +
  theme_minimal()


data_total <- data %>%
  group_by(SEMESTER) %>%
  summarise(Total_Admitidos = sum(COUNT, na.rm = TRUE)) %>%
  ungroup()

ggplot(data_total, aes(x = SEMESTER, y = Total_Admitidos)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Tendencia Total de Admitidos por Semestre Académico",
    x = "Semestre Académico",
    y = "Cantidad Total de Admitidos"
  ) +
  theme_minimal()


ggplot(data, aes(x = SEMESTER, y = COUNT, color = GENDER)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Tendencia de Admitidos por Género y Semestre Académico",
    x = "Semestre Académico",
    y = "Cantidad de Admitidos",
    color = "Género"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Cargar los datos del archivo proporcionado
data <- readxl::read_excel("~/Downloads/Data_Admitted_anonimized.xlsx")
print(head(data))

data <- data %>%
  group_by(CAMPUS, SEMESTER, GENDER, LEVEL_SOCIOECONOMIC, AGE, `TYPE OF SCHOOL`) %>%
  summarise(COUNT = n(), .groups = "drop")

campus_vec <- unique(data$CAMPUS)

# Función que crea el gráfico para un campus ----
plot_campus <- function(camp){
  ggplot(filter(data, CAMPUS == camp),
         aes(x = SEMESTER, y = COUNT, color = GENDER)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = camp,
      x = "Academic Semester",
      y = "Number of Admitted",
      color = "Gender"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 1️⃣  Generar un solo gráfico, por ejemplo “CAMPUS PALMIRA” ----------
plot_campus("CAMPUS PALMIRA")
plot_campus("CAMPUS AMAZONIA")
plot_campus("CAMPUS CARIBE")
plot_campus("CAMPUS DE LA PAZ")
plot_campus("CAMPUS MANIZALES")
plot_campus("CAMPUS ORINOQUIA")
plot_campus("CAMPUS TUMACO")

ggplot(data, aes(x = GENDER, y = COUNT, fill = LEVEL_SOCIOECONOMIC)) +
  geom_bar(stat = "identity", position = "dodge") + # Barras lado a lado
  labs(
    title = "Socioeconomic Level Distribution by Gender",
    x = "Gender",
    y = "Number of Admitted",
    fill = "Socioeconomic Level"
  ) +
  theme_minimal()

data_edad_promedio <- data %>%
  group_by(SEMESTER, GENDER) %>%
  summarise(Edad_Promedio = mean(AGE, na.rm = TRUE), .groups = "drop")

ggplot(data_edad_promedio, aes(x = SEMESTER, y = Edad_Promedio, color = GENDER)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Average Age of Admitted by Gender and Semester",
    x = "Academic Semester",
    y = "Average Age",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = LEVEL_SOCIOECONOMIC, y = COUNT, fill = `TYPE OF SCHOOL`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Type of School of Origin by Socioeconomic Status",
    x = "Socioeconomic Level",
    y = "Number of Admitted",
    fill = "Type of school"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = CAMPUS, y = AGE)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Campus",
    x = "Campus",
    y = "Age"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Asegúrate de que LEVEL_SOCIOECONOMIC sea un factor ordenado para esta visualización
data <- data %>%
  mutate(LEVEL_SOCIOECONOMIC = factor(LEVEL_SOCIOECONOMIC, ordered = TRUE))

ggplot(data, aes(x = SEMESTER, fill = LEVEL_SOCIOECONOMIC)) +
  geom_bar(position = "fill") + # 'fill' para ver proporciones
  facet_wrap(~ GENDER) +
  labs(
    title = "Evolution of Socioeconomic Status by Gender over Time",
    x = "Academic Semester",
    y = "Proportion",
    fill = "Socioeconomic Level"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar 45 grados

ggplot(data, aes(x = AGE, y = LEVEL_SOCIOECONOMIC)) +
  geom_boxplot(orientation = "y") +
  labs(
    title = "Relationship between Age and Socioeconomic Level",
    x = "Age",
    y = "Socioeconomic Level"
  ) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
