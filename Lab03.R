library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(mltools)
# Importando el archivo
http_data <- read_table("D:\\MAESTRIA-CIBERSEGURIDAD\\SEMINARIO_DATA_SCIENCE\\LABS\\Lab03_lengR\\epa-http.csv", col_names = FALSE)
# Renonbrando los encabezados de las columnas
colnames(http_data) <- c("Directions", "Date", "Method", "Resource", "Protocol", "Response_code", "Bytes")

###########Pregunta 1
# Conversiones
http_data$Method <- as.factor(http_data$Method)
http_data$Protocol <- as.factor(http_data$Protocol)
http_data$Response_code <- as.factor(http_data$Response_code)
http_data$Bytes <- as.numeric(http_data$Bytes)
#http_data$Resource <- as.factor(http_data$Resource)
# Conversión de los valores NA por 0
# Otras formas
# http_data$Bytes <- replace(http_data$Bytes, is.na(http_data$Bytes), 0)
# http_data$Bytes[is.na(http_data$Bytes)] <- 0
http_data$Bytes <- ifelse(is.na(http_data$Bytes), 0, http_data$Bytes)
nrow(http_data)
View(http_data)

#########Pregunta 2
# Creando nueva tabla segun las repeticiones de las direcciones, para obtener direcciones únicas
directions_table <- data.frame(Directions = http_data$Directions, Response_code =http_data$Response_code) 
concurrences <- as.data.frame(table(directions_table))
View(concurrences)
# Filtrando los valores existentes y ordenando de forma ascendente por la columna Response_code
# 200, 302, 304, 400, 403, 404, 500, 501
directions_data <- filter(concurrences, Freq > 0) 


directions_data <- directions_data %>%
  arrange(Response_code)
View(directions_data)

unique_codes <- unique(directions_data$Response_code)
# 200, 302, 304, 400, 403, 404, 500, 501

# Filtrar y guardar en diferentes tablas
for (code in unique_codes) {
  filtered_table <- filter(directions_data, Response_code == code)
  assign(paste0("table_code_", code), filtered_table)
}

# Hallando usuarios según el código de respuesta de la petición
code200_users <- nrow(table_code_200)
code200_users
code302_users <- nrow(table_code_302)
code302_users
code304_users <- nrow(table_code_304)
code304_users
code400_users <- nrow(table_code_400)
code400_users
code403_users <- nrow(table_code_403)
code403_users
code404_users <- nrow(table_code_404)
code404_users
code500_users <- nrow(table_code_500)
code500_users
code501_users <- nrow(table_code_501)
code501_users

####### Pregunta 3

# contar la frecuencia de la columna http
freq_http <- table(http_data$Method)
method_data <- data.frame(http = names(freq_http), freq_http = as.vector(freq_http))
View(method_data)
# "GET     46020
# "HEAD       106
# "POST      1622

# Hallando la frecuencia de la columna http, filtrando previamente los recursos tipo imagen
different_image_data <- http_data %>%
  filter(!grepl("(?i)\\.(gif|jpg|jpeg|png|bmp)$", Resource))
View(different_image_data)

freq_http2 <- table(different_image_data$Method)
method2_data <- data.frame(http = names(freq_http2), freq_http2 = as.vector(freq_http2))
View(method2_data)
#  http freq_http
#  "GET     46020
# "HEAD       106
# "POST      1622

### Pregunta 4
# Eligan el grafico que les convenga
tabla_frecuencia <-table(http_data$Response_code)
response_code_table <- data.frame(Response_code = names(tabla_frecuencia), 
                          Frecuencia = as.vector(tabla_frecuencia))


ggplot(response_code_table, aes(x = Response_code, y = Frecuencia)) +
  geom_bar(stat = "identity") +
  labs(title = "Gráfico 2 de Respuesta de Código",
       x = "Código de respuesta",
       y = "Frecuencia")

ggplot(response_code_table, aes(x = "", y = Frecuencia, fill = Response_code)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Gráfico 3 de Respuesta de Código",
       fill = "Código de respuesta") +
  theme_void()





########Pregunta 5


http_data_filtered <- http_data[, c("Method", "Response_code", "Protocol")]

epa_http_one_hot <- one_hot(as.data.table(http_data_filtered), sparsifyNAs = TRUE)

http_data$Resource_size <- nchar(http_data$Resource)

# Agrupamiento de 4 y 3

results2 <- kmeans(epa_http_one_hot, centers = 4)

results3 <- kmeans(epa_http_one_hot, centers = 3)

# Pregunta 6

# Gráficas en base a la columna bytes y Resource_size segun el tipo de agrupamiento
colores2 <- rainbow(n = length(unique(results2$cluster)))
grap1 <- plot(x = http_data$Bytes, y = http_data$Resource_size, col = colores2[results2$cluster], main="GRafico con 2")
grap1
# Creando leyenda
legend("topright", legend = levels(factor(results2$cluster)), col = colores2, pch = 16)

colores3 <- rainbow(n = length(unique(results3$cluster)))
grap2 <- plot(x = http_data$Bytes, y = http_data$Resource_size, col = colores3[results3$cluster], main="GRafico con 3")
grap2
# Creando leyenda
legend("topright", legend = levels(factor(results3$cluster)), col = colores3, pch = 16)

#termino


