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

# Hallando la frecuencia de la columna http, filtrando previamente los recursos tipo imagen
different_image_data <- http_data %>%
  filter(!grepl("(?i)\\.(gif|jpg|jpeg|png|bmp)$", Resource))
View(different_image_data)

freq_http2 <- table(different_image_data$Method)
method2_data <- data.frame(http = names(freq_http2), freq_http2 = as.vector(freq_http2))
View(method2_data)


### Pregunta 4
# Eligan el grafico que les convenga
tabla_frecuencia <- as.data.frame(table(http_data$Method, http_data$Response_code))
colnames(tabla_frecuencia) <- c("Method", "code", "Freq")
View(tabla_frecuencia)


ggplot(tabla_frecuencia, aes(x = code, y = Freq, fill = Method)) +
  geom_bar(stat = "identity")+
labs(title = "Graph 1")
ggplot(tabla_frecuencia, aes(x = code, y = Freq, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Graph 2")

ggplot(tabla_frecuencia, aes(x = code, y = Freq, color = Method)) +
  geom_line() +
  geom_point() +
labs(title = "Graph 3")




ggplot(tabla_frecuencia, aes(x = "", y = Freq, fill = Method)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "5.Distribución de peticiones por método", fill = "Method") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5))


ggplot(tabla_frecuencia, aes(x = Method, y = code, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "6.Distribución de peticiones por método y código de respuesta")

########Pregunta 5

http_data_filtered <- http_data[, c("Method", "Response_code", "Protocol")]

epa_http_one_hot <- one_hot(as.data.table(http_data_filtered), sparsifyNAs = TRUE)

epa_http_one_hot$Resource_size <- nchar(http_data$Resource)
epa_http_one_hot$Bytes <- http_data$Bytes

View(epa_http_one_hot)

results2 <- kmeans(epa_http_one_hot, centers = 2)
View(results2)
results3 <- kmeans(epa_http_one_hot, centers = 3)

# Pregunta 6

grap1 <- plot(x = epa_http_one_hot$Bytes, y = epa_http_one_hot$Resource_size, col = results2$cluster, main="GRafico con 2")
grap1

grap2 <- plot(x = epa_http_one_hot$Bytes, y = epa_http_one_hot$Resource_size, col = results3$cluster, main="GRafico con 3")
grap2
#termino


