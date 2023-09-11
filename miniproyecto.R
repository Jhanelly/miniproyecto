library(readxl)
library(dplyr)
install.packages("devtools")
install.packages("DataExplorer")
library(DataExplorer)
library(stringr)
library(readr)
library(openxlsx)
datos <- read_xlsx("directorio_companias.xlsx")

#Explorar si hay valores faltantes en las columnas relevantes
introduce(datos) %>% t()
#Identificando cuantos valores faltantes hay por columnas
colSums(is.na(datos))

#Respaldo de la data original
data <- datos

#Transformando la variable Capital suscrito a una variable numerica
data <- data %>% mutate(CAPITAL=str_replace_all(`CAPITAL SUSCRITO`,pattern="\\.",replacement="")) %>% 
  mutate(CAPITAL=str_replace(CAPITAL, pattern=",",replacement=".")) %>% 
  mutate(CAPITAL=parse_number(CAPITAL,locale=locale(decimal_mark = "."))) 


#comprobando cuantos valores faltantes hay en las empresas activas

data %>% group_by(`SITUACIÓN LEGAL`) %>% 
  summarise(faltantes=sum(is.na(CAPITAL)))

#Debido a que en las empresas activas solo hay 4 valores faltantes, se realizará 
#imputación de datos con la media

data <- data %>%
  group_by(`SITUACIÓN LEGAL`) %>%
  mutate(CAPITAL = ifelse(is.na(CAPITAL), mean(CAPITAL, na.rm = TRUE), CAPITAL)) %>%
  ungroup()

#Verificacion
sum(is.na(data$CAPITAL))

#Exportacion de la data
write.xlsx(data, "directorio.xlsx")
