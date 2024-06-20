#-----------------------------------------------
#
# Análise exploratória dos dados de Precipitacao
# PE - 2000 a 2024
#
#-----------------------------------------------

library(lubridate)
library(dplyr)
library(tidyr)

#---- lendo os dados
#- diretorio
setwd("C:\\Users\\belsd\\Desktop\\Epigeodata\\Analise_Temperatura-e-Precipitacao")

#- estacoes

library(readr)
estacoes_PE <- read_delim("dados/estacoes_PE.csv", 
                          delim = ";", escape_double = FALSE, col_types = cols(Latitude = col_character(), 
                                                                               Longitude = col_character(), `Data Inicial` = col_date(format = "%Y-%m-%d"), 
                                                                               `Data Final` = col_date(format = "%Y-%m-%d")), 
                          trim_ws = TRUE)
estacoes_PE = estacoes_PE %>% rename(Codigo_Estacao = `Codigo Estacao`)
estacoes_PE %>% View()
#- dados
dados <- read_delim("dados/temperatura_precipitacao_diario_PE.csv", 
                                                 delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                                 trim_ws = TRUE)
head(dados)
View(dados)
summary(dados)

#pegando o ano acima de 2000:
dados = dados %>% 
  filter(year(Data) >= 2000) %>% 
  mutate(Ano = year(Data),
         Mes = month(Data))
dados %>% View()

#analise por mes-ano

na_counts <- dados %>%
  group_by(Ano, Mes, Codigo_Estacao) %>%
  summarize(Precipitacao_na_count = sum(is.na(Precipitacao )),
            Precipitacao_total_count = n(),
            Precipitacao_percentage_na = round((Precipitacao_na_count/Precipitacao_total_count)*100,2),
            Temperatura_minima_na_count = sum(is.na(Temperatura_minima)),
            Temperatura_minima_total_count = n(),
            Temperatura_minima_percentage_na = round((Temperatura_minima_na_count/Temperatura_minima_total_count)*100,2),
            Temperatura_maxima_na_count = sum(is.na(Temperatura_minima)),
            Temperatura_maxima_total_count = n(),
            Temperatura_maxima_percentage_na = round((Temperatura_maxima_na_count/Temperatura_maxima_total_count)*100,2),
            .groups = 'drop')

na_counts_details_mensal = left_join(na_counts,estacoes_PE , by = "Codigo_Estacao") %>% 
  mutate(id = paste(Latitude,Longitude,Ano,Mes))

write.csv2(na_counts_details_mensal,file="na_counts_details_mensal.csv",sep=";")

#analise por ano
na_counts <- dados %>%
  group_by(Ano, Codigo_Estacao) %>%
  summarize(Precipitacao_na_count = sum(is.na(Precipitacao )),
            Precipitacao_total_count = n(),
            Precipitacao_percentage_na = round((Precipitacao_na_count/Precipitacao_total_count)*100,2),
            Temperatura_minima_na_count = sum(is.na(Temperatura_minima)),
            Temperatura_minima_total_count = n(),
            Temperatura_minima_percentage_na = round((Temperatura_minima_na_count/Temperatura_minima_total_count)*100,2),
            Temperatura_maxima_na_count = sum(is.na(Temperatura_minima)),
            Temperatura_maxima_total_count = n(),
            Temperatura_maxima_percentage_na = round((Temperatura_maxima_na_count/Temperatura_maxima_total_count)*100,2),
            .groups = 'drop')

na_counts_details_anual = left_join(na_counts,estacoes_PE , by = "Codigo_Estacao") %>% 
  mutate(id = paste(Latitude,Longitude,Ano))
na_counts_details_anual %>% View()
write.csv2(na_counts_details_anual,file="na_counts_details_anual.csv",sep=";")

#analise no periodo de estudo
na_counts <- dados %>%
  group_by(Codigo_Estacao) %>%
  summarize(Precipitacao_na_count = sum(is.na(Precipitacao )),
            Precipitacao_total_count = n(),
            Precipitacao_percentage_na = round((Precipitacao_na_count/Precipitacao_total_count)*100,2),
            Temperatura_minima_na_count = sum(is.na(Temperatura_minima)),
            Temperatura_minima_total_count = n(),
            Temperatura_minima_percentage_na = round((Temperatura_minima_na_count/Temperatura_minima_total_count)*100,2),
            Temperatura_maxima_na_count = sum(is.na(Temperatura_minima)),
            Temperatura_maxima_total_count = n(),
            Temperatura_maxima_percentage_na = round((Temperatura_maxima_na_count/Temperatura_maxima_total_count)*100,2),
            .groups = 'drop')

na_counts_details= left_join(na_counts,estacoes_PE , by = "Codigo_Estacao") %>% 
  mutate(id = paste(Latitude,Longitude))
na_counts_details %>% View()

write.csv2(na_counts_details,file="na_counts_details.csv",sep=";")

