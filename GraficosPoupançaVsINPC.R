library(dplyr)
library(tidyverse)
library(plotly)

INPC <- read.csv("indice.csv", header = T, sep = ";")

INPC10anos <- INPC %>% 
  filter(Ano < 2024 & Ano > 2012)

Poup <- read.csv("table_data.csv", header = T, sep = ",")

Poup10anos <- Poup %>% 
  filter(Ano < 2024 & Ano > 2012)

INPC10anos <- INPC10anos %>% 
  select(c(Ano,TaxaAnual))

resultado <- Poup10anos %>%
  pivot_longer(
    cols = Jan:Dez,  # Define o intervalo de colunas que representam os meses
    names_to = "MES",  # Nome da nova coluna que representará o mês
    values_to = "taxa"  # Nome da coluna que conterá os valores
  ) %>%
  mutate(
    MES = recode(MES,
                 jan = 'Jan',
                 fev = 'Fev',
                 mar = 'Mar',
                 abr = 'Abr',
                 mai = 'Mai',
                 jun = 'Jun',
                 jul = 'Jul',
                 ago = 'Ago',
                 set = 'Set',
                 out = 'Out',
                 nov = 'Nov',
                 dez = 'Dez'),
    taxa = gsub("%", "", taxa),
    taxa = gsub(",", ".", taxa),
    taxa = as.numeric(taxa)
  )

teste <- resultado %>%
  group_by(Ano) %>% 
  summarise(
    TaxaAnual = round(( (1 + mean(taxa))^12 - 1 ),2)
  )

teste2 <- resultado %>% 
  filter()
