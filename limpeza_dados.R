#vamos isntalar os pacotes
install.packages("tidyverse")
#Activar oa pacotes
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(tidyr)
#carregar o ficheiro

dados_sujos_limpeza_df<-read.csv("C:/Users/Administrateur/Downloads/dados_sujos_limpeza.csv")
#Visualizar cabecalho
head<- head(dados_sujos_limpeza_df)
glimpse(dados_sujos_limpeza_df)

#limpeza de dados
dados_limpos <- dados_sujos_limpeza_df %>% distinct(id_venda, .keep_all = TRUE) %>% mutate(produto = str_trim(produto), produto = str_to_sentence(produto), idade = as.integer(idade), idade = if_else(idade > 120 | idade < 0, NA_integer_, idade), preco_unitario_MZN = as.numeric(preco_unitario_MZN))
glimpse(dados_limpos)
dados_limpos <- dados_limpos %>% group_by(produto) %>% mutate(preco_unitario_MZN = if_else(is.na(preco_unitario_MZN),median(preco_unitario_MZN, na.rm = TRUE), preco_unitario_MZN)) %>% ungroup()
glimpse(dados_limpos)

dados_limpos <- dados_limpos %>% mutate(cidade = str_trim(cidade), cidade = str_to_sentence(cidade), cidade = if_else(cidade == "" | is.na(cidade), "Desconhecida", cidade))
dados_limpos <- dados_limpos %>% mutate(email = str_trim(email), email = if_else(email == "" | is.na(email), "Nao tem email", email))
glimpse(dados_limpos)
head(dados_limpos)
dados_limpos <- dados_limpos %>% mutate(data_venda = if_else(data_venda == "" | is.na(data_venda), "NA", data_venda)
dados_limpos <- dados_limpos %>% fill(data_venda, .direction = "down")
glimpse(dados_limpos)
dados_limpos <- dados_limpos %>% mutate(cidade = str_to_title(str_trim(cidade)), nome_cliente = str_to_title(str_trim(nome_cliente)), metodo_pagamento = str_to_title(str_trim(metodo_pagamento)))
glimpse(dados_limpos)
view(dados_limpos)
dados_limpos <- dados_limpos %>% mutate(email = na_if(email, ""), email = if_else(is.na(email), paste0(nome_cliente, "@email.com"), email))
view(dados_limpos)
library(scales)
dados_limpos <- dados_limpos %>% mutate(preco_unitario_MZN = comma(preco_unitario_MZN, accuracy = 0.01))
glimpse(dados_limpos)
view(dados_limpos)
write.csv(dados_limpos, "dados_limpos.csv", row.names = FALSE)
glimpse(dados_limpos)
View(dados_limpos)
