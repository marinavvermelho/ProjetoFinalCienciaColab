

# Atividade 01

# Antes de começar precisei substituir as vírgulas por pontos nas planilhas originais #

# Definindo diretório #
setwd("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2")

# Pacotes #

library(tidyverse)
library(dplyr)
library(lubridate)

# Importando e organizando tabela Vitor #

tab_Vi <- read.csv("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2/atividade1_Vitor_Figueira_Arueira.csv", sep = ";")

# Renomeando colunas #
names(tab_Vi)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

# Reordenando colunas #
attach(tab_Vi)
tab_Vi2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)


# Importando e organizando tabela Lorrana #


tab_Lo <- read.csv("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2/Atividade1_Lorrana.csv", sep = ";")

# Renomeando colunas #
names(tab_Vi)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

# Reordenando colunas #
attach(tab_Lo)
tab_Lo2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)



# Importando e organizando tabela Marina #

tab_Ma <- read.csv("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2/atividade1_MARINA.csv", sep = ";")

# Renomeado colunas #
names(tab_Ma)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

# Reordenando colunas #
attach(tab_Ma)
tab_Ma2 = cbind(amostra, spp, data,site,latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)


# Importando e organizando tabela Henrique #

tab_He <- read.csv("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2/atividade1_HenriqueSimfrone.csv", sep = ";")

# Renomeando colunas #
names(tab_He)[1:10] <- c("amostra", "spp", "tamanho_sepala","largura_sepala", "tamanho_petala", "largura_petala", "site", "longitude", "latitude","data")

# Reordenando colunas #
attach(tab_He)
tab_He2 = cbind(amostra, spp, data,site,latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)


# Importando e organizando tabela Mariana #

tab_Mab <- read.csv("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2/atividade1_MARIANA-BURATO.csv", sep = ";")

# Renomeando colunas #
names(tab_Mab)[1:10] <- c("amostra", "site", "latitude", "longitude", "data", "spp", "tamanho_petala", "largura_petala", "tamanho_sepala", "largura_sepala")

# Reordenando colunas #
attach(tab_Mab)
tab_Mab2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)


# Importando e organizando tabela Jonatha #

tab_Jo <- read.csv("D:/Doutorado/Disciplinas/Ciencia Colaborativa/Atividades 1 e 2/atividade1_JonathaR.csv", sep = ";")

# Renomeando colunas #
names(tab_Jo)[1:10] <- c("amostra", "spp", "tamanho_sepala", "largura_sepala", "tamanho_petala", "largura_petala", "site","latitude", "longitude", "data")

# Reordenando colunas #
attach(tab_Jo)
tab_Jo2 = cbind(amostra, spp, data,site, latitude, longitude, tamanho_petala, largura_petala, tamanho_sepala, largura_sepala)

# Unindo tabelas #
dados <- rbind(tab_He2, tab_Lo2, tab_Ma2, tab_Mab2, tab_Vi2, tab_Jo2)

# Criando data.frame #
dados <- as.data.frame(dados)


# Padronizando nomes das espécies #

dados$spp[dados$spp == "IRIS_VERSICOLOR"] <- "Iris versicolor"
dados$spp[dados$spp == "IRIS_VIRGINICA"] <- "Iris virginica"
dados$spp[dados$spp == "IRIS_SETOSA"] <- "Iris setosa"
dados$spp[dados$spp == "iris_versicolor"] <- "Iris versicolor"
dados$spp[dados$spp == "iris_virginica"] <- "Iris virginica"
dados$spp[dados$spp == "iris_setosa"] <- "Iris setosa"
dados$spp[dados$spp == "Iris_versicolor"] <- "Iris versicolor"
dados$spp[dados$spp == "Iris_virginica"] <- "Iris virginica"
dados$spp[dados$spp == "Iris_setosa"] <- "Iris setosa"

# Padronizando sites #

dados$site[dados$site == "S3"] <- "Site3"
dados$site[dados$site == "S2"] <- "Site2"
dados$site[dados$site == "S1"] <- "Site1"
dados$site[dados$site == "site3"] <- "Site3"
dados$site[dados$site == "site2"] <- "Site2"
dados$site[dados$site == "site1"] <- "Site1"


# Padronizando formato das datas #

dados$data[dados$data == "01/12/1929"] <- "01_12_1929"
dados$data[dados$data == "13/02/1930"] <- "13_02_1930"
dados$data[dados$data == "1929_12_01"] <- "01_12_1929"
dados$data[dados$data == "1930_02_13"] <- "13_02_1930"

# Convertendo a coluna data com classe "factor" para "date" e padronizando o formato para YYY-MM-DD #

dados$data <- dmy(dados$data)
is.Date(dados$data)
class(dados$data)

# Ordenando a planilha pela coluna "spp" #


dados<-dados[order(dados$spp),]

# Visualizando a planilha #

View(dados)

# Exportando em formato .csv #

write.csv2(dados, "TabelaFinal_MarinaVermelho.csv")
