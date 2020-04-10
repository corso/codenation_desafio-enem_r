# Definindo a pasta de trabalho
getwd()
setwd("D:/Estudos/Codenation")

# Carregando os pacotes 
library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)

# carregando CSV
dataset <- read.csv("train.csv", sep = ",", encoding = "UTF-8")

# visualizando dados
View(dataset)
str(dataset)

# Cria dataframe somente com o que sera usado na regressao linear
dataframe <- data.frame(
  NU_INSCRICAO = dataset$NU_INSCRICAO,
  CO_MUNICIPIO_RESIDENCIA = dataset$CO_MUNICIPIO_RESIDENCIA,
  CO_UF_RESIDENCIA = dataset$CO_UF_RESIDENCIA,
  NU_IDADE = dataset$NU_IDADE,
  TP_SEXO = dataset$TP_SEXO,
  TP_ESTADO_CIVIL = dataset$TP_ESTADO_CIVIL,
  TP_COR_RACA = dataset$TP_COR_RACA,
  TP_NACIONALIDADE = dataset$TP_NACIONALIDADE,
  TP_ESCOLA = dataset$TP_ESCOLA,
  TP_ENSINO = dataset$TP_ENSINO,
  IN_TREINEIRO = dataset$IN_TREINEIRO, 
  TP_DEPENDENCIA_ADM_ESC = dataset$TP_DEPENDENCIA_ADM_ESC,
  IN_BAIXA_VISAO = dataset$IN_BAIXA_VISAO,
  IN_CEGUEIRA = dataset$IN_CEGUEIRA,
  IN_SURDEZ = dataset$IN_SURDEZ,
  IN_DISLEXIA = dataset$IN_DISLEXIA,
  IN_DISCALCULIA = dataset$IN_DISCALCULIA,
  IN_SABATISTA = dataset$IN_SABATISTA,
  IN_GESTANTE = dataset$IN_GESTANTE,
  IN_IDOSO = dataset$IN_IDOSO,
  Q001 = dataset$Q001,
  Q002 = dataset$Q002,
  Q006 = dataset$Q006,
  Q024 = dataset$Q024,
  Q025 = dataset$Q025,
  Q026 = dataset$Q026,
  Q027 = dataset$Q027,
  Q047 = dataset$Q047,
  TP_LINGUA = dataset$TP_LINGUA,
  TP_PRESENCA_CN = dataset$TP_PRESENCA_CN,
  TP_PRESENCA_CH = dataset$TP_PRESENCA_CH,
  TP_PRESENCA_LC = dataset$TP_PRESENCA_LC,
  TP_PRESENCA_MT = dataset$TP_PRESENCA_MT,
  CO_PROVA_CN = dataset$CO_PROVA_CN,
  CO_PROVA_CH = dataset$CO_PROVA_CH,
  CO_PROVA_LC = dataset$CO_PROVA_LC,
  CO_PROVA_MT = dataset$CO_PROVA_MT,
  NU_NOTA_CN = dataset$NU_NOTA_CN,
  NU_NOTA_CH = dataset$NU_NOTA_CH,
  NU_NOTA_LC = dataset$NU_NOTA_LC,
  NU_NOTA_MT = dataset$NU_NOTA_MT,
  TP_STATUS_REDACAO = dataset$TP_STATUS_REDACAO,
  NU_NOTA_COMP1 = dataset$NU_NOTA_COMP1,
  NU_NOTA_COMP2 = dataset$NU_NOTA_COMP2,
  NU_NOTA_COMP3 = dataset$NU_NOTA_COMP3,
  NU_NOTA_COMP4 = dataset$NU_NOTA_COMP4,
  NU_NOTA_COMP5 = dataset$NU_NOTA_COMP5,
  NU_NOTA_REDACAO = dataset$NU_NOTA_REDACAO,
  TP_ST_CONCLUSAO = dataset$TP_ST_CONCLUSAO
)

View(dataframe)
str(dataframe)

# Mutando valores missing
dfcheck <- data.frame(
  NU_INSCRICAO = dataset$NU_INSCRICAO,
  TP_SEXO = dataset$TP_SEXO,
  Q001 = dataset$Q001,
  Q002 = dataset$Q002,
  Q006 = dataset$Q006,
  Q024 = dataset$Q024,
  Q025 = dataset$Q025,
  Q026 = dataset$Q026,
  Q027 = dataset$Q027,
  Q047 = dataset$Q047
)

# Valores faltando dfcheck
sapply(dfcheck, function(x) sum(is.na(x)))
missmap(dfcheck, main = "Valores Missing Observados")
#dataset <- na.omit(dataset)

# Precisam de conversao factor -> number
dfcheck <- dfcheck %>% 
  dplyr::mutate_at(c('TP_SEXO','Q001','Q002','Q006','Q024','Q025','Q026','Q027','Q047'), as.numeric)

str(dfcheck)

# Repassa ao df principal
dataframe$TP_SEXO <- dfcheck$TP_SEXO
dataframe$Q001 <- dfcheck$Q001
dataframe$Q002 <- dfcheck$Q002
dataframe$Q006 <- dfcheck$Q006
dataframe$Q024 <- dfcheck$Q024
dataframe$Q025 <- dfcheck$Q025
dataframe$Q026 <- dfcheck$Q026
dataframe$Q027 <- dfcheck$Q027
dataframe$Q047 <- dfcheck$Q047

# Valores faltando dataframe
sapply(dataframe, function(x) sum(is.na(x)))
missmap(dataframe, main = "Valores Missing Observados")

str(dataframe)

# Replace NAs para resolver missing
#dfcheck$TESTE <- dataframe$NU_NOTA_COMP1
#dfcheck$TESTE[is.na(dfcheck$TESTE)] <- 0
dataframe$TP_DEPENDENCIA_ADM_ESC[is.na(dataframe$TP_DEPENDENCIA_ADM_ESC)] <- 0
dataframe$TP_ENSINO[is.na(dataframe$TP_ENSINO)] <- 0
dataframe$NU_NOTA_REDACAO[is.na(dataframe$NU_NOTA_REDACAO)] <- 0
dataframe$NU_NOTA_COMP1[is.na(dataframe$NU_NOTA_COMP1)] <- 0
dataframe$NU_NOTA_COMP2[is.na(dataframe$NU_NOTA_COMP2)] <- 0
dataframe$NU_NOTA_COMP3[is.na(dataframe$NU_NOTA_COMP3)] <- 0
dataframe$NU_NOTA_COMP4[is.na(dataframe$NU_NOTA_COMP4)] <- 0
dataframe$NU_NOTA_COMP5[is.na(dataframe$NU_NOTA_COMP5)] <- 0
dataframe$TP_STATUS_REDACAO[is.na(dataframe$TP_STATUS_REDACAO)] <- 0
dataframe$NU_NOTA_MT[is.na(dataframe$NU_NOTA_MT)] <- 0
dataframe$NU_NOTA_LC[is.na(dataframe$NU_NOTA_LC)] <- 0
dataframe$NU_NOTA_CH[is.na(dataframe$NU_NOTA_CH)] <- 0
dataframe$NU_NOTA_CN[is.na(dataframe$NU_NOTA_CN)] <- 0
dataframe$TP_ESTADO_CIVIL[is.na(dataframe$TP_ESTADO_CIVIL)] <- 0


# Pesos notas
mt_peso = 3
cn_peso = 2
lc_peso = 1.5
ch_peso = 1
rd_peso = 3
soma_pesos = mt_peso + cn_peso + lc_peso + ch_peso + rd_peso
soma_pesos



#########################################################################
#          Criacao do modelo de regressao linear - Treino               #
#########################################################################
model <- lm(NU_NOTA_MT ~ CO_UF_RESIDENCIA +
              NU_IDADE +		
              TP_SEXO +
              TP_COR_RACA +
              TP_NACIONALIDADE +
              TP_ESCOLA +
              TP_ENSINO +    
              IN_TREINEIRO +
              TP_DEPENDENCIA_ADM_ESC +
              IN_BAIXA_VISAO +
              IN_CEGUEIRA +
              IN_SURDEZ +     
              IN_DISLEXIA +    
              IN_DISCALCULIA +
              IN_SABATISTA +  
              IN_GESTANTE +   
              IN_IDOSO +
              Q001 +   
              Q002 +  
              Q006 +  
              Q024 +  
              Q025 +  
              Q026 +  
              Q027 +  
              Q047 +  
              TP_LINGUA +
              TP_PRESENCA_CN +
              TP_PRESENCA_CH +
              TP_PRESENCA_LC +
              CO_PROVA_CN +   
              CO_PROVA_CH +  
              CO_PROVA_LC +  
              CO_PROVA_MT +  
              NU_NOTA_CN +  
              NU_NOTA_CH +   
              NU_NOTA_LC +  
              TP_STATUS_REDACAO +
              NU_NOTA_COMP1 + 
              NU_NOTA_COMP2 +
              NU_NOTA_COMP3 +
              NU_NOTA_COMP4 +
              NU_NOTA_COMP5 +
              NU_NOTA_REDACAO +
              TP_ST_CONCLUSAO, 
              data = dataframe)

model

# Salvando o modelo
saveRDS(model, file = "lm_model.rds")

# Carregando o modelo
modelo <- readRDS("lm_model.rds")

###############################################
# Carregando o dataset de teste para predicao #
###############################################

# carregando CSV
dataset_test <- read.csv("test.csv", sep = ",", encoding = "UTF-8")

# visualizando dados
View(dataset_test)
str(dataset_test)

# Mutando valores missing
dfcheck_test <- data.frame(
  NU_INSCRICAO = dataset_test$NU_INSCRICAO,
  TP_SEXO = dataset_test$TP_SEXO,
  Q001 = dataset_test$Q001,
  Q002 = dataset_test$Q002,
  Q006 = dataset_test$Q006,
  Q024 = dataset_test$Q024,
  Q025 = dataset_test$Q025,
  Q026 = dataset_test$Q026,
  Q027 = dataset_test$Q027,
  Q047 = dataset_test$Q047
)

# Valores faltando dfcheck
sapply(dfcheck_test, function(x) sum(is.na(x)))
missmap(dfcheck_test, main = "Valores Missing Observados")


# Precisam de conversao factor -> number
dfcheck_test <- dfcheck_test %>% 
  dplyr::mutate_at(c('TP_SEXO','Q001','Q002','Q006','Q024','Q025','Q026','Q027','Q047'), as.numeric)

# Repassa ao dataset_test
dataset_test$TP_SEXO <- dfcheck_test$TP_SEXO
dataset_test$Q001 <- dfcheck_test$Q001
dataset_test$Q002 <- dfcheck_test$Q002
dataset_test$Q006 <- dfcheck_test$Q006
dataset_test$Q024 <- dfcheck_test$Q024
dataset_test$Q025 <- dfcheck_test$Q025
dataset_test$Q026 <- dfcheck_test$Q026
dataset_test$Q027 <- dfcheck_test$Q027
dataset_test$Q047 <- dfcheck_test$Q047

# Valores faltando dataframe
sapply(dataset_test, function(x) sum(is.na(x)))
missmap(dataset_test, main = "Valores Missing Observados")

str(dataset_test)

# Replace NAs para resolver missing
dataset_test$TP_DEPENDENCIA_ADM_ESC[is.na(dataset_test$TP_DEPENDENCIA_ADM_ESC)] <- 0
dataset_test$TP_ENSINO[is.na(dataset_test$TP_ENSINO)] <- 0
dataset_test$NU_NOTA_REDACAO[is.na(dataset_test$NU_NOTA_REDACAO)] <- 0
dataset_test$NU_NOTA_COMP1[is.na(dataset_test$NU_NOTA_COMP1)] <- 0
dataset_test$NU_NOTA_COMP2[is.na(dataset_test$NU_NOTA_COMP2)] <- 0
dataset_test$NU_NOTA_COMP3[is.na(dataset_test$NU_NOTA_COMP3)] <- 0
dataset_test$NU_NOTA_COMP4[is.na(dataset_test$NU_NOTA_COMP4)] <- 0
dataset_test$NU_NOTA_COMP5[is.na(dataset_test$NU_NOTA_COMP5)] <- 0
dataset_test$TP_STATUS_REDACAO[is.na(dataset_test$TP_STATUS_REDACAO)] <- 0
dataset_test$NU_NOTA_LC[is.na(dataset_test$NU_NOTA_LC)] <- 0
dataset_test$NU_NOTA_CH[is.na(dataset_test$NU_NOTA_CH)] <- 0
dataset_test$NU_NOTA_CN[is.na(dataset_test$NU_NOTA_CN)] <- 0

# Removendo a coluna UF
dataset_test$SG_UF_RESIDENCIA <- NULL

# Incluindo variaveis no dataset de test
#dataset_test$CO_MUNICIPIO_RESIDENCIA <- 0
#dataset_test$TP_ESTADO_CIVIL <- 0
#dataset_test$TP_PRESENCA_MT <- 0

# Guardando o NU_INSCRICAO 
answer <- data.frame(dataset_test$NU_INSCRICAO)

# Movendo o dataset_test para outra variavel
ds_test <- dataset_test

# Removendo a coluna NU_INSCRICAO dele
ds_test$NU_INSCRICAO <- NULL


#####################################################
# Realizando a predicao em cima do dataset de teste #
#####################################################

pred <- predict(model, newdata = ds_test)
pred

answer$NU_NOTA_MT = pred

colnames(answer)[1] <- 'NU_INSCRICAO'
colnames(answer)[2] <- 'NU_NOTA_MT'

# Substituindo valores negativos por zero
answer$NU_NOTA_MT <- ifelse(answer$NU_NOTA_MT < 0, 0, answer$NU_NOTA_MT)

# Gerando o CSV com as respostas
write.csv(answer,
          'answer.csv', 
          row.names = FALSE,
          quote=FALSE)






