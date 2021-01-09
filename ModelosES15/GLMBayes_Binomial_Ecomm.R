#====================================================
# Title:  BAYESIAN GENERALIZED LINEAR MODELS
# Method: Binomial(Logistic)
# Case:   Ecommerce
# Date:   2021/Jan/08
# Author: André Luis M.F. dos Santos
# e-mail: andre@metodosexatos.com.br
# Source: www.metodosexatos.com
#====================================================
# Pacotes necessários:

# rstanarm
# readr
# dplyr
#------------------------
# Diretórios e Arquivos:

# getwd() # Qual o diretório que o script está apontando
# list.files() # Quais arquivos estão contidos no diretório
# setwd("C:/Users/andre/Downloads") # muda a pasta de destino
#-------------------------------------------------------------
# Leitura de dataset:

# A. opção para ler arquivo salvo no computador
# mydata <- read.csv(file = "wells.csv") 

# B. opção para ler arquivo na web (github)
library (readr)
urlfile="https://raw.githubusercontent.com/metodosexatos/mlgbayes/main/DatasetsES15/ecomm.csv"
mydata<-read_csv2(url(urlfile)) # para csv no formato brasileiro use read_csv2
head(mydata)
#------------
# Subset:
library(dplyr)
amostra <- mydata %>% filter(amostras == "Treino")
#------------
# Modelo:

library(rstanarm)
model_binomial <- stan_glm(status ~ genero+compras+regiao+pagto+qtde+pedido,
                           data = amostra, family = binomial())
summary(model_binomial)
#------------------------
# Interpretação dos coeficientes:

coef <- exp(model_binomial$coefficients)
coef
#---------------------------------------
# Ajuste:

model_binomial$ses                # erro dos coeficientes
model_binomial$residuals          # resíduos
sum(model_binomial$residuals^2)   # erro
#---------------------------------------
# Previsão para novos dados:

genero <- c(0,1,0,1,0,1,0)
compras <- c(1,1,10,3,1,14,2)
regiao <- c(0,1,0,0,0,0,0)
pagto <- c(1,1,1,1,1,0,1)
qtde <- c(12,1,5,4,6,4,1)
pedido <- c(300,800,350,1900,4000,2000,500)
predict_data <- data.frame(genero, compras, regiao, pagto, qtde, pedido)
predict_data[1,]

new_predictions <- posterior_predict(model_binomial, predict_data, draw = 100)
new_predictions[1:10,]
summary(new_predictions[,1])
