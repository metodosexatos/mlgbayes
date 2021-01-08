#====================================================
# Title:  BAYESIAN GENERALIZED LINEAR MODELS
# Method: Binomial(Logistic)
# Date:   2021/Jan/08
# Author: André Luis M.F. dos Santos
# e-mail: andre@metodosexatos.com.br
# Source: www.metodosexatos.com
#====================================================
# Pacotes necessários:

# rstanarm
# readr
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
urlfile="https://raw.githubusercontent.com/metodosexatos/mlgbayes/main/DatasetsES15/wells.csv"
mydata<-read_csv(url(urlfile)) # para csv no formato brasileiro use read_csv2
head(mydata)
#------------
# Modelo:

library(rstanarm)
model_binomial <- stan_glm(switch ~ dist + arsenic + assoc + educ,
                           data = mydata, family = binomial())
summary(model_binomial)
#------------------------
# Interpretação dos coeficientes:

coef <- exp(model_binomial$coefficients)
coef

