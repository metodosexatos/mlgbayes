#====================================================
# Title:  BAYESIAN GENERALIZED LINEAR MODELS
# Method: Poisson
# Case:   Ecommerce
# Date:   2021/Jan/10
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
# mydata <- read.csv(file = "ecomm.csv") 

# B. opção para ler arquivo na web (github)
library (readr)
urlfile="https://raw.githubusercontent.com/metodosexatos/mlgbayes/main/DatasetsES15/folhas.csv"
mydata<-read_csv2(url(urlfile)) # para csv no formato brasileiro use read_csv2#head(mydata)
#------------
# Histograma da variável dependente:

k <- round(1+3.3*log10(nrow(mydata)),0) # Número de classes: Regra de Sturges
hist(mydata$folhas, main = "Valores observados", xlab = "folhas", nclass = k, col = 5)

#--------
# Modelo:

library(rstanarm)

model_poisson <- stan_glm(folhas ~ dist, data = mydata, family = poisson())
summary(model_poisson)

coeff <- exp(model_poisson$coefficients)
coeff

#------------
# Histograma da distribuição posterior:

k <- round(1+3.3*log10(nrow(posterior_predict(model_poisson))),0) # Número de classes: Regra de Sturges
hist(posterior_predict(model_poisson), main = "Posterior", xlab = "folhas", nclass = k, col = 5)


