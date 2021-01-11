#====================================================
# Title:  BAYESIAN GENERALIZED LINEAR MODELS
# Method: Gaussian
# Case:   Kid's score IQ
# Date:   2021/Jan/11
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
library(readr)
urlfile="https://raw.githubusercontent.com/metodosexatos/mlgbayes/main/DatasetsES15/kidiq.csv"
mydata<-read_csv(url(urlfile)) # para csv no formato brasileiro use read_csv2#head(mydata)
#str(mydata)

#------------
# Histograma da variável dependente:

k <- round(1+3.3*log10(nrow(mydata)),0) # Número de classes: Regra de Sturges
hist(mydata$kid_score, main = "", xlab = "", nclass = k, col = 5)
#------------
# Modelo:

library(rstanarm)

modelo_normal1 <- stan_glm(kid_score ~ mom_hs, data = mydata,
                           family = gaussian(link = "identity"), seed = 12345)

modelo_normal2 <- update(modelo_normal1, formula = . ~ mom_iq)
modelo_normal3 <- update(modelo_normal1, formula = . ~ mom_hs + mom_iq)
(modelo_normal4 <- update(modelo_normal1, formula = . ~ mom_hs * mom_iq))

#------------
# Erro dos coeficientes:

modelo_normal1$ses
modelo_normal2$ses
modelo_normal3$ses
modelo_normal4$ses

#------------
# Estudo de HS:
library(dplyr)
hs1 <- mydata %>% filter(mom_hs == 1)
hs0 <- mydata %>% filter(mom_hs == 0)
mu <- rbind(c(mean(hs0$kid_score), mean(hs0$mom_iq)), 
            c(mean(hs1$kid_score), mean(hs1$mom_iq)))
colnames(mu) <- c("KID-IQ", "MOM-IQ")
rownames(mu) <- c('HS0','HS1')
round(mu,2)






