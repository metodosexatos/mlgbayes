#====================================================
# Title:  BAYESIAN GENERALIZED LINEAR MODELS
# Method: Binomial(Logistic)
# Date:   2021/Jan/08
# Author: André Luis M.F. dos Santos
# e-mail: andre@metodosexatos.com.br
# Source: www.metodosexatos.com
#====================================================
# Diretórios e Arquivos:

# getwd() # Qual o diretório que o script está apontando
# list.files() # Quais arquivos estão contidos no diretório
# setwd("C:/Users/andre/Downloads") # muda a pasta de destino
#-------------------------------------------------------------
# Amostra
set.seed(1)
treino <- sample(1:730,365, replace = F)
treino
write.csv(treino,'sample.csv')
