#====================================================
# Title:  BAYESIAN GENERALIZED LINEAR MODELS
# Method: Gamma
# Case:   Clotting
# Date:   2021/Jan/11
# Author: André Luis M.F. dos Santos
# e-mail: andre@metodosexatos.com.br
# Source: www.metodosexatos.com
#====================================================
# Base de dados:

# A.Variáveis originais

u <- c(5,10,15,20,30,40,60,80,100)          # concentração de plasma (%)
lot1 <- c(118,58,42,35,27,25,21,19,18)      # tempo de coagulação (s)
lot2 <- c(69,35,26,21,18,16,13,12,12)       # tempo de coagulação (s)

#- B.Transformação das variáveis

log_plasma <- rep(log(u), 2)                      # logarítmo da variável u
clot_time <- c(lot1, lot2)                        # concatenando lot1 e lot2
lot_id <- factor(rep(c(1,2), each = length(u)))   # se: lot1=1; lot2=2

#- C.Dataframe

mydata <- data.frame(log_plasma, clot_time, lot_id)
#------------------------------------------------------
# Modelo com interação:

library(rstanarm)
model_gamma <- stan_glm(clot_time ~ log_plasma * lot_id, data = mydata,
                        family = Gamma, seed = 12345)
print(model_gamma, digits = 3)
#------------------------------------------------------
# Modelo sem interação:

model_gamma1 <- update(model_gamma, formula = . ~ log_plasma)
model_gamma2 <- update(model_gamma, formula = . ~ log_plasma + lot_id)
#------------------------------------------------------
# Coeficientes:

coef1 <- rbind(model_gamma1$coefficients[1],exp(model_gamma1$coefficients[2]))
row.names(coef1) <- c("Intercepto", "Plasma")
colnames(coef1) <- "Modelo1"
coef1

coef2 <- rbind(model_gamma2$coefficients[1],exp(model_gamma2$coefficients[2]), model_gamma2$coefficients[3])
row.names(coef2) <- c("Intercepto", "Plasma", "Lote")
colnames(coef2) <- "Modelo2"
coef2
merge(coef1, coef2, by="row.names", all = TRUE)
