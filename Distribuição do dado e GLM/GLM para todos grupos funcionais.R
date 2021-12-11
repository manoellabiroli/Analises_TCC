############# GLM para todos grupos funcionais #############

rm(list = ls())

############# Definindo o dado como distribuição beta

library(readxl)
library(dplyr)


### Função para transformar o dado e caber dentro do da distribuição beta
y.transf.betareg <- function(y){ 
  n.obs <- sum(!is.na(y)) 
  (y * (n.obs - 1) + 0.5) / n.obs
}


### Definindo diretório e pegando a planilha
setwd("C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Análises")
dir()
ilhas <- read_excel("data_Noronha_Rocas_final.xlsx")


### Converter minha tabela de porcentagem para decimal:
ilhas_dec <- ilhas %>%
  mutate(calcificadores = calcificadores/100,
         macroalgas = macroalgas/100,
         MAE = MAE/100,
         cianobacterias = cianobacterias/100,
         suspensivoros.filtradores = suspensivoros.filtradores/100,
         zoantideo = zoantideo/100)


### Transformação do dado para não ter valores 0 ou 1:
ilhas_beta <- apply(ilhas_dec[8:13],
                    MARGIN = 2,
                    y.transf.betareg) %>% 
  data.frame

ilhas_beta <- cbind(ilhas[1:7],
                    ilhas_beta)



############# GLM

library(betareg)
library(readxl)
library(dplyr)
library(car)
library(multcomp)


### Mudar as colunas ilha e ano para fator (com o R base):
ilhas_beta$ano <- factor(ilhas_beta$ano)

ilhas_beta_noronha <- ilhas_beta
ilhas_beta_noronha$ilha <- factor(ilhas_beta_noronha$ilha)

ilhas_beta_rocas <- ilhas_beta
ilhas_beta_rocas$ilha <- factor(ilhas_beta_rocas$ilha,
                                levels = c("rocas", "noronha"))

rm(ilhas_beta)


### GLM propriamente dito (usando a função betareg):

### CALCIFICADORES 
## Noronha:
glm_Noronha_interac_cal <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_cal)

summary(glht(model = glm_Noronha_interac_cal,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Rocas:
glm_Rocas_interac_cal <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                                 data = ilhas_beta_rocas)
summary(glm_Rocas_interac_cal)

summary(glht(model = glm_Rocas_interac_cal,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary(glht(model = glm_Rocas_interac_cal,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).



### MACROALGAS
## Noronha:
glm_Noronha_interac_mac <- betareg(formula = macroalgas ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_mac)

summary(glht(model = glm_Noronha_interac_mac,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Rocas:
glm_Rocas_interac_mac <- betareg(formula = macroalgas ~ ano + ilha + ano:ilha,
                                 data = ilhas_beta_rocas)
summary(glm_Rocas_interac_mac)

summary(glht(model = glm_Rocas_interac_mac,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary(glht(model = glm_Rocas_interac_mac,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).



### MAE
## Noronha:
glm_Noronha_interac_mae <- betareg(formula = MAE ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_mae)

summary(glht(model = glm_Noronha_interac_mae,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Rocas:
glm_Rocas_interac_mae <- betareg(formula = MAE ~ ano + ilha + ano:ilha,
                                 data = ilhas_beta_rocas)
summary(glm_Rocas_interac_mae)

summary(glht(model = glm_Rocas_interac_mae,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary(glht(model = glm_Rocas_interac_mae,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).



### CIANOBACTÉRIAS
## Noronha:
glm_Noronha_interac_ciano <- betareg(formula = cianobacterias ~ ano + ilha + ano:ilha,
                                     data = ilhas_beta_noronha)
summary(glm_Noronha_interac_ciano)

summary(glht(model = glm_Noronha_interac_ciano,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Rocas:
glm_Rocas_interac_ciano <- betareg(formula = cianobacterias ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_rocas)
summary(glm_Rocas_interac_ciano)

summary(glht(model = glm_Rocas_interac_ciano,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary(glht(model = glm_Rocas_interac_ciano,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).



### SUSPENSÍVOROS E FILTRADORES
## Noronha:
glm_Noronha_interac_sf <- betareg(formula = suspensivoros.filtradores ~ ano + ilha + ano:ilha,
                                  data = ilhas_beta_noronha)
summary(glm_Noronha_interac_sf)

summary(glht(model = glm_Noronha_interac_sf,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Rocas:
glm_Rocas_interac_sf <- betareg(formula = suspensivoros.filtradores ~ ano + ilha + ano:ilha,
                                data = ilhas_beta_rocas)
summary(glm_Rocas_interac_sf)

summary(glht(model = glm_Rocas_interac_sf,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary(glht(model = glm_Rocas_interac_sf,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).



### ZOANTÍDEO
## Noronha:
glm_Noronha_interac_zoa <- betareg(formula = zoantideo ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_zoa)

summary(glht(model = glm_Noronha_interac_zoa,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Rocas:
glm_Rocas_interac_zoa <- betareg(formula = zoantideo ~ ano + ilha + ano:ilha,
                                 data = ilhas_beta_rocas)
summary(glm_Rocas_interac_zoa)

summary(glht(model = glm_Rocas_interac_zoa,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary(glht(model = glm_Rocas_interac_zoa,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).

