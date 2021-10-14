############# Normalidade e Homocedasticidade #############

install.packages("car")
library(car)
library(dplyr)
library(readxl)
library(ggplot2)


setwd("C:\\Users/manoe/Desktop/TCC/atual/Estatística/Analises_TCC/")
dir()

# ilhas <- read_excel("data_Noronha_Rocas_final.xlsx",
#                     range = "A1:M1879")

ilhas <- read_excel("data_transformed_Noronha_Rocas_final.xlsx",
                    range = "A1:M1879")

# COM DADOS TRANSFORMADOS: basta substituir a planilha a cima para a planilha que
# contem os dados transformados.

# É interessante depois de transformar olhar os histogramas para ver como ficou 
# a cara do dado e se a transformação é interessante mesmo de ser feita.

# A transformação pode ser para que os dados se encaixem na normalidade e 
# homocedasticidade, mas não é só para isso.

ilhas <- data.frame(ilhas)
class(ilhas)


### Testar premissas da ANOVA:

# Resíduos: a diferença entre o que eu espero dos meus dados e o que eu 
# realmente observo na coleta.

## 1. Normalidade (análise dos resíduos): distribuição tem de ser em formato de
# sino e o teste deve dar p > 0,05

## 2. Homocedasticidade (análise dos resíduos): homocedasticidade das variâncias;
# se as variâncias acompanham ou não a variação dos dados; se a variação ao 
# longo do tempo é continua (= homo; se não for contínua é heterocedástico).
# É mais importante que a normalidade dos meus dados.
# Uma dica é se não há homocedasticidade, pode-se transformar os dados em log,
# mas meu problema é que temos dados que são zero, logo, não podemos transformar
# em log. Já que log de zero é igual a "-infinito".

## 3. Independência entre as amostras: saber se consideramos nossos dados 
# medidas repetitivas ou não. 
# Os exemplo na internet era sobre comprimeto da barbatana de pinguins de spp.
# diferentes, que aí há independência entre as amostras.
# No nosso caso, pesar de termos uma amostragem regular, acreditamos que os dados
# são independentes. Afinal, tem um ano de distância entre as amostras e não é 
# sempre exatamente no mesmo lugar que tiramos o fotoquadrado. Logo, são dados 
# do mesmo lugar (mas não exatamente o “mesmo lugar”), porém em anos diferentes. 


######################### NORONHA
##################### MAE
# Selecionar da tabela qual ilha quero estudar e escolher algum dos grupos 
# funcionais:
mae_noronha <- ilhas %>%
  filter(ilha == "noronha") %>%
  dplyr::select(ano, MAE)
# Tenho os meus dados no environment.

# Transformar ano em fator para não ser tratado como número e sim como grupos:
mae_noronha$ano <- as.factor(mae_noronha$ano)
# Agora sim tenho meu dado de forma adequada.

# Plotar pra ver eles com o arco-iris bonito:
ggplot(mae_noronha) +
  aes(x = ano, 
      y = MAE, 
      color = ano)+
  geom_jitter()

# Escolher um nome pra variável que será o resultado da ANOVA univariada usado 
# para testar normalidade e homocedasticidade
mae_noronha_ANOVA <- aov(MAE ~ ano,
                         data = mae_noronha)


## Normalidade
# Histogramas e o teste Shapiro com resíduos:
hist(mae_noronha_ANOVA$residuals) # A distribuição não é em formato de sino.
shapiro.test(mae_noronha_ANOVA$residuals)
# Não normal (p < 0,05); p = 0.007402.

## Homocedasticidade

# Através do gráfico:
# os dados são homocedásticos se o gráfico de pontos tem a disposição dos pontos
# crescendo continuamente (aparência de diagonal para cima).
qqPlot(mae_noronha_ANOVA$residuals,
       id = F)
# 'id= F' é pro R não identificar o que é cada pontinho.
# Outros gráfico que vão falar coisas parecidas que já vi:
plot(mae_noronha_ANOVA,
     which = 1)
plot(mae_noronha_ANOVA,
     which = 2)

# Através do teste de Levene (esse não pega o resultado da ANOVA para gerar os 
# resultados):
leveneTest(MAE ~ ano,
           data = mae_noronha)
# É homocedástico! Não furou a hipótese nula. Valor de p = 0.7135


##################### Macroalgas

# ANOVA univariada
macr_noronha <- ilhas %>%
  filter(ilha == "noronha") %>%
  dplyr::select(ano, macroalgas)

macr_noronha$ano <- as.factor(macr_noronha$ano)

macr_noronha_ANOVA <- aov(macroalgas ~ ano,
                          data = macr_noronha)


## Normalidade
hist(macr_noronha_ANOVA$residuals)
shapiro.test(macr_noronha_ANOVA$residuals)
# Não normal (p < 0,05); p = 2.2e-16

## Homocedasticidade

# Através do gráfico:
qqPlot(macr_noronha_ANOVA$residuals,
       id = F)
plot(macr_noronha_ANOVA,
     which = 2)

# Através do teste de Levene:
leveneTest(macroalgas ~ ano,
           data = macr_noronha)
# Heterocedásticos (p < 0,05); p = 0.001055


##################### Calcificadores

# ANOVA univariada
cal_noronha <- ilhas %>%
  filter(ilha == "noronha") %>%
  dplyr::select(ano, calcificadores)

cal_noronha$ano <- as.factor(cal_noronha$ano)

cal_noronha_ANOVA <- aov(calcificadores ~ ano,
                         data = cal_noronha)

## Normalidade
hist(cal_noronha_ANOVA$residuals)
shapiro.test(cal_noronha_ANOVA$residuals)
# Não normal (p < 0,05); p = 2.2e-16

## Homocedasticidade

# Através do gráfico:
qqPlot(cal_noronha_ANOVA$residuals,
       id = F)
plot(cal_noronha_ANOVA,
     which = 2)

# Através do teste de Levene:
leveneTest(calcificadores ~ ano,
           data = cal_noronha)


##################### Cianobactérias

# ANOVA univariada
ciano_noronha <- ilhas %>%
  filter(ilha == "noronha") %>%
  dplyr::select(ano, cianobacterias)

ciano_noronha$ano <- as.factor(ciano_noronha$ano)

ciano_noronha_ANOVA <- aov(cianobacterias ~ ano,
                           data = ciano_noronha)

## Normalidade
hist(ciano_noronha_ANOVA$residuals)
shapiro.test(ciano_noronha_ANOVA$residuals)
# Não normal (p < 0,05); p = 2.2e-16

## Homocedasticidade

# Através do gráfico:
qqPlot(ciano_noronha_ANOVA$residuals,
       id = F)
plot(ciano_noronha_ANOVA,
     which = 2)

# Através do teste de Levene:
leveneTest(cianobacterias ~ ano,
           data = ciano_noronha)


##################### Suspensívoros e filtradores

# ANOVA univariada
sf_noronha <- ilhas %>%
  filter(ilha == "noronha") %>%
  dplyr::select(ano, suspensivoros.filtradores)

sf_noronha$ano <- as.factor(sf_noronha$ano)

sf_noronha_ANOVA <- aov(suspensivoros.filtradores ~ ano,
                           data = sf_noronha)

## Normalidade
hist(sf_noronha_ANOVA$residuals)
shapiro.test(sf_noronha_ANOVA$residuals)
# Não normal (p < 0,05); p = 2.2e-16

## Homocedasticidade

# Através do gráfico:
qqPlot(sf_noronha_ANOVA$residuals,
       id = F)
plot(sf_noronha_ANOVA,
     which = 2)

# Através do teste de Levene:
leveneTest(suspensivoros.filtradores ~ ano,
           data = sf_noronha)


##################### Zoantídeos

# ANOVA univariada
zoa_noronha <- ilhas %>%
  filter(ilha == "noronha") %>%
  dplyr::select(ano, zoantideo)

zoa_noronha$ano <- as.factor(zoa_noronha$ano)

zoa_noronha_ANOVA <- aov(zoantideo ~ ano,
                        data = zoa_noronha)

## Normalidade
hist(zoa_noronha_ANOVA$residuals)
shapiro.test(zoa_noronha_ANOVA$residuals)
# Não normal (p < 0,05); p = 2.2e-16

## Homocedasticidade

# Através do gráfico:
qqPlot(zoa_noronha_ANOVA$residuals,
       id = F)
plot(zoa_noronha_ANOVA,
     which = 2)

# Através do teste de Levene:
leveneTest(zoantideo ~ ano,
           data = zoa_noronha)

######################### ATOL DAS ROCAS
##################### MAE

mae_rocas <- ilhas %>%
  filter(ilha == "rocas") %>%
  dplyr::select(ano, MAE)

mae_rocas$ano <- as.factor(mae_rocas$ano)

ggplot(mae_rocas) +
  aes(x = ano, 
      y = MAE, 
      color = ano)+
  geom_jitter()

# ANOVA univariada
mae_rocas_ANOVA <- aov(MAE ~ ano,
                       data = mae_rocas)

## Normalidade:
hist(mae_rocas_ANOVA$residuals)
shapiro.test(mae_rocas_ANOVA$residuals)


## Homocedasticidade (análise dos resíduos)

# Através do gráfico:
qqPlot(mae_rocas_ANOVA$residuals,
       id = F)

# Através do teste de Levene:
leveneTest(MAE ~ ano,
           data = mae_rocas)


##################### Macroalgas

# ANOVA univariada
macr_rocas <- ilhas %>%
  filter(ilha == "rocas") %>%
  dplyr::select(ano, macroalgas)

macr_rocas$ano <- as.factor(macr_rocas$ano)

macr_rocas_ANOVA <- aov(macroalgas ~ ano,
                        data = macr_rocas)

## Normalidade
hist(macr_rocas_ANOVA$residuals)
shapiro.test(macr_rocas_ANOVA$residuals)

## Homocedasticidade

# Através do gráfico:
qqPlot(macr_rocas_ANOVA$residuals,
       id = F)

# Através do teste de Levene:
leveneTest(macroalgas ~ ano,
           data = macr_rocas)


##################### Calcificadores

# ANOVA univariada
cal_rocas <- ilhas %>%
  filter(ilha == "rocas") %>%
  dplyr::select(ano, calcificadores)

cal_rocas$ano <- as.factor(cal_rocas$ano)

cal_rocas_ANOVA <- aov(calcificadores ~ ano,
                       data = cal_rocas)

## Normalidade
hist(cal_rocas_ANOVA$residuals)
shapiro.test(cal_rocas_ANOVA$residuals)

## Homocedasticidade

# Através do gráfico:
qqPlot(cal_rocas_ANOVA$residuals,
       id = F)

# Através do teste de Levene:
leveneTest(calcificadores ~ ano,
           data = cal_rocas)


##################### Cianobactérias

# ANOVA univariada
ciano_rocas <- ilhas %>%
  filter(ilha == "rocas") %>%
  dplyr::select(ano, cianobacterias)

ciano_rocas$ano <- as.factor(ciano_rocas$ano)

ciano_rocas_ANOVA <- aov(cianobacterias ~ ano,
                         data = ciano_rocas)

## Normalidade
hist(ciano_rocas_ANOVA$residuals)
shapiro.test(ciano_rocas_ANOVA$residuals)
# Não normal (p < 0,05); p = 2.2e-16

## Homocedasticidade

# Através do gráfico:
qqPlot(ciano_rocas_ANOVA$residuals,
       id = F)

# Através do teste de Levene:
leveneTest(cianobacterias ~ ano,
           data = ciano_rocas)
