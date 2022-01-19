############# Box-plots + GLM #############

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
setwd("C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Analises_TCC")
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



############# GLM + Boxplots

library(betareg)
library(readxl)
library(dplyr)
#library(car)
library(multcomp)
library(ggplot2)
library(ggpubr)
library(ggsignif)


### Função para facilitar a plotagem dos gráficos para os 6 grupos funcionais:

## Função para os pares de box-plots:
faz_boxplot_com_signif <- function(dataset, #dado de entrada que pode ser o data frame de 'noronha' ou 'rocas'.
                                   grupo_funcional, #o grupo funcional escolhido.
                                   pares_anos_signif, #lista em que cada elemento da lista é um vetor que fala qual par de anos foi significativo.
                                   notation_signif, #vetor com as notações do que eu quero que o R plote em cada par significativo.
                                   presenca_titulo = F, #se tem ou não o título do gráfico (bolleano -> tipo de variável TRUE ou FALSE); o padrão é não ter título (caso eu não especificar).
                                   titulo_principal, #qual o título do gráfico.
                                   rotulo_eixoy, #rótulo do eixo y.
                                   cor_jitter, #cor dos pontos de dispersão no gráfico (jitter).
                                   limite_superior = 100, #limite superior do eixo y; o limite superior que será plotado, ou seja, sem a barrinha de significância (breaks).
                                   desloc_limite_superior = 30, #deslocamento do limite superior (espaço dedicado para colocar a barrinha de significância).
                                   presenca_eixox = T){ #se faz ou não eixo x (bolleano); se eu não específicar fará o eixo x.
  
  arg <- match.call() #os argumentos que entram da função (entre os parenteses do 'function').
  
  out_plot <- ggplot(data = dataset,
                     mapping = aes(x = ano,
                                   y = eval(arg$grupo_funcional))) + 
    geom_boxplot(outlier.shape = 1) + 
    geom_jitter(alpha = 0.2,
                width = 0.1,
                col = cor_jitter) + 
    scale_y_continuous(breaks = seq(from = 0,
                                    to = limite_superior,
                                    by = 20),
                       limits = c(-1, limite_superior + desloc_limite_superior),
                       minor_breaks = seq(from = 0,
                                          to = limite_superior,
                                          by = 20))+
    ylab(rotulo_eixoy)+
    theme_minimal()+ #pro fundo do gráfico ser branco.
    theme(plot.margin = unit(c(0.25, 0, 0, 0.2), #para separar o nome do plot e da ilha um pouco no gráfico.
                             units = "cm"),
          plot.title = element_text(margin = margin(t = 0, # para dar um espaço entre o título e o 'bottom' (parte de baixo).
                                                    r = 0,
                                                    b = 0.25,
                                                    l = 0,
                                                    unit = "cm")),
          axis.title.y = element_text(size = 10))
  
  if(length(notation_signif) > 0){
    out_plot <- out_plot + geom_signif(comparisons = pares_anos_signif,
                                       annotations = notation_signif,
                                       y_position = rep(x = limite_superior,
                                                        times = length(notation_signif)) + desloc_limite_superior/2.5)
  }
  
  if(presenca_titulo == T){
    out_plot <- out_plot + ggtitle(titulo_principal)+
      theme(plot.title = element_text(size = 12))
  }
  
  if(presenca_eixox == F){
    out_plot <- out_plot + theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank())
  } else{
    out_plot <- out_plot + theme(axis.title.x = element_text(size = 10))
  }
  
  return(out_plot)
  
} #final da função (fechando com curly brackets).


## Função para o gráfico de segunda dimensão de significância comparando as ilhas (os p-valores):
faz_plot_comp_ilhas_signif <- function(dataset,
                                       eixo_x = "ano",
                                       anos_signif, #coluna da tabela com os anos (não é uma lista com pares como na outra função, é só um vetor).
                                       notation_signif){ #coluna da tabela que tem os asteriscos.
  
  notation_NS <- ifelse(test = notation_signif == "NS", #transformar tudo que é NS do vetor de anotação em vazio (para não plotar no gráfico).
                        yes = "",
                        no = notation_signif)
  
  bars_colors <- ifelse(test = notation_signif == "NS", #mudar as cores das barrinhas para as que forem "NS" serem brancas e as que tiverem significância serem pretas.
                        yes = "white",
                        no = "black")
  
  bars_breaks <- levels(factor(data.frame(dataset[, eixo_x])[,1])) #criando um vetor que terá a posição onde serão colocadas as barrinhas (para lá em baixo efetivamente colocar as barrinhas com as cores).
  
  out_plot <- ggplot(data = dataset,
                     mapping = aes_string(x = eixo_x,
                                          y = 1)) +
    geom_errorbar(aes_string(ymin = 0,
                             ymax = 1,
                             color = eixo_x), #apenas um "place holder" para saber que irei colocar eventualmente cor aqui para pintar as barrinhas de notação de comparação entre as ilhas (pois as barrinhas que forem 'NS' serão brancas para não aparecer no plot final).
                  width = 0.1) +
    scale_color_manual(breaks = bars_breaks, #falar onde no gráfico serão colocadas as barrinhas com as cores que serão especificadas.
                       values = bars_colors) + #especificação das cores.
    ylab("") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(colour = "white"),
          line = element_blank(),
          legend.position = "none") #para não ter legenda nas cores de linhas das barrinhas de comparação de significância entre as ilhas.
  
  if(length(notation_signif) > 0){
    out_plot <- out_plot + annotate(geom = "text",
                                    x = 1:length(notation_NS) - 0.15,
                                    y = 0.5,
                                    label = notation_NS,
                                    size = 4,
                                    hjust = 1)
  }
  
  return(out_plot)
}


### Filtrando a tabela/pegando os dados dos fotoquadrados para cada ilha pra 
# fazer os box-plots (só uma vez serve para todos os grupos pois é a mesma tabela
# de entrada):
noronha <- ilhas %>%
  filter(ilha == "noronha") %>% 
  mutate(ano = as.factor(ano))

rocas <- ilhas %>% 
  filter(ilha == "rocas") %>% 
  mutate(ano = as.factor(ano))


### Mudar as colunas ilha e ano para fator (com o R base) para fazer o GLM:
ilhas_beta$ano <- factor(ilhas_beta$ano)

ilhas_beta_noronha <- ilhas_beta

ilhas_beta_noronha$ilha <- factor(ilhas_beta_noronha$ilha)

ilhas_beta_rocas <- ilhas_beta
ilhas_beta_rocas$ilha <- factor(ilhas_beta_rocas$ilha,
                                levels = c("rocas", "noronha"))

rm(ilhas_beta)


### GLM propriamente dito (usando a função betareg) + plotagem dos boxplots:

### CALCIFICADORES 
## Noronha:
glm_Noronha_interac_cal <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_cal)

summary_noronha_cal <- summary(glht(model = glm_Noronha_interac_cal,
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

summary_rocas_cal <- summary(glht(model = glm_Rocas_interac_cal,
                                  linfct = c("ano2014 = 0", # de 2014 p/ 2013
                                             "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                                             "ano2016 - ano2015 = 0",
                                             "ano2017 - ano2016 = 0",
                                             "ano2018 - ano2017 = 0",
                                             "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary_comp_ilhas_cal <- summary(glht(model = glm_Rocas_interac_cal,
                                       linfct = c("ilhanoronha = 0",
                                                  "ano2014 - ano2014:ilhanoronha = 0", 
                                                  "ano2015 - ano2015:ilhanoronha = 0", 
                                                  "ano2016 - ano2016:ilhanoronha = 0",
                                                  "ano2017 - ano2017:ilhanoronha = 0",
                                                  "ano2018 - ano2018:ilhanoronha = 0",
                                                  "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).


## Criando um data frame que vai ter 4 colunas contendo 2 listas dos anos (para
# comparações), p-valor do GLM e notation (que é a notação para anos com 
# significância) para plotagem do gráfico:

# NORONHA:
comp_noronha_cal <- data.frame(anos1 = as.character(seq(from = 2013,
                                                        to = 2018)),
                               anos2 = as.character(seq(from = 2014,
                                                        to = 2019)),
                               p_value = summary_noronha_cal$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_noronha_cal <- comp_noronha_cal %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_noronha_cal <- as.list(as.data.frame(t(comp_noronha_cal[,c("anos1","anos2")])))


# ROCAS:
comp_rocas_cal <- data.frame(anos1 = as.character(seq(from = 2013,
                                                      to = 2018)),
                             anos2 = as.character(seq(from = 2014,
                                                      to = 2019)),
                             p_value = summary_rocas_cal$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_rocas_cal <- comp_rocas_cal %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_rocas_cal <- as.list(as.data.frame(t(comp_rocas_cal[,c("anos1","anos2")])))


# COMPARANDO AS ILHAS:
comp_ilhas_cal <- data.frame(anos = as.character(seq(from = 2013,
                                                     to = 2019)),
                             p_value = summary_comp_ilhas_cal$test$pvalues[1:7]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
# comp_ilhas_cal <- comp_ilhas_cal %>% 
#   filter(notation != "NS")
# Não tirar os NS p/ a comparação entre as ilhas - como fiz anteriormente para a
# comparação entre os anos dos boxplots - , por motivos de: não deslocar a notação 
# em relação aos anos (tudo é puxado para esquerda - os primeiros anos - quando 
# tira os NS).


## Criando os box-plots:

# Filtragem da tabela/pegando os dados pra fazer os box-plots já foi feita no início
# e não precisa ser feito novamente (pois é a mesma entrada da tabela para todos os grupos).

# Fazer o box-plot propriamente dito:
# Chamada da função pra plotar calcificadores (fazer o mesmo com todos outros grupos funcionais):
plot_noronha_cal <- faz_boxplot_com_signif(dataset = noronha,
                                           grupo_funcional = calcificadores,
                                           pares_anos_signif = par_anos_noronha_cal,
                                           notation_signif = comp_noronha_cal$notation,
                                           presenca_titulo = T,
                                           titulo_principal = "Cobertura calcificadores (%)",
                                           rotulo_eixoy = "Fernando de Noronha",
                                           cor_jitter = "deeppink1",
                                           presenca_eixox = F)

plot_rocas_cal <- faz_boxplot_com_signif(dataset = rocas,
                                         grupo_funcional = calcificadores,
                                         pares_anos_signif = par_anos_rocas_cal,
                                         notation_signif = comp_rocas_cal$notation,
                                         presenca_titulo = F,
                                         rotulo_eixoy = "Atol das Rocas",
                                         cor_jitter = "palevioletred1",
                                         presenca_eixox = T)

# Chamada para plotar só as barrinhas de significância entre as ilhas para calcificadores:
plot_ilhas_cal <- faz_plot_comp_ilhas_signif(dataset = noronha, #pois ele vai dar os anos e só precisamos do eixo x e o eixo x é comum a todos.
                                             anos_signif = comp_ilhas_cal$anos,
                                             notation_signif = comp_ilhas_cal$notation)

# Juntando os plots
# Coloquei no começo uma função que junta os gráficos
ggarrange(plot_noronha_cal,
          plot_ilhas_cal,
          plot_rocas_cal,
          ncol = 1,
          heights = c(4, 1, 4))



### MACROALGAS
## Noronha:
glm_Noronha_interac_mac <- betareg(formula = macroalgas ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_mac)

summary_noronha_mac <- summary(glht(model = glm_Noronha_interac_mac,
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

summary_rocas_mac <- summary(glht(model = glm_Rocas_interac_mac,
                                  linfct = c("ano2014 = 0", # de 2014 p/ 2013
                                             "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                                             "ano2016 - ano2015 = 0",
                                             "ano2017 - ano2016 = 0",
                                             "ano2018 - ano2017 = 0",
                                             "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary_comp_ilhas_mac <- summary(glht(model = glm_Rocas_interac_mac,
                                       linfct = c("ilhanoronha = 0",
                                                  "ano2014 - ano2014:ilhanoronha = 0", 
                                                  "ano2015 - ano2015:ilhanoronha = 0", 
                                                  "ano2016 - ano2016:ilhanoronha = 0",
                                                  "ano2017 - ano2017:ilhanoronha = 0",
                                                  "ano2018 - ano2018:ilhanoronha = 0",
                                                  "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).


## Criando um data frame que vai ter 4 colunas contendo 2 listas dos anos (para 
# comparações), p-valor do GLM e notation (que é a notação para anos com significância - os asteriscos):

# NORONHA
comp_noronha_mac <- data.frame(anos1 = as.character(seq(from = 2013,
                                                        to = 2018)),
                               anos2 = as.character(seq(from = 2014,
                                                        to = 2019)),
                               p_value = summary_noronha_mac$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_noronha_mac <- comp_noronha_mac %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_noronha_mac <- as.list(as.data.frame(t(comp_noronha_mac[,c("anos1","anos2")])))


# ROCAS
comp_rocas_mac <- data.frame(anos1 = as.character(seq(from = 2013,
                                                      to = 2018)),
                             anos2 = as.character(seq(from = 2014,
                                                      to = 2019)),
                             p_value = summary_rocas_mac$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_rocas_mac <- comp_rocas_mac %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_rocas_mac <- as.list(as.data.frame(t(comp_rocas_mac[,c("anos1","anos2")])))


# COMPARANDO AS ILHAS
comp_ilhas_mac <- data.frame(anos = as.character(seq(from = 2013,
                                                     to = 2019)),
                             p_value = summary_comp_ilhas_mac$test$pvalues[1:7]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))


## Criando os box-plots

# Filtrando a tabela/pegando os dados pra fazer os box-plots já no início.

# Fazer o box-plot propriamente dito:
# Chamada de função pra plotar macroalgas (fazer o mesmo com todos outros grupos funcionais):
plot_noronha_mac <- faz_boxplot_com_signif(dataset = noronha,
                                           grupo_funcional = macroalgas,
                                           pares_anos_signif = par_anos_noronha_mac,
                                           notation_signif = comp_noronha_mac$notation,
                                           presenca_titulo = T,
                                           titulo_principal = "Cobertura macroalgas (%)",
                                           rotulo_eixoy = "Fernando de Noronha",
                                           cor_jitter = "forestgreen",
                                           presenca_eixox = F)

plot_rocas_mac <- faz_boxplot_com_signif(dataset = rocas,
                                         grupo_funcional = macroalgas,
                                         pares_anos_signif = par_anos_rocas_mac,
                                         notation_signif = comp_rocas_mac$notation,
                                         presenca_titulo = F,
                                         rotulo_eixoy = "Atol das Rocas",
                                         cor_jitter = "chartreuse2",
                                         presenca_eixox = T)

# Chamada para plotar só as barrinhas de significância entre as ilhas para macroalgas:
plot_ilhas_mac <- faz_plot_comp_ilhas_signif(dataset = noronha, #pois ele vai dar os anos e só precisamos do eixo x e o eixo x é comum a todos.
                                             anos_signif = comp_ilhas_mac$anos,
                                             notation_signif = comp_ilhas_mac$notation)

# Juntando os plots
# Coloquei no começo uma função que junta os gráficos
ggarrange(plot_noronha_mac,
          plot_ilhas_mac,
          plot_rocas_mac,
          ncol = 1,
          heights = c(4, 1, 4))



### MAE
## Noronha:
glm_Noronha_interac_mae <- betareg(formula = MAE ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_mae)

summary_noronha_mae <- summary(glht(model = glm_Noronha_interac_mae,
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

summary_rocas_mae <- summary(glht(model = glm_Rocas_interac_mae,
                                  linfct = c("ano2014 = 0", # de 2014 p/ 2013
                                             "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                                             "ano2016 - ano2015 = 0",
                                             "ano2017 - ano2016 = 0",
                                             "ano2018 - ano2017 = 0",
                                             "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary_comp_ilhas_mae <- summary(glht(model = glm_Rocas_interac_mae,
                                       linfct = c("ilhanoronha = 0",
                                                  "ano2014 - ano2014:ilhanoronha = 0", 
                                                  "ano2015 - ano2015:ilhanoronha = 0", 
                                                  "ano2016 - ano2016:ilhanoronha = 0",
                                                  "ano2017 - ano2017:ilhanoronha = 0",
                                                  "ano2018 - ano2018:ilhanoronha = 0",
                                                  "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).


## Criando um data frame que vai ter 4 colunas contendo 2 listas dos anos,  p-valor
# do GLM e notation (que é a notação para anos com significância - os asteriscos):

# NORONHA
comp_noronha_mae <- data.frame(anos1 = as.character(seq(from = 2013,
                                                        to = 2018)),
                               anos2 = as.character(seq(from = 2014,
                                                        to = 2019)),
                               p_value = summary_noronha_mae$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_noronha_mae <- comp_noronha_mae %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_noronha_mae <- as.list(as.data.frame(t(comp_noronha_mae[,c("anos1","anos2")])))


# ROCAS
comp_rocas_mae <- data.frame(anos1 = as.character(seq(from = 2013,
                                                      to = 2018)),
                             anos2 = as.character(seq(from = 2014,
                                                      to = 2019)),
                             p_value = summary_rocas_mae$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_rocas_mae <- comp_rocas_mae %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_rocas_mae <- as.list(as.data.frame(t(comp_rocas_mae[,c("anos1","anos2")])))


# COMPARANDO AS ILHAS
comp_ilhas_mae <- data.frame(anos = as.character(seq(from = 2013,
                                                     to = 2019)),
                             p_value = summary_comp_ilhas_mae$test$pvalues[1:7]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))


## Criando os box-plots

# Filtrando a tabela/pegando os dados pra fazer os box-plots já no início.

# Fazer o box-plot propriamente dito:
# Chamada de função pra plotar MAE (fazer o mesmo com todos outros grupos funcionais):
plot_noronha_mae <- faz_boxplot_com_signif(dataset = noronha,
                                           grupo_funcional = MAE,
                                           pares_anos_signif = par_anos_noronha_mae,
                                           notation_signif = comp_noronha_mae$notation,
                                           presenca_titulo = T,
                                           titulo_principal = "Cobertura MAE (%)",
                                           rotulo_eixoy = "Fernando de Noronha",
                                           cor_jitter = "gold",
                                           presenca_eixox = F)

plot_rocas_mae <- faz_boxplot_com_signif(dataset = rocas,
                                         grupo_funcional = MAE,
                                         pares_anos_signif = par_anos_rocas_mae,
                                         notation_signif = comp_rocas_mae$notation,
                                         presenca_titulo = F,
                                         rotulo_eixoy = "Atol das Rocas",
                                         cor_jitter = "yellow2",
                                         presenca_eixox = T)

# Chamada para plotar só as barrinhas de significância entre as ilhas para macroalgas:
plot_ilhas_mae <- faz_plot_comp_ilhas_signif(dataset = noronha, #pois ele vai dar os anos e só precisamos do eixo x e o eixo x é comum a todos.
                                             anos_signif = comp_ilhas_mae$anos,
                                             notation_signif = comp_ilhas_mae$notation)

# Juntando os plots
# Coloquei no começo uma função que junta os gráficos
ggarrange(plot_noronha_mae,
          plot_ilhas_mae,
          plot_rocas_mae,
          ncol = 1,
          heights = c(4, 1, 4))



### CIANOBACTÉRIAS
## Noronha:
glm_Noronha_interac_ciano <- betareg(formula = cianobacterias ~ ano + ilha + ano:ilha,
                                     data = ilhas_beta_noronha)
summary(glm_Noronha_interac_ciano)

summary_noronha_ciano <- summary(glht(model = glm_Noronha_interac_ciano,
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

summary_rocas_ciano <- summary(glht(model = glm_Rocas_interac_ciano,
                                    linfct = c("ano2014 = 0", # de 2014 p/ 2013
                                               "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                                               "ano2016 - ano2015 = 0",
                                               "ano2017 - ano2016 = 0",
                                               "ano2018 - ano2017 = 0",
                                               "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary_comp_ilhas_ciano <- summary(glht(model = glm_Rocas_interac_ciano,
                                         linfct = c("ilhanoronha = 0",
                                                    "ano2014 - ano2014:ilhanoronha = 0", 
                                                    "ano2015 - ano2015:ilhanoronha = 0", 
                                                    "ano2016 - ano2016:ilhanoronha = 0",
                                                    "ano2017 - ano2017:ilhanoronha = 0",
                                                    "ano2018 - ano2018:ilhanoronha = 0",
                                                    "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).


## Criando um data frame que vai ter 4 colunas contendo 2 listas dos anos,  p-valor
# do GLM e notation (que é a notação para anos com significância - os asteriscos):

# NORONHA
comp_noronha_ciano <- data.frame(anos1 = as.character(seq(from = 2013,
                                                          to = 2018)),
                                 anos2 = as.character(seq(from = 2014,
                                                          to = 2019)),
                                 p_value = summary_noronha_ciano$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_noronha_ciano <- comp_noronha_ciano %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_noronha_ciano <- as.list(as.data.frame(t(comp_noronha_ciano[,c("anos1","anos2")])))


# ROCAS
comp_rocas_ciano <- data.frame(anos1 = as.character(seq(from = 2013,
                                                        to = 2018)),
                               anos2 = as.character(seq(from = 2014,
                                                        to = 2019)),
                               p_value = summary_rocas_ciano$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_rocas_ciano <- comp_rocas_ciano %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_rocas_ciano <- as.list(as.data.frame(t(comp_rocas_ciano[,c("anos1","anos2")])))


# COMPARANDO AS ILHAS
comp_ilhas_ciano <- data.frame(anos = as.character(seq(from = 2013,
                                                       to = 2019)),
                               p_value = summary_comp_ilhas_ciano$test$pvalues[1:7]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))


## Criando os box-plots

# Filtrando a tabela/pegando os dados pra fazer os box-plots já no início.

# Fazer o box-plot propriamente dito:
# Chamada de função pra plotar cianobactérias (fazer o mesmo com todos outros grupos funcionais):
plot_noronha_ciano <- faz_boxplot_com_signif(dataset = noronha,
                                             grupo_funcional = cianobacterias,
                                             pares_anos_signif = par_anos_noronha_ciano,
                                             notation_signif = comp_noronha_ciano$notation,
                                             presenca_titulo = T,
                                             titulo_principal = "Cobertura cianobactérias (%)",
                                             rotulo_eixoy = "Fernando de Noronha",
                                             cor_jitter = "dodgerblue2",
                                             presenca_eixox = F)

plot_rocas_ciano <- faz_boxplot_com_signif(dataset = rocas,
                                           grupo_funcional = cianobacterias,
                                           pares_anos_signif = par_anos_rocas_ciano,
                                           notation_signif = comp_rocas_ciano$notation,
                                           presenca_titulo = F,
                                           rotulo_eixoy = "Atol das Rocas",
                                           cor_jitter = "deepskyblue",
                                           presenca_eixox = T)

# Chamada para plotar só as barrinhas de significância entre as ilhas para macroalgas:
plot_ilhas_ciano <- faz_plot_comp_ilhas_signif(dataset = noronha, #pois ele vai dar os anos e só precisamos do eixo x e o eixo x é comum a todos.
                                               anos_signif = comp_ilhas_ciano$anos,
                                               notation_signif = comp_ilhas_ciano$notation)

# Juntando os plots
# Coloquei no começo uma função que junta os gráficos
ggarrange(plot_noronha_ciano,
          plot_ilhas_ciano,
          plot_rocas_ciano,
          ncol = 1,
          heights = c(4, 1, 4))



### SUSPENSÍVOROS E FILTRADORES
## Noronha:
glm_Noronha_interac_sf <- betareg(formula = suspensivoros.filtradores ~ ano + ilha + ano:ilha,
                                  data = ilhas_beta_noronha)
summary(glm_Noronha_interac_sf)

summary_noronha_sf <- summary(glht(model = glm_Noronha_interac_sf,
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

summary_rocas_sf <- summary(glht(model = glm_Rocas_interac_sf,
                                 linfct = c("ano2014 = 0", # de 2014 p/ 2013
                                            "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                                            "ano2016 - ano2015 = 0",
                                            "ano2017 - ano2016 = 0",
                                            "ano2018 - ano2017 = 0",
                                            "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary_comp_ilhas_sf <- summary(glht(model = glm_Rocas_interac_sf,
                                      linfct = c("ilhanoronha = 0",
                                                 "ano2014 - ano2014:ilhanoronha = 0", 
                                                 "ano2015 - ano2015:ilhanoronha = 0", 
                                                 "ano2016 - ano2016:ilhanoronha = 0",
                                                 "ano2017 - ano2017:ilhanoronha = 0",
                                                 "ano2018 - ano2018:ilhanoronha = 0",
                                                 "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).


## Criando um data frame que vai ter 4 colunas contendo 2 listas dos anos,  p-valor
# do GLM e notation (que é a notação para anos com significância - os asteriscos):

# NORONHA
comp_noronha_sf <- data.frame(anos1 = as.character(seq(from = 2013,
                                                       to = 2018)),
                              anos2 = as.character(seq(from = 2014,
                                                       to = 2019)),
                              p_value = summary_noronha_sf$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_noronha_sf <- comp_noronha_sf %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_noronha_sf <- as.list(as.data.frame(t(comp_noronha_sf[,c("anos1","anos2")])))


# ROCAS
comp_rocas_sf <- data.frame(anos1 = as.character(seq(from = 2013,
                                                     to = 2018)),
                            anos2 = as.character(seq(from = 2014,
                                                     to = 2019)),
                            p_value = summary_rocas_sf$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_rocas_sf <- comp_rocas_sf %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_rocas_sf <- as.list(as.data.frame(t(comp_rocas_sf[,c("anos1","anos2")])))


# COMPARANDO AS ILHAS
comp_ilhas_sf <- data.frame(anos = as.character(seq(from = 2013,
                                                    to = 2019)),
                            p_value = summary_comp_ilhas_sf$test$pvalues[1:7]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))


## Criando os box-plots

# Filtrando a tabela/pegando os dados pra fazer os box-plots já no início.

# Fazer o box-plot propriamente dito:
# Chamada de função pra plotar suspensívoros e filtradores (fazer o mesmo com todos outros grupos funcionais):
plot_noronha_sf <- faz_boxplot_com_signif(dataset = noronha,
                                          grupo_funcional = suspensivoros.filtradores,
                                          pares_anos_signif = par_anos_noronha_sf,
                                          notation_signif = comp_noronha_sf$notation,
                                          presenca_titulo = T,
                                          titulo_principal = "Cobertura suspensívoros e filtradores (%)",
                                          rotulo_eixoy = "Fernando de Noronha",
                                          cor_jitter = "darkorange3",
                                          presenca_eixox = F,
                                          limite_superior = 60)

plot_rocas_sf <- faz_boxplot_com_signif(dataset = rocas,
                                        grupo_funcional = suspensivoros.filtradores,
                                        pares_anos_signif = par_anos_rocas_sf,
                                        notation_signif = comp_rocas_sf$notation,
                                        presenca_titulo = F,
                                        rotulo_eixoy = "Atol das Rocas",
                                        cor_jitter = "tan1",
                                        presenca_eixox = T,
                                        limite_superior = 60)

# Chamada para plotar só as barrinhas de significância entre as ilhas para macroalgas:
plot_ilhas_sf <- faz_plot_comp_ilhas_signif(dataset = noronha, #pois ele vai dar os anos e só precisamos do eixo x e o eixo x é comum a todos.
                                            anos_signif = comp_ilhas_sf$anos,
                                            notation_signif = comp_ilhas_sf$notation)

# Juntando os plots
# Coloquei no começo uma função que junta os gráficos
ggarrange(plot_noronha_sf,
          plot_ilhas_sf,
          plot_rocas_sf,
          ncol = 1,
          heights = c(4, 1, 4))



### ZOANTÍDEO
## Noronha:
glm_Noronha_interac_zoa <- betareg(formula = zoantideo ~ ano + ilha + ano:ilha,
                                   data = ilhas_beta_noronha)
summary(glm_Noronha_interac_zoa)

summary_noronha_zoa <- summary(glht(model = glm_Noronha_interac_zoa,
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

summary_rocas_zoa <- summary(glht(model = glm_Rocas_interac_zoa,
                                  linfct = c("ano2014 = 0", # de 2014 p/ 2013
                                             "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                                             "ano2016 - ano2015 = 0",
                                             "ano2017 - ano2016 = 0",
                                             "ano2018 - ano2017 = 0",
                                             "ano2019 - ano2018 = 0")))


## Comparando as duas ilhas:
summary_comp_ilhas_zoa <- summary(glht(model = glm_Rocas_interac_zoa,
                                       linfct = c("ilhanoronha = 0",
                                                  "ano2014 - ano2014:ilhanoronha = 0", 
                                                  "ano2015 - ano2015:ilhanoronha = 0", 
                                                  "ano2016 - ano2016:ilhanoronha = 0",
                                                  "ano2017 - ano2017:ilhanoronha = 0",
                                                  "ano2018 - ano2018:ilhanoronha = 0",
                                                  "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).



## Criando um data frame que vai ter 4 colunas contendo 2 listas dos anos,  p-valor
# do GLM e notation (que é a notação para anos com significância - os asteriscos):

# NORONHA
comp_noronha_zoa <- data.frame(anos1 = as.character(seq(from = 2013,
                                                        to = 2018)),
                               anos2 = as.character(seq(from = 2014,
                                                        to = 2019)),
                               p_value = summary_noronha_zoa$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_noronha_zoa <- comp_noronha_zoa %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_noronha_zoa <- as.list(as.data.frame(t(comp_noronha_zoa[,c("anos1","anos2")])))


# ROCAS
comp_rocas_zoa <- data.frame(anos1 = as.character(seq(from = 2013,
                                                      to = 2018)),
                             anos2 = as.character(seq(from = 2014,
                                                      to = 2019)),
                             p_value = summary_rocas_zoa$test$pvalues[1:6]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))

# Tirando o NS p/ Noronha (tem que fazer separado para cada um dos data frames):
comp_rocas_zoa <- comp_rocas_zoa %>% 
  filter(notation != "NS")

# Parear os anos de forma mais automatizada para na chamada do plot não precisar
# ficar escrevendo parzinho por parzinho:
par_anos_rocas_zoa <- as.list(as.data.frame(t(comp_rocas_zoa[,c("anos1","anos2")])))


# COMPARANDO AS ILHAS
comp_ilhas_zoa <- data.frame(anos = as.character(seq(from = 2013,
                                                     to = 2019)),
                             p_value = summary_comp_ilhas_zoa$test$pvalues[1:7]) %>% 
  mutate(notation = case_when(p_value >= 0.05 ~ "NS",
                              p_value < 0.001  ~ "***",
                              p_value < 0.01 ~ "**",
                              p_value >= 0.01 ~ "*"))


## Criando os box-plots

# Filtrando a tabela/pegando os dados pra fazer os box-plots já no início.

# Fazer o box-plot propriamente dito:
# Chamada de função pra plotar zoantídeos (fazer o mesmo com todos outros grupos funcionais):
plot_noronha_zoa <- faz_boxplot_com_signif(dataset = noronha,
                                           grupo_funcional = zoantideo,
                                           pares_anos_signif = par_anos_noronha_zoa,
                                           notation_signif = comp_noronha_zoa$notation,
                                           presenca_titulo = T,
                                           titulo_principal = "Cobertura zoantídeos (%)",
                                           rotulo_eixoy = "Fernando de Noronha",
                                           cor_jitter = "mediumorchid",
                                           presenca_eixox = F,
                                           limite_superior = 60)

plot_rocas_zoa <- faz_boxplot_com_signif(dataset = rocas,
                                         grupo_funcional = zoantideo,
                                         pares_anos_signif = par_anos_rocas_zoa,
                                         notation_signif = comp_rocas_zoa$notation,
                                         presenca_titulo = F,
                                         rotulo_eixoy = "Atol das Rocas",
                                         cor_jitter = "orchid1",
                                         presenca_eixox = T,
                                         limite_superior = 60)

# Chamada para plotar só as barrinhas de significância entre as ilhas para macroalgas:
plot_ilhas_zoa <- faz_plot_comp_ilhas_signif(dataset = noronha, #pois ele vai dar os anos e só precisamos do eixo x e o eixo x é comum a todos.
                                             anos_signif = comp_ilhas_zoa$anos,
                                             notation_signif = comp_ilhas_zoa$notation)

# Juntando os plots
# Coloquei no começo uma função que junta os gráficos
ggarrange(plot_noronha_zoa,
          plot_ilhas_zoa,
          plot_rocas_zoa,
          ncol = 1,
          heights = c(4, 1, 4))



############# Montandos os paineis com 2 gráficos cada

## MAE + macroalga

png(filename = "C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Gráficos/GLM + Box-plots/Paineis dos boxplots/MAE_macroalga.png",
    width = 5*300,
    height = 9*300,
    res = 300)

ggarrange(plot_noronha_mae,
          plot_ilhas_mae,
          plot_rocas_mae,
          plot_noronha_mac,
          plot_ilhas_mac,
          plot_rocas_mac,
          ncol = 1,
          labels = c("A", "", "", "B", "", ""),
          label.x = 0,
          heights = rep(c(6, 1.4, 6),
                        times = 2))

dev.off()


## Calcificadores + cianobactérias

png(filename = "C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Gráficos/GLM + Box-plots/Paineis dos boxplots/Calcificadores_cianobactérias.png",
    width = 5*300,
    height = 9*300,
    res = 300)

ggarrange(plot_noronha_cal,
          plot_ilhas_cal,
          plot_rocas_cal,
          plot_noronha_ciano,
          plot_ilhas_ciano,
          plot_rocas_ciano,
          ncol = 1,
          labels = c("A", "", "", "B", "", ""),
          label.x = 0,
          heights = rep(c(6, 1.4, 6),
                        times = 2))

dev.off()


## Zoantídeos + suspensívoros e filtradores

png(filename = "C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Gráficos/GLM + Box-plots/Paineis dos boxplots/Zoantídeos_sf.png",
    width = 5*300,
    height = 9*300,
    res = 300)

ggarrange(plot_noronha_zoa,
          plot_ilhas_zoa,
          plot_rocas_zoa,
          plot_noronha_sf,
          plot_ilhas_sf,
          plot_rocas_sf,
          ncol = 1,
          labels = c("A", "", "", "B", "", ""),
          label.x = 0,
          heights = rep(c(6, 1.4, 6),
                        times = 2))

dev.off()