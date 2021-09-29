############# Box-plots #############

install.packages("scales")
library(readxl)
library(scales)
library(dplyr)

setwd("C:\\Users/manoe/Desktop/TCC/atual/Estatística/Analises_TCC/")
dir()


### IMPORTANDO OS DADOS DE NORONHA:
Noronha <- read_excel(path = "Noronha.xls",
                      sheet = 1,
                      range = "A1:J1041")

View(Noronha)

class(Noronha)

Noronha <- data.frame(Noronha)

class(Noronha)


### BOX-PLOT

# Rapidinho fazer um box-plot bem simples
boxplot(Noronha$calcificadores)

# Pra interpretar o boxplot a função 'stripchart' ajuda.
# Tem que rodar o gráfico (no caso o box-plot ali em cima) antes.
stripchart(Noronha$calcificadores,
           vertical = TRUE,
           add = TRUE,
           pch = 16,
           method = "jitter")
# vertical = TRUE: pois o padrão é fazer de lado e queremos ele de pé.

# add = TRUE: no plot que já existia ele sobrepõe; se ficar rodando de novo e
# de novo essa função, ele vai sobrepondo e vai ficando grossinhos os pontos; tem
# que rodar o box-plot novamente toda vez que mudar algo então.

# pch: é "point character"; é para escolher qual tipo de ponto queremos.

# method = "jitter": espalha os dados, ficando mais vizual; a altura dos pontos 
# é a mesma e a distribuição dos pontos ao longo do eixo x é aleatória.

# Truque: 
# O primeiro parâmetro é muito importante então ele é o primeiro. Os que são
# sempre os mesmo é bom deixar no final e os que são maquiagem deixar no meio.
# Isso porque se um for comentado, o resto ainda funciona.
# Nesse caso, jogar o 'vertical' e o 'add' como últimos. Comentar com hashtag o
# 'method = "jitter"' e colocar uma nova forma de 'method' abaixo dele.
# Não precisando ter o de cima separado, podendo ligar e desligar os comentários.
stripchart(Noronha$calcificadores,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col = alpha("blue", 0.5),
           vertical = TRUE,
           add = TRUE)

# col = (): Com o pacote 'scales' dá para fazer novas formas de pontos,
# configurando de formas diferentes, como colocar transparência nos pontos. O 
# número no parenteses vai de 0 a 1, sendo 0 totalmente transparete e 1
# totalmente opaco.

# INTERPRETANDO O BOX-PLOT:

# Quando há o espaço entre as duas partes do box-plot
# bem achatadinho é pois tem muita amostra lá dentro. Se ele está mais espalhado
# é porque as amostras estão mais distribuidas lá dentro. A linha da mediana pra
# baixo é bem menor do que a linha da mediana pra cima.

# As bolinhas em cima são outliers. Boa parte da minha amostra está abaixo de 20
# mas não chega a quarenta. O sanduíche principal está bem compacto, quanto mais
# próximas essas linhas mais amostra tem ali dentro. E há alguns outliers saindo
# fora. 

# O legal dos box-plots é quando tem mais grupos (anos, por exemplo) e 
# conseguimos observar se tem uma boa sobreposição ou não entre eles. Quanto menos
# sobreposição, mais é possível ver de cara bater o olho e saber se está
# estatísticamente diferente ou não.

# Média ou mediana?
# A média é uma boa medida de centralidade quando a distribuição é normal. Se a 
# distribuição não é normal, a média não é um bom indicativo da centralidade do
# seu dado. E a mediana é o número do meio, havendo a mesma quantidade de
# números acima dela e abaixo dela. 

# Fazendo os demais box-plot:
# Basta copiar o de cima e só mudar uma linha de cada um
boxplot(calcificadores ~ ano,
        data = Noronha,
        las = 2)
# las = 2: parâmetro pra colocar de lado os anos

stripchart(calcificadores ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("blue", 0.5),
           vertical = TRUE,
           add = TRUE)

# INTERPRETANDO: nenhum grupo (ano) tem uma coisa absurdamente diferente da 
# outra pois todos estão mais ou menos na mesma altura a parte do sanduíche
# principal (tem uma grande sobreposição). Há poucas amostras que chegam em 80%,
# maior parte das amostras está entre 10 e 30% na maior parte dos anos.


### IMPORTANDO OS DADOS DE ROCAS TAMBÉM:
dir()

Rocas <- read_excel(path = "Rocas.xlsx",
                    sheet = 1,
                    range = "A1:H839")

Rocas <- data.frame(Rocas)

class(Rocas)


### GRÁFICOS DAS DUAS ILHAS (FINAIS) 


# Para ver valores referentes ao box-plots (média, mediana, 1o e 3o quartil, máx
# e mín, etc):
summary(Rocas)
summary(Noronha)
# Dessa forma ele mostra no geral (sem separar por anos).

Noronha %>%
  filter(ano == 2016) %>%
  summary()
# Filtrando por ano (é só ir mudando o ano para saber cada qual)

Rocas %>%
  filter(ano == 2016) %>%
  summary()


## MAE

# Exportando a imagem com qualidade (tem que colocar aqui no começo):
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/Box-plot/MAE.png",
    width = 1000,
    height = 500)
# Se eu quiser o gráfico alto utilizar 'height = 800', caso eu queira mais 
# quadrado utilizar 'height = 500'.

# Dividindo a tela do plot para parear os gráficos
# par(mfrow=c(1, 2)) #ANTIGO
oldpar <- par(mfrow=c(1,2), #NOVO (mais detalhado sobre as bordas)
              mar=c(3,4,1,2), 
              oma=c(4,3,3,3))
# 'mfrow=c(no. de linhas,no. de colunas)'
# mar = margem de cada plot = 1o no. é a margem de baixo e vai girando no 
# sentindo horário, portanto 'mar=c(margem de baixo, esquerda, cima e direita).
# oma = outside margin = espaço ao redor da imagem total = funciona como o 'mar'.

# Noronha:
boxplot(MAE ~ ano,
        data = Noronha,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "Cobertura (%)",
        main = "Fernando de Noronha")
stripchart(MAE ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("gold", 0.5),
           vertical = TRUE,
           add = TRUE)

# Para colocar a identificação por letras para criar a legenda na monografia:
legend(legend = "(A)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Rocas:
boxplot(MAE ~ ano,
        data = Rocas,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "",
        main = "Atol das Rocas")
stripchart(MAE ~ ano,
           data = Rocas,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("khaki1", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(B)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Ajustes visuais ("maquiagem")
mtext(text = "MAE",
      side = 3,
      line = 1,
      outer = T,
      cex = 2,
      font = 2)
mtext(text= "Ano",
      side = 1,
      line = 1,
      outer = T,
      cex = 1.3,
      font = 1)

# Finalizando para acontecer de fato a exportação da imagem:
dev.off()


## MACROALGAS
# Exportando a imagem com qualidade (tem que colocar aqui no começo):
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/Box-plot/Macroalga.png",
    width = 1000,
    height = 500)

# Dividindo a tela do plot para parear os gráficos
oldpar <- par(mfrow=c(1,2), #NOVO (feito na Aula 8)
              mar=c(3,4,1,2), 
              oma=c(4,3,3,3))

# Noronha:
boxplot(macroalgas ~ ano,
        data = Noronha,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "Cobertura (%)",
        main = "Fernando de Noronha")
stripchart(macroalgas ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("forestgreen", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(C)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Rocas:
boxplot(macroalgas ~ ano,
        data = Rocas,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "",
        main = "Atol das Rocas")
stripchart(macroalgas ~ ano,
           data = Rocas,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("darkolivegreen1", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(D)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Ajustes visuais ("maquiagem")
mtext(text = "Macroalgas",
      side = 3,
      line = 1,
      outer = T,
      cex = 2,
      font = 2)
mtext(text= "Ano",
      side = 1,
      line = 1,
      outer = T,
      cex = 1.3,
      font = 1)

# Finalizando para acontecer de fato a exportação da imagem:
dev.off()


## CALCIFICADORES
# Exportando a imagem com qualidade (tem que colocar aqui no começo):
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/Box-plot/Calcificadores.png",
    width = 1000,
    height = 500)

# Dividindo a tela do plot para parear os gráficos
oldpar <- par(mfrow=c(1,2), #NOVO (feito na Aula 8)
              mar=c(3,4,1,2), 
              oma=c(4,3,3,3))
# Noronha:
boxplot(calcificadores ~ ano,
        data = Noronha,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "Cobertura (%)",
        main = "Fernando de Noronha")
stripchart(calcificadores ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("deeppink1", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(E)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Rocas:
boxplot(calcificadores ~ ano,
        data = Rocas,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "",
        main = "Atol das Rocas")
stripchart(calcificadores ~ ano,
           data = Rocas,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("pink", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(F)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Ajustes visuais ("maquiagem")
mtext(text = "Calcificadores",
      side = 3,
      line = 1,
      outer = T,
      cex = 2,
      font = 2)
mtext(text= "Ano",
      side = 1,
      line = 1,
      outer = T,
      cex = 1.3,
      font = 1)

# Finalizando para acontecer de fato a exportação da imagem:
dev.off()


## CIANOBACTÉRIA

# Exportando a imagem com qualidade (tem que colocar aqui no começo):
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/Box-plot/Cianobacterias.png",
    width = 1000,
    height = 500)

# Dividindo a tela do plot para parear os gráficos
oldpar <- par(mfrow=c(1,2), #NOVO (feito na Aula 8)
              mar=c(3,4,1,2), 
              oma=c(4,3,3,3))

# Noronha:
boxplot(cianobacterias ~ ano,
        data = Noronha,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "Cobertura (%)",
        main = "Fernando de Noronha")
stripchart(cianobacterias ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("dodgerblue2", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(G)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Rocas:
boxplot(cianobacterias ~ ano,
        data = Rocas,
        las = 2,
        ylim=c(0, 100),
        xlab = "Anos",
        ylab = "",
        main = "Atol das Rocas")
stripchart(cianobacterias ~ ano,
           data = Rocas,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("cadetblue1", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(H)",
       x = -0.2,
       y = 107,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Ajustes visuais ("maquiagem")
mtext(text = "Cianobactérias",
      side = 3,
      line = 1,
      outer = T,
      cex = 2,
      font = 2)
mtext(text= "Ano",
      side = 1,
      line = 1,
      outer = T,
      cex = 1.3,
      font = 1)

# Finalizando para acontecer de fato a exportação da imagem:
dev.off()

## ZOANTÍDEOS

# Exportando a imagem com qualidade (tem que colocar aqui no começo):
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/Box-plot/Zoantideos.png",
    width = 1000,
    height = 500)

# Dividindo a tela do plot para parear os gráficos
oldpar <- par(mfrow=c(1,2), #NOVO (feito na Aula 8)
              mar=c(3,4,1,2), 
              oma=c(4,3,3,3))

# Noronha:
boxplot(zoantideo ~ ano,
        data = Noronha,
        las = 2,
        ylim=c(0, 50),
        xlab = "Anos",
        ylab = "Cobertura (%)",
        main = "Fernando de Noronha")
stripchart(zoantideo ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("mediumorchid", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(I)",
       x = -0.2,
       y = 53.5,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Rocas:
boxplot(zoantideo ~ ano,
        data = Rocas,
        las = 2,
        ylim=c(0, 50),
        xlab = "Anos",
        ylab = "",
        main = "Atol das Rocas")
stripchart(zoantideo ~ ano,
           data = Rocas,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("orchid1", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(J)",
       x = -0.2,
       y = 53.5,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Ajustes visuais ("maquiagem")
mtext(text = "Zoantídeos",
      side = 3,
      line = 1,
      outer = T,
      cex = 2,
      font = 2)
mtext(text= "Ano",
      side = 1,
      line = 1,
      outer = T,
      cex = 1.3,
      font = 1)

# Finalizando para acontecer de fato a exportação da imagem:
dev.off()


## SUSPENSÍVOROS E FILTRADORES

# Exportando a imagem com qualidade (tem que colocar aqui no começo):
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/Box-plot/Suspensívoros e Filtradores.png",
    width = 1000,
    height = 500)

# Dividindo a tela do plot para parear os gráficos
oldpar <- par(mfrow=c(1,2), #NOVO (feito na Aula 8)
              mar=c(3,4,1,2), 
              oma=c(4,3,3,3))

# Noronha:
boxplot(suspensivoros.filtradores ~ ano,
        data = Noronha,
        las = 2,
        ylim=c(0, 50),
        xlab = "Anos",
        ylab = "Cobertura (%)",
        main = "Fernando de Noronha")
stripchart(suspensivoros.filtradores ~ ano,
           data = Noronha,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("orange", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(K)",
       x = -0.2,
       y = 53.5,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Rocas:
boxplot(suspensivoros.filtradores ~ ano,
        data = Rocas,
        las = 2,
        ylim=c(0, 50),
        xlab = "Anos",
        ylab = "",
        main = "Atol das Rocas")
stripchart(suspensivoros.filtradores ~ ano,
           data = Rocas,
           pch = 16,
           method = "jitter",
           #method = "stack",
           col= alpha("tan1", 0.5),
           vertical = TRUE,
           add = TRUE)
legend(legend = "(L)",
       x = -0.2,
       y = 53.5,
       text.col = "black",
       cex = 1.3,
       bty = "n")

# Ajustes visuais ("maquiagem")
mtext(text = "Suspensívoros e Filtradores",
      side = 3,
      line = 1,
      outer = T,
      cex = 2,
      font = 2)
mtext(text= "Ano",
      side = 1,
      line = 1,
      outer = T,
      cex = 1.3,
      font = 1)

# Finalizando para acontecer de fato a exportação da imagem:
dev.off()
