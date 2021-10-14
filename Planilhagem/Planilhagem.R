############# Planilhagem #############

# Pro R apagar só um objeto no environment usar função (limpar o environment):
# 'rm(objeto que eu quero que ele apague)'
# Pro R apagar todos os objetos do environment usar:
# 'rm(list = ls())

# Pro R ler as planilhas do excel sem ter que converter em csv precisa baixar e
# instalar o pacote 'readxl'.

# O pacote 'scales' é para ter 

install.packages("readxl")
library(readxl)
# Normalmente se coloca os 'install' juntos, pois quando for rodar novamente nem
# roda esse começo do código pois ele já vai ter instalado.
# Também geralmente se coloca eles em ordem alfabética para ser mais fácil achar.


# Setar diretório: é melhor setar assim para não ter setar manualmente toda vez
# que entrar no arquivo do R que desejo trabalhar. Dessa forma é só rodar o 
# comando que ele coloca no local junto das planilhas. 
setwd("C:\\Users/manoe/Desktop/TCC/atual/Estatística/Analises_TCC")
dir()


# Colocar a planilha no environmet e já criar um objeto dela.

# Dica: 'alt' e '-' gera automaticamente a flechinha.
# Dica: dar enter+espaço depois de cada comando no parenteses.

# Control espaço na área entre os parenteses mostra todos os parâmetros
# disponíveis que eu posso possivelmente preencher (uns são obrigatório e outros
# não).

# sheet: número da planilha ou o nome dela entre aspas, específicando que folha
# da planilha está sendo usada (o R só lê uma folha por vez).

# range: ele deixa ser escrito no formato do excel de onde a onde eu desejo
# pegar os dados na planilha. Colocar entre aspas, ir lá no excel ver de onde
# até onde eu quero (ver a diagonal).
Noronha <- read_excel(path = "Noronha.xls",
                      sheet = 1,
                      range = "A1:J1041")


# Clicando no nome da tabela dá o equivalente ao comando:
View(Noronha)


# A função 'read_excel' devolve um objeto que não é exatamente um data frame, 
# para vizualizar isso faz o comando:
class(Noronha)
# 'Noronha' está tbl data frame. Tem que converter em somente data frame.

# O que é data frame?
# Objeto de duas dimensões, como uma tabela da vida real, só que ele permite
# eu ter cada coluna de um tipo (exemplo ano como número e sítio como caracter).
# Ele não me permite ter colunas com nomes de números e se 'header = TRUE' ele
# não permite ter colunas com nomes iguais. Além disso, ele preenche com 'NA'
# células de colunas/linhas que tenham tamanhos diferentes.
# A matriz também é um objeto de duas dimensões, mas não me permite ter uma 
# coluna de cada tipo. O R coagiria o dado a todo mundo ser um tipo de dado só
# (no fim das contas tudo pode ser caracter ou número, por exemplo).


# Quando usar aspas e sem aspas?
# As aspas é uma indicação de que estarei colocando o caracter ali. Por exemplo
# procurar alguma coisa no mundo essa coisa que ele nem sabe o que é ainda, como
# fizemos em instalas pacotes. Depois de instalado o objeto já está em algum
# lugar, já é uma variavel, então vou me referir a ela sem aspas. Chamar colunas
# pelo nome usamos aspas (salvo algumas exceções).


# Sobrescrever 'Noronha' tbl por 'Noronha' só data frame.
Noronha <- data.frame(Noronha)
# verificar se está só data frame:
class(Noronha)


### FATIAR DATA FRAME/TABELA:

# Jeito mais comum de fatiar é usando o '$'. (O '~' é uma função, usei no meu 
# boxplot pois é uma fórmula, como "y em função de x" = "y ~ x").

# 'Noronha$' seguido de control espaço o R vai me deixar ver tudo que há de opção.
Noronha$ano
# Isso é igual "tabela$coluna".
# O R transformou em um vetor, que é um objeto que só tem uma dimensão e que vai
# guardar valores do mesmo tipo (por isso só coloca um elemento entre colchetes).

# O entre colchetes no meu console a esquerda é a posição do elemento. Para, por
# exemplo, saber o número que está na posição 327:
Noronha$ano[327]
# É o ano de 2015.
# Se colocar um número maior do que tem o R dá erro ou 'NA':
Noronha$ano[579984839202020]

# Como usar mais colchetes?
# Colchete funciona para pedir subsessões daquela coisa.
# Como 'Noronha' é um data frame de duas dimensões, separar por vírgula linha e 
# coluna desse data frame = 'Noronha[linha,coluna]'

# Pegar 10 primeiras linhas da coluna 3:
Noronha[1:10,3]
# Ele devolve os 10 elementos

# Elemento da linha 1000 com a coluna 5:
Noronha[1000,5]
# Ele pode devolver uma coisa só (tipo batalha naval).

# 10 primeiras linhas de todas colunas:
Noronha[1:10,]
# Para todas colunas basta não especificar o valor da coluna após a vírgula 
# deixando em branco.

# Todas as linhas da coluna 3:
Noronha[,3]

# Especificando por nome a coluna ao invés de dizer a posição dela na planilha:
Noronha[,"calcificadores"]

# Pulando a coluna 2:
Noronha[,c(1,3,4,5)]
# c = concatenar.
# Também pode colocar entre aspas o nome das colunas que eu quero, por exemplo:
Noronha[,c("calcificadores", "macroalgas")]


### FILTRAR TABELA:

# É diferente de fatiar, pois quando fatiamos sabemos o que queremos, quando
# filtramos não sabemos exatamente as que serão escolhidas, mas queremos que
# todas obedeçam a uma mesma condição.

# tabela$coluna:
Noronha$MAE

# Quero valores que são maiores do que 60 nessa coluna:
Noronha$MAE > 60
# Apareceu tudo TRUE ou FALSE, ou seja, os dados maiores que 60 são equivalentes
# a TRUE e os menores a FALSE. Usar isso como um filtro de linha, pois pegar só
# os os TRUES são as linhas que obedeceram à nossa condição.

# Colocar dentro de colchete que significa '[linha,coluna]' para filtrar linha
# e queremos todas as colunas. E como é um filtro da tabela 'Noronha' tem que
# colocar no começo que é essa tabela.
Noronha[Noronha$MAE > 60,]
# Filtro que só levou em conta a coluna MAE, mas não deixou de mostrar as outras
# colunas, pois pedimos todas as colunas.

# Combinar os filtros.
# tabela[(condição de linha 1) & (condição de linha 2) , coluna]
# & = combina condições.
# '=' pro R é uma atribuição igual a setinha, usado para passar parametro e não
# se lê como "igual", se lê como "recebe".
# '==' pro R significa que algo é igual (tal coisa igual a outra coisa). 
Noronha[(Noronha$cianobacterias > 50) & (Noronha$sitio == "cagarras") ,]
# Para ver o que estamos rodando pintar só com o mouse o que está dentro do 
# paranteses que queremos ver e dar control enter.

# Notação útil de saber:
# Quando quero algumas linhas (assim como fizemos com colunas).
# O R não ajuda sempre da forma que queremos quando usamos '=='seguido do vetor
# da coisa que eu quero. Usar '==' não dá bom quando se tem um vetor, tem de
# usar o '%in%'
# Todas as linhas em que o ano seja 2013 ou 2019:
Noronha$ano == c(2013,2019) # Dá ruim, ele fica buscando a padronização
# "2013 2019" nos dados, ao invés de buscar quais são os anos de 2013 e de 2019.
Noronha$ano %in% c(2013,2019) # Mostrou TRUE para, separadamente, ambos anos.


### DIFERENÇA ENTRE VARIÁVEL E VETOR

# Variavél: nome genérico de todas coisas que eu defino e vão parar lá no meu
# ambiente. Se eu chamar a=1, minha variavel é 'a' e dentro dela mora o número 1
# e fazendo 'class' desse dado eu vejo a natureza dele, que será numérico. 
a = 1
class(a)

# Vetor: é um objeto de uma dimensão e que pode ter vários valores dentro, sendo
# todos do mesmo tipo 'a <- c(1,2)' estou declarando um vetor (isso é igual a
# recebe c).
a <- c(1,2)
class(a)
# str = structure de a; essa função indica que é um vetor pois há duas posições
str(a)
# Se eu fizer com o 2 entre aspas e depois ver a classe
a <- c(1,"2")
class(a)
# O vetor será um carater (character), pois o R coage o dado a ser todo do mesmo
# tipo, logo, se te um string, todo mundo já virou esse mesmo string.

# DICA: 'dev.off()' é como passar a régua e zerar tudo que já fez. Se a tela dos
# gráficos estiver dividida em dois, por exemplo, esse comando zera e faz voltar
# a ser só uma.
dev.off()

