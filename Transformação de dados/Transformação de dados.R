############# Transformação de dados #############

#install.packages("xlsx")
install.packages("openxlsx")
library(dplyr)
library(ggplot2)
library(readxl)
library(openxlsx)
#library(xlsx) # Não funcionou como eu esperava essa library, tive de baixar o
# pacote "openxlsx".


## Ler o arquivo da planilha

ilhas <- read_excel("C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Analises_TCC/data_Noronha_Rocas_final.xlsx")

ilhas <- data.frame(ilhas)
class(ilhas)


## DÚVIDA:
# Transformar os dados e depois fazer mediana?
# ou
# Fazer a mediana e depois transformar?

# Acho mais fácil de explicar que transformei os dados e depois fiz tudo com 
# eles transformados, ou seja, primeiro transformar e depois mediana.


## Aprendendo sobre a transformação:

# Fazer a raíz do dado e depois a inversa do seno.

# asin = arcoseno
# sqrt = raíz quadrada
# /100 = pois meu dado está em porcetagem e eu quero ele em fração 

# Essa transformação faz os valores do meio ficarem mais achatados (mais juntos)
# e os valores das pontas mais diferentes. O dado vai de 10 em 10, rodando
# apenas 'asin(sqrt(x1/100))' é possível ver que quando transformado o dado
# vai de 0, 32, 14, 11 ... 11, 14, 33. 
# Para visualizar as diferenças e similaridades entre amostragens é interessante
# antes realizar essa transformação do arcoseno da raíz quadrada com o objetivo 
# de reduzir a influência de organismos abundantes e dos organismos raros. 

# Tenho 2 vetores, um com 10 observações (par) e outro com 11 observações (ímpar)
x1 = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # par
x2 = c(0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # ímpar

# Converte e depois mediana (ler de dentro pra fora - a saída do que está 
# dentro, é a entrada do que está fora)
median(asin(sqrt(x1/100))) # = 0.7853982 (par)
median(asin(sqrt(x2/100))) # = 0.7350587 (ímpar)

# Mediana e depois converte
asin(sqrt(median(x1/100))) # = 0.7853982 (par)
asin(sqrt(median(x2/100))) # = 0.7353145 (ímpar)

# O primeiro e terceiro resultado são iguais, o segundo e o quarto são quase 
# iguais mas um pouco diferentes.

# Relembrando que o cálculo da mediana com número par de observações é feito a
# partir da média dos dois números do meio. Portanto, o ímpar só é convertido,
# então tanto faz converter antes ou depois da mediana. Mas no caso do par, que 
# é a média dos dois valores do meio, a proporção não se mantém quando eu 
# converto antes ou depois, logo, é diferente converter antes ou depois.


## Fazendo com uma coluna para testar o comando da conversão

asin(sqrt(ilhas$calcificadores/100))
# Convertendo pra arcoseno essa coluna que nem está filtrada para confirmar que
# dá pra fazer essa transformação com meu dado

# Espera-se que saia um vetor com valores entre 0 e 1 pois é arcoseno.


## Transformar todas minhas colunas (grupos funcionais) da tabela 'ilhas' pro 
# arcoseno da raíz quadrada

# Função que faz essa transformação (quando quero mudar uma coluna ou inserir 
# uma coluna nova dentro do dplyr, que no caso são 6 colunas novas pois possuo
# 6 grupos funcionais) é 'mutate'.

# mutate(nome que quero dar pra coluna sobrescrevendo ou criando um nome novo = 
# o que é pra ele fazer)

str(ilhas)

ilhas_trans <- ilhas %>% 
  mutate(calcificadores_trans = asin(sqrt(calcificadores/100)),
         macroalgas_trans = asin(sqrt(macroalgas/100)),
         MAE_trans = asin(sqrt(MAE/100)),
         cianobacterias_trans = asin(sqrt(cianobacterias/100)),
         suspensivoros.filtradores_trans = asin(sqrt(suspensivoros.filtradores/100)),
         zoantideo_trans = asin(sqrt(zoantideo/100))) #%>%
#View 

# O pipe pro View abre uma visualização da minha tabela nova sem ela precisar 
# estar no environment.
# Pipe e View comentados por motivos de: salvar a tabela.


## Agrupar de acordo com ano/sítio (análise da vez) usando 'group_by' e depois 
# fazendo a mediana e atribuindo pra uma variável 
# (Código duplicado)

ilhas_trans_med <- ilhas %>% 
  mutate(calcificadores_trans = asin(sqrt(calcificadores/100)),
         macroalgas_trans = asin(sqrt(macroalgas/100)),
         MAE_trans = asin(sqrt(MAE/100)),
         cianobacterias_trans = asin(sqrt(cianobacterias/100)),
         suspensivoros.filtradores_trans = asin(sqrt(suspensivoros.filtradores/100)),
         zoantideo_trans = asin(sqrt(zoantideo/100))) %>%
  group_by(ilha, transecto, sitio, ano) %>%
  summarise(calcificadores_trans_med = median(calcificadores_trans),
            macroalgas_trans_med = median(macroalgas_trans),
            MAE_trans_med = median(MAE_trans),
            cianobacterias_trans_med = median(cianobacterias_trans),
            suspensivoros.filtradores_trans_med = median(suspensivoros.filtradores_trans),
            zoantideo_trans_med = median(zoantideo_trans))
# group_by não funciona sozinho, precisa de um summarise em seguida.


## Exportando em formato excel a planilha dos dados transformados "não 
# medianicos" (para que eu sempre que for fazer um teste estatístico possa dar 
# de entrada essa planilha ao invés de ficar transformando tudo de novo).
write.xlsx(x = ilhas_trans,
           file = "C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Analises_TCC//data_transformed_Noronha_Rocas_final.xlsx",
           sheetName = "arcoseno da raíz quadrada",
           col.names = T,
           row.names = F)

