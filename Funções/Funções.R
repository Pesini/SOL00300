
                           
                             # Funções em R 

# Questionamentos? 
# O que é uma função? 

# f(x) = 5.5 x X 
# preço para abastecer um carro - gasolina

# Soma: soma dos valores de Xi de da seq n1 a n. 
# Média simples: função composta por funções 

# u = sum()/n()

# Uma função é um bloco de código que executa alguma operação.

# exemplos iniciais 
# média

mean(c(13.5, 30, 56))

#Desvio padrão 
sd(c(3,5,6,5,6))


# Utilidade: 

# Reutilização de código 
# Organizar e simplificar o código
# Resolução de problemas 
# Agilidade de operação 


# Uma função é dividida em:
# i)   Argumento - lista de argumentos que controlam como você chama a função
# ii)  Corpo - o código dentro da função
# iii) Ambiente - estrutura que determina como a função encontra os valores

# O ambiente da função sempre existe, 
# mas só é impresso quando a função não está definida no
# global environment.

# Para escrever uma função, deve-se ter clareza das: 
# Entradas
# Saídas 
# Operações 


# Sintaxe das funções 

#nome <- function(argumento1 , … , argumento n) {

#   Comandos da função

#   return (saída)

#   }

# Exemplos 

# Criando uma função para calcular a área de um retângulo 

area_ret <- function(b, h){

  area<- b*h
  return(area)
  
}

# usando a função para calcular área do retângulo 
area_ret(32.5, 23.1)
  

# Usando a função para um conjunto de informações 

base<- 1:15 # vetor
altura <- 36:50 # vetor

# cálculo da área para conjunto de informações 
area_ret(b = base, h = altura)


# Obtendo mais de um parâmetro de uma função
# parâmetros de uma esfera: circunferência, disco, volume

p_esfera <- function(raio){
  circun <- 2*pi*raio
  disco <- pi*raio^2
  volume <- 3/4*pi*raio^3
  saida <- c(circun, disco, volume)
  return(saida)
}

# Usando a função 
p_esfera(45)


# Entrada com conjunto de informações 
# Melhorando a função...
p_esfera <- function(raio){
  circun <- 2*pi*raio
  disco <- pi*raio^2
  volume <- 3/4*pi*raio^3
  saida <- data.frame(circ=circun, area=disco, vol=volume)
  return(saida)
}

# Dados de entrada 
raios <- 20:50

# cálculo da área para conjunto de informações 
p_esfera(raios)

# Função com operadores de condição 
maior = function (a,b) {
  if (a < b) {
    return (b)
  } else { 
    return (a)
  }
}

# Usando a função 
maior(50,10)



                        ##### Usando funções para gráficos #####

# Pacotes 
#install.packages("ggplot2")
#install.packages("carData")
library(ggplot2)
library(carData)

# Dados
dados<-carData::Soils

# Gráfico Violino em ggplot (Densidade de kernel) 
ggplot(dados, aes(x=Contour, y=pH, fill= Contour))+
  geom_violin(show.legend = F)+
  geom_boxplot(fill = "white", width = 0.1, outlier.alpha = 1)+
  labs(y = "pH", x = "Contour")+
  theme_classic()

#Criando função 
violino <- function(df, vind, vdep, xlab, ylab){
  library(ggplot2)
  ggplot(df, aes(x={{vind}}, y={{vdep}}, fill= {{vind}}))+
    geom_violin(show.legend = F)+
    geom_boxplot(fill = "white", width = 0.1, outlier.alpha = 1)+
    labs(x = xlab, y = ylab)+
    theme_classic()
  
}

# Aplicando a função
violino(dados, Contour, pH, "Countour", "pH")
violino(dados, Contour, N, "Countour", "N")
violino(dados, Contour, P, "Countour", "P")
violino(dados, Depth, K, "Depth", "K")

 
# Salvar funções em um script e carregar por aqui 
source("C:/Users/gusta/OneDrive/Aulas R - PPG/Scrips rascunho/funcoes.R")


# Material de apoio 

# Wickham, H. (2015). “Advanced R”, 2ed. Chapman & Hall’s R Series.
# https://adv-r.hadley.nz/index.html


# Wickham, H. et al. (2023).R for Data Science (2e). O’Reilly. 
# http://r4ds.hadley.nz/





                        #### ADICIONAL ####


            #### Agilizando atividades com funções ####

# Ler, juntar, transformar e converter formato de arquivo 

# Pacotes
#install.packages("dplyr")
#install.packages("stringr")
library(dplyr)
library(stringr)

# define a pasta dos arquivos (pasta que contém somente planilhas, neste caso)
pasta <- "C:/Users/gusta/OneDrive/R/Anexos/gabi/dpt_to_csv/NIR_data_ervamate_dpt"

# lista os arquivos dentro da pasta
docs <- list.files(pasta, full.names = T)

# cria a função que vai ler e criar uma coluna com o nome do arquivo
pega_arquivo <- function(arquivo){
  bd <- read.delim(file = arquivo,        # vai ler cada arquivo... 
                    header = F,
                    sep = ",",
                    col.names = c("X", "Y"))
  bd <- mutate(bd, ID = str_sub(basename(arquivo), end = -5)) # cria uma coluna com o nome do arquivo 
  
  return(bd)
}

# repete o processo dentro da lista de arquivo usando a função acima
base <- purrr::map_dfr(docs, pega_arquivo) #Aplica a função para cada arquivo

# transforma para o formato longo (cada amostra em uma coluna)
f_longo<- tidyr::pivot_wider(base,
                             names_from = ID,
                             values_from = Y)

teste <- tidyr::pivot_longer(f_longo, cols = c("1_EM":"99_EM"), # c("Rep_1", )
                             names_to='ID',
                             values_to='pH')



