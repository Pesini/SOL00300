
#                                  SOL 00300
#                    Tópicos Especiais em Ciência do solo: 
#   Introdução à linguagem de programação R para estudos em Ciência do Solo

# ANOVA e teste de comparação de médias 
# 16 de novembro de 2023
# Gustavo Pesini


# Carregar os pacotes

library(dplyr)
library(RVAideMemoire)
library(car)
library(psych)
library(rstatix)
library(DescTools)
library(carData)
library(agridat)
library(emmeans)
library(multcomp)
library(ggpubr)
library(rstatix)
library(MASS)
library(conflicted)
library(lmerTest)
library(AgroR)
library(agricolae)
library(ExpDes.pt)
library(readxl)

# Preferência à função de mesmo nome 
conflict_prefer("lmer", "lmerTest")


# Dados e pressupostos da ANOVA -------------------------------------------


# Dados
U_V <- PlantGrowth # conjunto de dados do pacote datasets

# níveis
levels(U_V$group)

# obs dos dados 
group_by(U_V, group) |> 
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

#obs dos dados (espacial)
ggplot(U_V, aes(group, weight))+
  geom_boxplot()

#dev.off() # caso der erro na geração do gráfico

# Pressupostos da ANOVA ---------------------------------------------------

# Normalidade 
# Homogeneidade 
# Independência do erro
# ...

# Teste de normalidade - Shapiro-Wilk

# Para dados - Geral
shapiro.test(U_V$weight) 

# Por grupo - pacote RVAideMemoire
byf.shapiro(weight ~ group, U_V) 


## Por grupo pelo pacote rstatix:
U_V |> group_by(group) |> shapiro_test(weight)

#Gráfico de frequência 
hist(U_V$weight)

# Teste da homogeneidade de varianças
## Teste de Levene (pacote car)

leveneTest(weight ~ group, U_V, center = mean)

# Observação:
# Por default, o teste realizado pelo pacote car tem como base a mediana (median)


# Verificação de outliers - além do boxplot
U_V |> 
  group_by(group) |> 
  identify_outliers(weight)

# formula: Q3 + 1.5xIQR or below Q1 - 1.5xIQR (IQR = interquartil)



# Aplicação da ANOVA de uma via -------------------------------------------

# Funcões para gerar a Anova 

# Anova()
# aov()     # Limitada para dados desbalanceados 
# anova()   # Limitada para dados desbalanceados 

# Modelo
anova_dados<- aov(weight ~ group, data = U_V) # ~ em função

# visualizar o resultado da ANOVA
summary(anova_dados)

# Outra forma 
modelo <- lm(weight ~ group, data = U_V) 
summary(modelo)
Anova(modelo)

# Gráficos para analisar os pressupostos
plot(modelo)

# Gráfico 1 - Resíduos (verifica a linearidade e homocedasticidade)
# Gráfico 2 - Normalidade 
# Gráfico 3 - homocedasticidade - linha horizontal 
# Gráfico 4 - Outliers - pontos de alavancagem 


# Post-hocs: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"

# Uso do teste de Duncan - Mais simples
PostHocTest(anova_dados, method = "duncan")

# Uso do teste de TukeyHSD - Médio 
PostHocTest(anova_dados, method = "hsd")

# Uso do teste de Bonferroni - Robusto 
PostHocTest(anova_dados, method = "bonf")


## Resumir os p-values em uma tabela mais de um post-hoc
round(
  cbind(duncan = PostHocTest(anova_dados, method="duncan")$group[,"pval"],
        bonf = PostHocTest(anova_dados, method="bonf")$group[,"pval"],
        hsd = PostHocTest(anova_dados, method="hsd")$group[,"pval"]),
  6)


# Observando os resultados do teste de Tukey com gráfico
TUKEY <- TukeyHSD(x=anova_dados, 'group', conf.level = .95)

#Gráfico
plot(TUKEY , las=1 , col="brown")


# Extraindo as médias 
medias<-emmeans(anova_dados, ~group)
medias

# Adicionando as letras
m_f <- cld(medias, reversed = T, Letters = letters) |> 
  as.data.frame() |> 
  mutate(letters = trimws(.group)) # remove espaços vazios 

m_f

# Gráfico 
ggplot(m_f, aes(group, emmean))+
  geom_pointrange(aes(ymin = emmean-SE, ymax =emmean+SE))+
  geom_text(aes(label = letters, y = emmean+.3))



# ANOVA de duas vias  -----------------------------------------------------

# Pulamos a etapa de verificação dos pressupostos 

# Dados 
dv <- ToothGrowth

# Transformação da variável independente para vetor
dv$dose <- factor(dv$dose, 
                  levels = c(0.5, 1, 2),
                  labels = c("D0.5", "D1", "D2"))

# Visualizando os dados - ggpubr
ggline(dv, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"))

# ANOVA para efeito simples (sinal de +)
es <- aov(len ~ supp + dose, data = dv)
summary(es)

# ANOVA para interação (sinal de *) - analisa efeito simples e interação 
ei <- aov(len ~ supp * dose, data = dv)
summary(ei)

# ANOVA para interação escrito de outra forma
ei1 <- aov(len ~ supp + dose + supp:dose, data = dv)
summary(ei1)

#Extraindo as médias e valores de p 
emm <- emmeans(ei, ~ supp * dose)
simp <- pairs(emm, simple = "each")
simp 

# Adicionando as letras de comparação 
m_f <- cld(emm, reversed = T, Letters = letters) |> 
  as.data.frame() |> 
  mutate(letters = trimws(.group))
m_f

# Gráfico com as letras - interação???
ggplot(m_f, aes(dose, emmean, color = supp))+
  geom_pointrange(aes(ymin = emmean-SE, ymax =emmean+SE))+
  geom_text(aes(label = letters, y = emmean+2))+
  facet_grid(~supp)


# Anova para delineamento de blocos casualizados --------------------------
dados <- read_excel("../SOL00300/Anova_posthoc/rcbd.xlsx")
# Fonte: agRonomy - Ag Data Analytics in R
# https://agronomy.netlify.app/teaching/2020_ksu_designanova/

#observação dos dados
str(dados)

# ordenando e transformando os dados
dados$SR_ksha<- factor(dados$SR_ksha, 
                       levels = c(40, 60, 80, 100, 120),
                       labels = c("40", "60", "80", "100", "120"))

# transformando o bloco em fator
dados$Rep <- as.factor(dados$Rep)

# Modelo - errado!!! 
blocos <- aov(Yield_Mgha~SR_ksha, data = dados)

# saída do modelo errado
summary(blocos)

# plots do modelo errado
plot(blocos)


# Observando os dados
ggplot(dados, aes(SR_ksha, Yield_Mgha))+
  geom_boxplot()+
  geom_point(aes(color = Rep))

#Modelo correto 
mcor <- aov(Yield_Mgha~SR_ksha + Rep, data = dados)
summary(mcor)

plot(mcor)


# Modelo de anova para DBC com mais de um fator 

#Dados
data("mead.turnip")

# Transformando oas variáveis independentes para vetor 
mead.turnip$spacing<-as.factor(mead.turnip$spacing)
mead.turnip$density<-as.factor(mead.turnip$density)
str(mead.turnip)

# Modelo 
doisf<-aov(yield ~ block + spacing*density, data = mead.turnip)

# saída do modelo
summary(doisf)


# Anova para parcelas subdivididas ----------------------------------------

#dados
data("oats")
str(oats)

# observação dos dados 
ggplot(oats, aes(x = N, y = Y, group = V, colour = V))+ 
  geom_line() + 
  facet_wrap(~ B) 

# Modelo (com função aov)
certo <- aov(Y ~ N*V + Error(B/V), data = oats)
summary(certo)

# Modelo com a função lmer do pacote lmerTest
fit.oats <- lmer(Y ~ B + V * N + (1 | B:V), data = oats) 
anova(fit.oats)

# modelo sem definir as parcelas subdivididas - incorreto
oatstes<-aov(Y ~ B+N*V, data = oats)
summary(oatstes)



# Pacotes com funções prontas para ANOVA e Post-hoc -----------------------

# AgroR 
# https://agronomiar.github.io/AgroR_Tutorial/


# agricolae 
#https://cran.r-project.org/package=agricolae


# ExpDes.pt 
# https://cran.r-project.org/web/packages/ExpDes.pt/ExpDes.pt.pdf

# Outros...
# https://agstats.io/post/keeping-up-with-r/
# https://schmidtpaul.github.io/DSFAIR/index.html


# Exemplo com AgroR - DBC em parcela subdividida 
data(tomate)
with(tomate, PSUBDBC(parc, subp, bloco, resp, ylab="Dry mass (g)"))
PSUBDBC

# Exemplo com agricolae - Comparação múltipla de Tukey 
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
HSD.test(model,"virus", group=TRUE,console=TRUE,
         main="Yield of sweetpotato\nDealt with different virus")

# Exemplo com ExpDes.pt - Delineamento em Quadrado Latino
data(ex3)
dql(ex3$trat, ex3$linha, ex3$coluna, ex3$resp, 
    quali = TRUE, mcomp = "snk", sigT = 0.05, sigF = 0.05)



# Material de apoio - Livros ----------------------------------------------


# Data Analysis in R. Capítulo 7: Understanding ANOVA in R
# https://bookdown.org/steve_midway/DAR/understanding-anova-in-r.html


# ANOVA and Mixed Models: A Short Introduction Using R
# https://stat.ethz.ch/~meier/teaching/anova/


# Applied Statistics with R. Capítulo 12: Analysis of Variance
# https://book.stat420.org/analysis-of-variance.html

