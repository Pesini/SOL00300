
# Função que verifica se o pacote pacman está instalado
if (!require("pacman")) install.packages("pacman")

# Função para liberar o pacote se estiver instalado ou baixar se não estiver
pacman::p_load(tidyverse,       # Pacote para importação e tratamento de dados
               readxl,          # Pacote para carregar dados excel
               remotes,         # Download de pacotes fora do CRAN
               RVAideMemoire,   # Pacote com funções estatísticos 
               car,             # Pacote com funções estatísticos 
               psych,           # Pacote com funções estatísticos 
               rstatix,         # Pacote com funções estatísticos 
               DescTools,       # Pacote com funções estatísticos 
               carData,         # Pacote com dados
               agridat,         # Pacote com dados
               emmeans,         # Pacote com funções estatísticos 
               multcomp,        # Pacote com funções estatísticos 
               ggpubr,          # Extensão do ggplot 
               rstatix,         # Pacote com modelos estatísticos 
               MASS,            # Pacote com algumas funções estatísticas 
               conflicted,      # Pacote para preferência a uma função de mesmo nome 
               lmerTest,        # Pacote para teste em modelos mistos 
               AgroR,           # Pacote com modelos estatísticos 
               agricolae,       # Pacote com modelos estatísticos 
               ExpDes.pt)       # Pacote com modelos estatísticos 
                                 

