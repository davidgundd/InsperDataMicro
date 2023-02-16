library(tidyverse)
library(Formula)
library(performance)
library(see)
library(patchwork)
library(ggthemes)
library(gridExtra)
library(stringr)

#################Importando os dados###########################################

dados <- readxl::read_excel('base_cr.xlsx')

##################Agregando as engenharias numa só e colocando dummies de gênero
#############################
dados <- dados %>% 
  mutate(CURSO = ifelse(Curso == "ENGENHARIA DE COMPUTAÇÃO"|
                            Curso=="ENGENHARIA MECATRÔNICA"|
                            Curso=="ENGENHARIA MECÂNICA", 
                          "ENGENHARIAS", Curso)) %>% 
  mutate(d_gen = ifelse(Sexo == "F", "1", "0")) %>% 
  mutate(CR = as.numeric(CR))

#################Filtrando a partir do 2 período, pois inclui direito###########
#########################Histograma dos CRs#####################################

dados %>% 
  filter(Periodo == 2) %>% 
  ggplot()+
  geom_density(aes(x = as.numeric(CR), fill = CURSO,
                   alpha = 0.8)) +
  guides(alpha = "none") +
  labs(x = "CR",
       y = "Densidade",
       title = 
         "Histograma de CR dos cursos") +
  theme(legend.position = "right") +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 13) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

#####################Histogramas dos CRs por Curso##############################

############################ECO e ADM###########################################

dados %>% 
  filter(Periodo==2 & Curso == "ADMINISTRAÇÃO" |
           Curso == "CIÊNCIAS ECONÔMICAS") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Curso, alpha = 0.8)) +
  guides(alpha = "none") +
  labs(x = "CR",
       y = "Densidade",
       title = 
         "Histograma CR Economia e Administração") +
  scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

##################################ENGENHARIAS###################################

dados %>% 
  filter(Periodo==2 & Curso == "ENGENHARIA MECÂNICA" |
           Curso == "ENGENHARIA DE COMPUTAÇÃO" |
           Curso == "ENGENHARIA MECATRÔNICA") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Curso, alpha = 0.8))+
  guides(alpha ="none") +
  labs(x = "CR",
       y = "Densidade",
       title = 
         "Histograma CR Engenharias") +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

####################################DIREITO##################################### 

dados %>% 
  filter(Periodo==2 & Curso == "DIREITO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR))) +
  labs(x = "CR",
       y = "Densidade",
       title = 
         "Histograma CR Direito") +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 12) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

######################CONTAGEM E FREQUÊNCIA DE GÊNERO POR CURSO#################

#################################ADMINISTRAÇÃO################################## 

freq_adm <- prop.table(table(dados$Sexo[dados$Curso=="ADMINISTRAÇÃO"]))
freq_adm <- as.data.frame(freq_adm)

colnames(freq_adm) <- c("Sexo","Porcentagem")

cont_adm <- ggplot(freq_adm,aes(x = Sexo, y = Porcentagem, fill = Sexo)) + 
  geom_col() + 
  scale_fill_hue(l=55, c=95) +
  labs(x = "",
       y = "",
       title = 
         "Administração") +
  scale_y_continuous(n.breaks = 13, labels = scales::percent) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color= NA))

cont_adm
  
###################################ECONOMIA#####################################

freq_eco <- prop.table(table(dados$Sexo[dados$Curso=="CIÊNCIAS ECONÔMICAS"]))
freq_eco <- as.data.frame(freq_eco)

colnames(freq_eco) <- c("Sexo","Porcentagem")

cont_eco <- ggplot(freq_eco,aes(x = Sexo, y = Porcentagem, fill = Sexo)) + 
  geom_col() + 
  scale_fill_hue(l=55, c=95) +
  labs(x = "",
       y = "",
       title = 
         "Economia") +
  scale_y_continuous(n.breaks = 10, labels = scales::percent) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color= NA))

cont_eco

#################################ENGENHARIAS####################################

freq_eng <- prop.table(table(dados$Sexo[dados$CURSO=="ENGENHARIAS"]))
freq_eng <- as.data.frame(freq_eng)

colnames(freq_eng) <- c("Sexo","Porcentagem")

cont_eng <- ggplot(freq_eng,aes(x = Sexo, y = Porcentagem, fill = Sexo)) + 
  geom_col() + 
  scale_fill_hue(l=55, c=95) +
  labs(x = "",
       y = "",
       title = 
         "Engenharias") +
  scale_y_continuous(n.breaks = 12, labels = scales::percent) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color= NA))

cont_eng

################################DIREITO#########################################

freq_dir <- prop.table(table(dados$Sexo[dados$Curso=="DIREITO"]))
freq_dir <- as.data.frame(freq_dir)

colnames(freq_dir) <- c("Sexo","Porcentagem")

cont_dir <- ggplot(freq_dir,aes(x = Sexo, y = Porcentagem, fill = Sexo)) + 
  geom_col() + 
  scale_fill_hue(l=55, c=95) +
  labs(x = "",
       y = "",
       title = 
         "Direito") +
  scale_y_continuous(n.breaks = 12, labels = scales::percent) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color= NA))

cont_dir 

##########################Plotando tudo em um só gráfico########################

grid.arrange(cont_adm,cont_eco,cont_eng,cont_dir, nrow = 2)

############################CR POR SEXO EM CADA CURSO########################### 

####################################ADM#########################################

cr_adm <- dados %>% 
  filter(Curso == "ADMINISTRAÇÃO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5)) +
  guides(alpha = "none") +
  labs(x = "",
       y = "Densidade",
       title = 
         "Histograma CR Administração") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))
cr_adm

#####################################ECO########################################

cr_eco <- dados %>% 
  filter(Curso == "CIÊNCIAS ECONÔMICAS") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5)) +
  guides(alpha = "none") +
  labs(x = "",
       y = "Densidade",
       title = 
         "Histograma CR Economia") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))
cr_eco

##########################Plotando tudo em um só gráfico########################

grid.arrange(cr_adm,cr_eco, nrow = 1)

####################################DIREITO#####################################

dados %>% 
  filter(Curso == "DIREITO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5)) +
  guides(alpha = "none") +
  labs(x = "",
       y = "",
       title = 
         "Histograma CR Direito por sexo") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

########################ENGENHARIA DA COMPUTAÇÃO################################

cr_comp <- dados %>% 
  filter(Curso == "ENGENHARIA DE COMPUTAÇÃO") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5)) +
  guides(alpha = "none") +
  scale_y_continuous(n.breaks = 12) +
  labs(x = "",
       y = "Densidade",
       title = 
         "Histograma CR Engenharia de Computação") +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

cr_comp

#############################ENGENHARIA MECATRÔNICA#############################

cr_mecat <- dados %>% 
  filter(Curso == "ENGENHARIA MECATRÔNICA") %>% 
  ggplot() +
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5)) +
  guides(alpha = "none") +
  labs(x = "",
       y = "Densidade",
       title = 
         "Histograma CR Engenharia Mecatrônica") +
  scale_y_continuous(n.breaks = 15) +
  theme_bw() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))
cr_mecat

#################################ENGENHARIA MECÂNICA############################

cr_mec <- dados %>% 
  filter(Curso == "ENGENHARIA MECÂNICA") %>% 
  ggplot()+
  geom_density(aes(as.numeric(CR), fill = Sexo, alpha = 0.5)) +
  guides(alpha = "none") +
  labs(x = "",
       y = "Densidade",
       title = 
         "Histograma CR Engenharia Mecânica") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent",
                                         color = NA))

cr_mec

grid.arrange(cr_comp, cr_mecat, cr_mec, nrow = 1)

################Distribuição dos 10 melhores CRs de Administração###############

freq_10_adm <- prop.table(table(dados$Sexo[dados$Curso=="ADMINISTRAÇÃO" & 
                                  dados$CR>= quantile(dados$CR, 
                                                      0.1)]))
freq_10_adm <- as.data.frame(freq_10_adm)

colnames(freq_10_adm) <- c("Sexo","Porcentagem")
freq_10_adm

################Distribuição dos 10 melhores CRs de Economia####################

freq_10_eco <- prop.table(table(dados$Sexo[dados$Curso=="CIÊNCIAS ECONÔMICAS" & 
                                             dados$CR>= quantile(dados$CR, 
                                                                 0.1)]))
freq_10_eco <- as.data.frame(freq_10_eco)

colnames(freq_10_eco) <- c("Sexo","Porcentagem")
freq_10_eco

################Distribuição dos 10 melhores CRs de Engenharia##################

freq_10_eng <- table(dados$Sexo[dados$CURSO =="ENGENHARIAS" & 
                                  dados$CR>= quantile(dados$CR, 0.1)])
prop.table(freq_10_eng)

###################Distribuição dos 10 melhores CRs de Engenharia###############

freq_10_dir <- table(dados$Sexo[dados$Curso=="DIREITO" & 
                                  dados$CR>= quantile(dados$CR, 0.1)])
prop.table(freq_10_dir)

#REGRESSÃO

modelo <- lm(log(CR)~d_gen+Periodo+factor(Curso),
             data = dados)

summary(modelo)

check_model(modelo)

