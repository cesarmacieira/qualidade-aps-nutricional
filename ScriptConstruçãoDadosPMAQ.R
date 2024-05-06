####==========================================================
#### Trabalho Catarina - Construção do banco de dados Vigitel
####==========================================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
#setwd("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina")#Diretório
setwd("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional")

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages("openxlsx"); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages("purrr"); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse)}#Manipulação de dados

####=========
#### Funções
####=========
DescritivaCat = function(x){
  tabela = cbind(table(x), prop.table(table(x)))
  colnames(tabela) = c("Freq. Absoluta (N)", "Freq. Relativa (%)")
  return(tabela)
}

DescritivaNum = function(x, more = F) {
  stats = list();
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$Mín. = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Máx. = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c("N","Média","Variância","D.P.","Mínimo","1ºQ","2ºQ","3ºQ","Máximo")
  t1
}

basic.stats = function(x, more = F) {
  stats = list()
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$E.P = round(sd(clean.x)/sqrt(length(clean.x)),3)
  stats$Min = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Max = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  t1
}

####=============================
#### Carregando o banco de dados 
####=============================
est_ciclo1 = read.xlsx("C:/Users/cesar_macieira/Downloads/PMAQ/UBS (Estrutura) Ciclo 1.xlsx", sheet = 1)
est_ciclo2 = read.xlsx("C:/Users/cesar_macieira/Downloads/PMAQ/UBS (Estrutura) Ciclo 2.xlsx", sheet = 1)
est_ciclo3 = read.xlsx("C:/Users/cesar_macieira/Downloads/PMAQ/UBS (Estrutura) Ciclo 3.xlsx", sheet = 1)

eq_ciclo1 = read.xlsx("C:/Users/cesar_macieira/Downloads/PMAQ/EquipeCiclo1.xlsx", sheet = 1)
eq_ciclo2 = read.xlsx("C:/Users/cesar_macieira/Downloads/PMAQ/EquipeCiclo2.xlsx", sheet = 1)
eq_ciclo3 = read.xlsx("C:/Users/cesar_macieira/Downloads/PMAQ/EquipeCiclo3.xlsx", sheet = 1)

####===========
#### Estrutura
####===========
DescritivaCat(est_ciclo1$I_3_6_4)
DescritivaCat(est_ciclo2$I_3_5_4)
DescritivaCat(est_ciclo3$I.3.2.4)

DescritivaCat(est_ciclo1$I_11_2)
DescritivaCat(est_ciclo2$I_12_2_1)
DescritivaCat(est_ciclo3$I.8.2)

DescritivaCat(est_ciclo1$I_11_3)
DescritivaCat(est_ciclo2$I_12_2_1_1)
DescritivaCat(est_ciclo3$I.8.4)

DescritivaCat(est_ciclo1$I_11_8)
DescritivaCat(est_ciclo2$I_12_5_1)
DescritivaCat(est_ciclo3$I.8.9)

DescritivaCat(est_ciclo1$I_11_9)
DescritivaCat(est_ciclo2$I_12_6_1)
DescritivaCat(est_ciclo3$I.8.10)

DescritivaCat(est_ciclo1$I_11_10)
DescritivaCat(est_ciclo2$I_12_7_1)
DescritivaCat(est_ciclo3$I.8.11)

DescritivaCat(est_ciclo1$I_11_11)
DescritivaCat(est_ciclo2$I_12_8_1)
DescritivaCat(est_ciclo3$I.8.12)
DescritivaCat(est_ciclo3$I.8.13)

DescritivaCat(est_ciclo1$I_11_17)
DescritivaCat(est_ciclo2$I_12_13_1)
DescritivaCat(est_ciclo3$I.8.26)

DescritivaCat(est_ciclo1$I_12_5)
DescritivaCat(est_ciclo2$I_16_7)
DescritivaCat(est_ciclo3$I.8.36)

DescritivaCat(est_ciclo1$I_11_12)
DescritivaCat(est_ciclo2$I_12_9_1)
DescritivaCat(est_ciclo3$I.8.5)

DescritivaCat(est_ciclo1$I_11_13)
DescritivaCat(est_ciclo2$I_12_9_1_1)
DescritivaCat(est_ciclo3$I.8.6)

DescritivaCat(est_ciclo1$I_11_24)
DescritivaCat(est_ciclo2$I_12_21_1)
DescritivaCat(est_ciclo3$I.8.31)

DescritivaCat(est_ciclo1$I_11_22)
DescritivaCat(est_ciclo2$I_12_16_1)
DescritivaCat(est_ciclo3$I.8.30)

DescritivaCat(est_ciclo1$I_12_17)
DescritivaCat(est_ciclo2$I_16_17)
DescritivaCat(est_ciclo3$I.12.6)

DescritivaCat(est_ciclo1$I_14_42_1)
DescritivaCat(est_ciclo2$I_19_22_1)
DescritivaCat(est_ciclo3$I.15.8.3)

DescritivaCat(est_ciclo1$I_14_41_1)
DescritivaCat(est_ciclo2$I_19_23_1)
DescritivaCat(est_ciclo3$I.15.8.4)

DescritivaCat(est_ciclo1$I_14_39_1)
DescritivaCat(est_ciclo2$I_19_20_1)
DescritivaCat(est_ciclo3$I.15.8.1)

DescritivaCat(est_ciclo1$I_14_40_1)
DescritivaCat(est_ciclo2$I_19_21_1)
DescritivaCat(est_ciclo3$I.15.8.2)

DescritivaCat(est_ciclo1$I_9_2)
DescritivaCat(est_ciclo2$I_10_1_14_2)
DescritivaCat(est_ciclo3$I.6.3.1)
DescritivaCat(est_ciclo3$I.6.3.2)
DescritivaCat(est_ciclo3$I.6.3.3)
DescritivaCat(est_ciclo3$I.6.3.4)

DescritivaCat(est_ciclo1$I_7_8_4)
DescritivaCat(est_ciclo2$I_8_6_6)
DescritivaCat(est_ciclo3$I.15.1)

####=========
#### Equipes
####=========
DescritivaCat(eq_ciclo1$II_24_6_2)
DescritivaCat(eq_ciclo2$II_19_4_2)
DescritivaCat(eq_ciclo3$II.16.6.2)

DescritivaCat(eq_ciclo1$II_24_6_3)
DescritivaCat(eq_ciclo2$II_19_4_3)
DescritivaCat(eq_ciclo3$II.16.6.3)

DescritivaCat(eq_ciclo1$II_24_3_2)
DescritivaCat(eq_ciclo2$II_14_7_7)
DescritivaCat(eq_ciclo3$II.16.8.2)

DescritivaCat(eq_ciclo1$II_25_5)
DescritivaCat(eq_ciclo2$II_14_5_2)
DescritivaCat(eq_ciclo3$II.17.7)

DescritivaCat(eq_ciclo1$II_26_5)
DescritivaCat(eq_ciclo2$II_14_5_3)
DescritivaCat(eq_ciclo3$II.18.8)

DescritivaCat(eq_ciclo1$II_31_1_6)
DescritivaCat(eq_ciclo2$II_26_2_6)
DescritivaCat(eq_ciclo3$II.26.3)

DescritivaCat(eq_ciclo1$II_33_3)
DescritivaCat(eq_ciclo2$II_27_2)
DescritivaCat(eq_ciclo3$II.30.2)

DescritivaCat(eq_ciclo1$II_15_10)
DescritivaCat(eq_ciclo2$II_12_17)
DescritivaCat(eq_ciclo3$II.10.5.3)

DescritivaCat(eq_ciclo1$II_12_2_1_1)#até
DescritivaCat(eq_ciclo1$II_12_2_1_21)###
DescritivaCat(eq_ciclo2$II_9_5_1)
DescritivaCat(eq_ciclo3$II.3.2.1)

DescritivaCat(eq_ciclo1$II_16_9)
DescritivaCat(eq_ciclo2$II_13_4)
DescritivaCat(eq_ciclo3$II.10.9.1)

####=====================
#### Seleção de capitais
####=====================
IBGE_capitais = c('280030','310620','150140','140010','530010','500270','510340','410690',
                  '420540','230440','520870','250750','160030','270430','130260','240810',
                  '172100','431490','110020','261160','120040','330455','292740','211130',
                  '355030','221100','320530')

cap_est_ciclo1 = est_ciclo1 %>% filter(IBGE %in% IBGE_capitais) %>%
  select(IBGE,I_3_6_4,I_11_2,I_11_3,I_11_8,I_11_9,I_11_10,I_11_11,I_11_17,I_12_5,I_11_12,I_11_13,
         I_11_24,I_11_22,I_12_17,I_14_42_1,I_14_41_1,I_14_39_1,I_14_40_1,I_9_2,I_7_8_4)

cap_est_ciclo2 = est_ciclo2 %>% filter(Ibge %in% IBGE_capitais) %>%
  select(Ibge,I_3_5_4,I_12_2_1,I_12_2_1_1,I_12_5_1,I_12_6_1,I_12_7_1,I_12_8_1,I_12_13_1,
         I_16_7,I_12_9_1,I_12_9_1_1,I_12_21_1,I_12_16_1,I_16_17,I_19_22_1,I_19_23_1,
         I_19_20_1,I_19_21_1,I_10_1_14_2,I_8_6_6)

cap_est_ciclo3 = est_ciclo3 %>% filter(IBGE %in% IBGE_capitais) %>%
  select(IBGE,I.3.2.4,I.8.2,I.8.4,I.8.9,I.8.10,I.8.11,I.8.12,I.8.13,I.8.26,I.8.36,I.8.5,
         I.8.6,I.8.31,I.8.30,I.12.6,I.15.8.3,I.15.8.4,I.15.8.1,I.15.8.2,I.6.3.1,I.6.3.2,
         I.6.3.3,I.6.3.4,I.15.1)

cap_eq_ciclo1 = eq_ciclo1 %>% filter(Ibge %in% IBGE_capitais) %>%
  select(Ibge,II_24_6_2,II_24_6_3,II_24_3_2,II_25_5,II_26_5,II_31_1_6,II_33_3,II_15_10,
         II_12_2_1_1,II_12_2_1_2,II_12_2_1_3,II_12_2_1_4,II_12_2_1_5,II_12_2_1_6,II_12_2_1_7,
         II_12_2_1_8,II_12_2_1_9,II_12_2_1_10,II_12_2_1_11,II_12_2_1_12,II_12_2_1_13,II_12_2_1_14,
         II_12_2_1_15,II_12_2_1_16,II_12_2_1_17,II_12_2_1_18,II_12_2_1_19,II_12_2_1_20,II_12_2_1_21,
         II_16_9)

cap_eq_ciclo2 = eq_ciclo2 %>% filter(Ibge %in% IBGE_capitais) %>%
  select(Ibge,II_19_4_2,II_19_4_3,II_14_7_7,II_14_5_2,II_14_5_3,II_26_2_6,II_27_2,II_12_17,II_9_5_1,
         II_13_4)

cap_eq_ciclo3 = eq_ciclo3 %>% filter(IBGE %in% IBGE_capitais) %>%
  select(IBGE,II.16.6.2,II.16.6.3,II.16.8.2,II.17.7,II.18.8,II.26.3,II.30.2,II.10.5.3,II.3.2.1,II.10.9.1)

####===========
#### Estrutura
####===========
cap_est_ciclo1[cap_est_ciclo1 == 999] = NA
cap_est_ciclo1[cap_est_ciclo1 == 998] = 0

cap_est_ciclo2[cap_est_ciclo2 == 998] = 0

cap_est_ciclo3[cap_est_ciclo3 == 998] = 0
cap_est_ciclo3[cap_est_ciclo3 == 9997] = NA

#Q9
cap_est_ciclo1$I_12_5 = case_when(cap_est_ciclo1$I_12_5 == 1 ~ 1,
                                  cap_est_ciclo1$I_12_5 == 2 | cap_est_ciclo1$I_12_5 == 3 ~ 0)
cap_est_ciclo2$I_16_7 = case_when(cap_est_ciclo2$I_16_7 == 1 ~ 1,
                                  cap_est_ciclo2$I_16_7 == 2 | cap_est_ciclo2$I_16_7 == 3 ~ 0)
cap_est_ciclo3$I.8.36 = case_when(cap_est_ciclo3$I.8.36 >= 1 ~ 1,
                                  cap_est_ciclo3$I.8.36 == 0 ~ 0)

#Q14
cap_est_ciclo1$I_12_17 = case_when(cap_est_ciclo1$I_12_17 == 1 ~ 1,
                                   cap_est_ciclo1$I_12_17 == 2 | cap_est_ciclo1$I_12_17 == 3 ~ 0)
cap_est_ciclo2$I_16_17 = case_when(cap_est_ciclo2$I_16_17 == 1 ~ 1,
                                   cap_est_ciclo2$I_16_17 == 2 | cap_est_ciclo2$I_16_17 == 3 ~ 0)
cap_est_ciclo3$I.12.6 = case_when(cap_est_ciclo3$I.12.6 == 1 ~ 1,
                                  cap_est_ciclo3$I.12.6 == 2 ~ 0)

#Q15
cap_est_ciclo1$I_14_42_1 = case_when(cap_est_ciclo1$I_14_42_1 == 1 ~ 1,
                                     cap_est_ciclo1$I_14_42_1 == 2 | cap_est_ciclo1$I_14_42_1 == 0 ~ 0)
cap_est_ciclo2$I_19_22_1 = case_when(cap_est_ciclo2$I_19_22_1 == 1 ~ 1,
                                     cap_est_ciclo2$I_19_22_1 == 2 | cap_est_ciclo2$I_19_22_1 == 0 ~ 0)
cap_est_ciclo3$I.15.8.3 = case_when(cap_est_ciclo3$I.15.8.3 == 1 ~ 1,
                                    cap_est_ciclo3$I.15.8.3 == 2 | cap_est_ciclo3$I.15.8.3 == 0 ~ 0)

#Q16
cap_est_ciclo1$I_14_41_1 = case_when(cap_est_ciclo1$I_14_41_1 == 1 ~ 1,
                                     cap_est_ciclo1$I_14_41_1 == 2 | cap_est_ciclo1$I_14_41_1 == 0 ~ 0)
cap_est_ciclo2$I_19_23_1 = case_when(cap_est_ciclo2$I_19_23_1 == 1 ~ 1,
                                     cap_est_ciclo2$I_19_23_1 == 2 | cap_est_ciclo2$I_19_23_1 == 0 ~ 0)
cap_est_ciclo3$I.15.8.4 = case_when(cap_est_ciclo3$I.15.8.4 == 1 ~ 1,
                                    cap_est_ciclo3$I.15.8.4 == 2 | cap_est_ciclo3$I.15.8.4 == 0 ~ 0)

#Q17
cap_est_ciclo1$I_14_39_1 = case_when(cap_est_ciclo1$I_14_39_1 == 1 ~ 1,
                                     cap_est_ciclo1$I_14_39_1 == 2 | cap_est_ciclo1$I_14_39_1 == 0 ~ 0)
cap_est_ciclo2$I_19_20_1 = case_when(cap_est_ciclo2$I_19_20_1 == 1 ~ 1,
                                     cap_est_ciclo2$I_19_20_1 == 2 | cap_est_ciclo2$I_19_20_1 == 0 ~ 0)
cap_est_ciclo3$I.15.8.1 = case_when(cap_est_ciclo3$I.15.8.1 == 1 ~ 1,
                                    cap_est_ciclo3$I.15.8.1 == 2 | cap_est_ciclo3$I.15.8.1 == 0 ~ 0)

#Q18
cap_est_ciclo1$I_14_40_1 = case_when(cap_est_ciclo1$I_14_40_1 == 1 ~ 1,
                                     cap_est_ciclo1$I_14_40_1 == 2 | cap_est_ciclo1$I_14_40_1 == 0 ~ 0)
cap_est_ciclo2$I_19_21_1 = case_when(cap_est_ciclo2$I_19_21_1 == 1 ~ 1,
                                     cap_est_ciclo2$I_19_21_1 == 2 | cap_est_ciclo2$I_19_21_1 == 0 ~ 0)
cap_est_ciclo3$I.15.8.2 = case_when(cap_est_ciclo3$I.15.8.2 == 1 ~ 1,
                                    cap_est_ciclo3$I.15.8.2 == 2 | cap_est_ciclo3$I.15.8.2 == 0 ~ 0)

#Q20
cap_est_ciclo1$I_7_8_4 = case_when(cap_est_ciclo1$I_7_8_4 == 1 ~ 1,
                                   cap_est_ciclo1$I_7_8_4 == 2 ~ 0)
cap_est_ciclo2$I_8_6_6 = case_when(cap_est_ciclo2$I_8_6_6 == 1 ~ 1,
                                   cap_est_ciclo2$I_8_6_6 == 2 ~ 0)
cap_est_ciclo3$I.15.1 = case_when(cap_est_ciclo3$I.15.1 == 1 ~ 1,
                                  cap_est_ciclo3$I.15.1 == 2 ~ 0)

dados_est_ciclo1 = cap_est_ciclo1 %>% 
  rename(Q1 = I_3_6_4, Q2 = I_11_2, Q3 = I_11_3, Q4 = I_11_8, Q5 = I_11_9,
         Q6 = I_11_10, Q7 = I_11_11, Q8 = I_11_17, Q9 = I_12_5, Q10 = I_11_12,
         Q11 = I_11_13, Q12 = I_11_24, Q13 = I_11_22, Q14 = I_12_17,
         Q15 = I_14_42_1, Q16 = I_14_41_1, Q17 = I_14_39_1, Q18 = I_14_40_1,
         Q19 = I_9_2, Q20 = I_7_8_4) %>% mutate(Ciclo = rep(1, dim(cap_est_ciclo1)[1])) %>% 
  select(IBGE,Ciclo,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20)

dados_est_ciclo2 = cap_est_ciclo2 %>% 
  rename(IBGE = Ibge, Q1 = I_3_5_4, Q2 = I_12_2_1, Q3 = I_12_2_1_1, Q4 = I_12_5_1, Q5 = I_12_6_1,
         Q6 = I_12_7_1, Q7 = I_12_8_1, Q8 = I_12_13_1, Q9 = I_16_7, Q10 = I_12_9_1,
         Q11 = I_12_9_1_1, Q12 = I_12_21_1, Q13 = I_12_16_1, Q14 = I_16_17,
         Q15 = I_19_22_1, Q16 = I_19_23_1, Q17 = I_19_20_1, Q18 = I_19_21_1,
         Q19 = I_10_1_14_2, Q20 = I_8_6_6) %>% mutate(Ciclo = rep(2, dim(cap_est_ciclo2)[1])) %>% 
  select(IBGE,Ciclo,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20)

cap_est_ciclo3$I.8.12e13 = cap_est_ciclo3$I.8.12 + cap_est_ciclo3$I.8.13
cap_est_ciclo3$I.6.3.1a4 = cap_est_ciclo3$I.6.3.1 + cap_est_ciclo3$I.6.3.2 + cap_est_ciclo3$I.6.3.3 + cap_est_ciclo3$I.6.3.4
dados_est_ciclo3 = cap_est_ciclo3 %>% 
  rename(Q1 = I.3.2.4, Q2 = I.8.2, Q3 = I.8.4, Q4 = I.8.9, Q5 = I.8.10,
         Q6 = I.8.11, Q7 = I.8.12e13, Q8 = I.8.26, Q9 = I.8.36, Q10 = I.8.5,
         Q11 = I.8.6, Q12 = I.8.31, Q13 = I.8.30, Q14 = I.12.6,
         Q15 = I.15.8.3, Q16 = I.15.8.4, Q17 = I.15.8.1, Q18 = I.15.8.2,
         Q19 = I.6.3.1a4, Q20 = I.15.1) %>% mutate(Ciclo = rep(3, dim(cap_est_ciclo3)[1])) %>% 
  select(IBGE,Ciclo,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20)

dados_est = rbind(dados_est_ciclo1,dados_est_ciclo2,dados_est_ciclo3)

#Q1
DescritivaCat(cap_est_ciclo1$I_3_6_4)
DescritivaCat(cap_est_ciclo2$I_3_5_4)
DescritivaCat(cap_est_ciclo3$I.3.2.4)

#Q2
DescritivaCat(cap_est_ciclo1$I_11_2)
DescritivaCat(cap_est_ciclo2$I_12_2_1)
DescritivaCat(cap_est_ciclo3$I.8.2)

#Q3
DescritivaCat(cap_est_ciclo1$I_11_3)
DescritivaCat(cap_est_ciclo2$I_12_2_1_1)
DescritivaCat(cap_est_ciclo3$I.8.4)

#Q4
DescritivaCat(cap_est_ciclo1$I_11_8)
DescritivaCat(cap_est_ciclo2$I_12_5_1)
DescritivaCat(cap_est_ciclo3$I.8.9)

#Q5
DescritivaCat(cap_est_ciclo1$I_11_9)
DescritivaCat(cap_est_ciclo2$I_12_6_1)
DescritivaCat(cap_est_ciclo3$I.8.10)

#Q6
DescritivaCat(cap_est_ciclo1$I_11_10)
DescritivaCat(cap_est_ciclo2$I_12_7_1)
DescritivaCat(cap_est_ciclo3$I.8.11)

#Q7
DescritivaCat(cap_est_ciclo1$I_11_11)
DescritivaCat(cap_est_ciclo2$I_12_8_1)
DescritivaCat(cap_est_ciclo3$I.8.12)
DescritivaCat(cap_est_ciclo3$I.8.13)

#Q8
DescritivaCat(cap_est_ciclo1$I_11_17)
DescritivaCat(cap_est_ciclo2$I_12_13_1)
DescritivaCat(cap_est_ciclo3$I.8.26)

#Q9
DescritivaCat(cap_est_ciclo1$I_12_5)
DescritivaCat(cap_est_ciclo2$I_16_7)
DescritivaCat(cap_est_ciclo3$I.8.36)

#Q10
DescritivaCat(cap_est_ciclo1$I_11_12)
DescritivaCat(cap_est_ciclo2$I_12_9_1)
DescritivaCat(cap_est_ciclo3$I.8.5)

#Q11
DescritivaCat(cap_est_ciclo1$I_11_13)
DescritivaCat(cap_est_ciclo2$I_12_9_1_1)
DescritivaCat(cap_est_ciclo3$I.8.6)

#Q12
DescritivaCat(cap_est_ciclo1$I_11_24)
DescritivaCat(cap_est_ciclo2$I_12_21_1)
DescritivaCat(cap_est_ciclo3$I.8.31)

#Q13
DescritivaCat(cap_est_ciclo1$I_11_22)
DescritivaCat(cap_est_ciclo2$I_12_16_1)
DescritivaCat(cap_est_ciclo3$I.8.30)

#Q14
DescritivaCat(cap_est_ciclo1$I_12_17)
DescritivaCat(cap_est_ciclo2$I_16_17)
DescritivaCat(cap_est_ciclo3$I.12.6)

#Q15
DescritivaCat(cap_est_ciclo1$I_14_42_1)
DescritivaCat(cap_est_ciclo2$I_19_22_1)
DescritivaCat(cap_est_ciclo3$I.15.8.3)

#Q16
DescritivaCat(cap_est_ciclo1$I_14_41_1)
DescritivaCat(cap_est_ciclo2$I_19_23_1)
DescritivaCat(cap_est_ciclo3$I.15.8.4)

#Q17
DescritivaCat(cap_est_ciclo1$I_14_39_1)
DescritivaCat(cap_est_ciclo2$I_19_20_1)
DescritivaCat(cap_est_ciclo3$I.15.8.1)

#Q18
DescritivaCat(cap_est_ciclo1$I_14_40_1)
DescritivaCat(cap_est_ciclo2$I_19_21_1)
DescritivaCat(cap_est_ciclo3$I.15.8.2)

#Q19
DescritivaCat(cap_est_ciclo1$I_9_2)
DescritivaCat(cap_est_ciclo2$I_10_1_14_2)
DescritivaCat(cap_est_ciclo3$I.6.3.1a4)

#Q20
DescritivaCat(cap_est_ciclo1$I_7_8_4)
DescritivaCat(cap_est_ciclo2$I_8_6_6)
DescritivaCat(cap_est_ciclo3$I.15.1)

####=========
#### Equipes
####=========
cap_eq_ciclo1[cap_eq_ciclo1 == 999] = NA
cap_eq_ciclo1[cap_eq_ciclo1 == 998] = 0

cap_eq_ciclo2[cap_eq_ciclo2 == 999] = NA
cap_eq_ciclo2[cap_eq_ciclo2 == 998] = 0

cap_eq_ciclo3[cap_eq_ciclo3 == 998] = 0
cap_eq_ciclo3[cap_eq_ciclo3 == 9997] = NA

#Q21
cap_eq_ciclo1$II_24_6_2 = case_when(cap_eq_ciclo1$II_24_6_2 == 1 ~ 1,
                                    cap_eq_ciclo1$II_24_6_2 == 2 ~ 0)
cap_eq_ciclo2$II_19_4_2 = case_when(cap_eq_ciclo2$II_19_4_2 == 1 ~ 1,
                                    cap_eq_ciclo2$II_19_4_2 == 2 ~ 0)
cap_eq_ciclo3$II.16.6.2 = case_when(cap_eq_ciclo3$II.16.6.2 == 1 ~ 1,
                                    cap_eq_ciclo3$II.16.6.2 == 0 | cap_eq_ciclo3$II.16.6.2 == 2 ~ 0)

#Q22
cap_eq_ciclo1$II_24_6_3 = case_when(cap_eq_ciclo1$II_24_6_3 == 1 ~ 1,
                                    cap_eq_ciclo1$II_24_6_3 == 2 ~ 0)
cap_eq_ciclo2$II_19_4_3 = case_when(cap_eq_ciclo2$II_19_4_3 == 1 ~ 1,
                                    cap_eq_ciclo2$II_19_4_3 == 2 ~ 0)
cap_eq_ciclo3$II.16.6.3 = case_when(cap_eq_ciclo3$II.16.6.3 == 1 ~ 1,
                                    cap_eq_ciclo3$II.16.6.3 == 0 | cap_eq_ciclo3$II.16.6.3 == 2 ~ 0)

#Q23
cap_eq_ciclo1$II_24_3_2 = case_when(cap_eq_ciclo1$II_24_3_2 == 1 ~ 1,
                                    cap_eq_ciclo1$II_24_3_2 == 2 ~ 0)
cap_eq_ciclo2$II_14_7_7 = case_when(cap_eq_ciclo2$II_14_7_7 == 1 ~ 1,
                                    cap_eq_ciclo2$II_14_7_7 == 2 ~ 0)
cap_eq_ciclo3$II.16.8.2 = case_when(cap_eq_ciclo3$II.16.8.2 == 1 ~ 1,
                                    cap_eq_ciclo3$II.16.8.2 == 0 | cap_eq_ciclo3$II.16.8.2 == 2 ~ 0)

#Q24
cap_eq_ciclo1$II_25_5 = case_when(cap_eq_ciclo1$II_25_5 == 1 ~ 1,
                                  cap_eq_ciclo1$II_25_5 == 2 ~ 0)
cap_eq_ciclo2$II_14_5_2 = case_when(cap_eq_ciclo2$II_14_5_2 == 1 ~ 1,
                                    cap_eq_ciclo2$II_14_5_2 == 2 ~ 0)
cap_eq_ciclo3$II.17.7 = case_when(cap_eq_ciclo3$II.17.7 == 1 ~ 1,
                                  cap_eq_ciclo3$II.17.7 == 0 | cap_eq_ciclo3$II.17.7 == 2 ~ 0)

#Q25
cap_eq_ciclo1$II_26_5 = case_when(cap_eq_ciclo1$II_26_5 == 1 ~ 1,
                                  cap_eq_ciclo1$II_26_5 == 2 ~ 0)
cap_eq_ciclo2$II_14_5_3 = case_when(cap_eq_ciclo2$II_14_5_3 == 1 ~ 1,
                                    cap_eq_ciclo2$II_14_5_3 == 2 ~ 0)
cap_eq_ciclo3$II.18.8 = case_when(cap_eq_ciclo3$II.18.8 == 1 ~ 1,
                                  cap_eq_ciclo3$II.18.8 == 0 | cap_eq_ciclo3$II.18.8 == 2 ~ 0)

#Q26
cap_eq_ciclo1$II_31_1_6 = case_when(cap_eq_ciclo1$II_31_1_6 == 1 ~ 1,
                                    cap_eq_ciclo1$II_31_1_6 == 2 ~ 0)
cap_eq_ciclo2$II_26_2_6 = case_when(cap_eq_ciclo2$II_26_2_6 == 1 ~ 1,
                                    cap_eq_ciclo2$II_26_2_6 == 0 | cap_eq_ciclo2$II_26_2_6 == 2 ~ 0)
cap_eq_ciclo3$II.26.3 = case_when(cap_eq_ciclo3$II.26.3 == 1 ~ 1,
                                  cap_eq_ciclo3$II.26.3 == 0 | cap_eq_ciclo3$II.26.3 == 2 ~ 0)

#Q27
cap_eq_ciclo1$II_33_3 = case_when(cap_eq_ciclo1$II_33_3 == 1 ~ 1,
                                  cap_eq_ciclo1$II_33_3 == 2 ~ 0)
cap_eq_ciclo2$II_27_2 = case_when(cap_eq_ciclo2$II_27_2 == 1 ~ 1,
                                  cap_eq_ciclo2$II_27_2 == 0 | cap_eq_ciclo2$II_27_2 == 2 ~ 0)
cap_eq_ciclo3$II.30.2 = case_when(cap_eq_ciclo3$II.30.2 == 1 ~ 1,
                                  cap_eq_ciclo3$II.30.2 == 0 | cap_eq_ciclo3$II.30.2 == 2 ~ 0)

#Q28
cap_eq_ciclo1$II_15_10 = case_when(cap_eq_ciclo1$II_15_10 == 1 ~ 1,
                                   cap_eq_ciclo1$II_15_10 == 2 ~ 0)
cap_eq_ciclo2$II_12_17 = case_when(cap_eq_ciclo2$II_12_17 == 1 ~ 1,
                                   cap_eq_ciclo2$II_12_17 == 2 ~ 0)
cap_eq_ciclo3$II.10.5.3 = case_when(cap_eq_ciclo3$II.10.5.3 == 1 ~ 1,
                                    cap_eq_ciclo3$II.10.5.3 == 0 | cap_eq_ciclo3$II.10.5.3 == 2 ~ 0)

#Q29
cap_eq_ciclo1$II_12_2_1_1 = ifelse(cap_eq_ciclo1$II_12_2_1_1 == 2, 0, cap_eq_ciclo1$II_12_2_1_1)
cap_eq_ciclo1$II_12_2_1_2 = ifelse(cap_eq_ciclo1$II_12_2_1_2 == 2, 0, cap_eq_ciclo1$II_12_2_1_2)
cap_eq_ciclo1$II_12_2_1_3 = ifelse(cap_eq_ciclo1$II_12_2_1_3 == 2, 0, cap_eq_ciclo1$II_12_2_1_3)
cap_eq_ciclo1$II_12_2_1_4 = ifelse(cap_eq_ciclo1$II_12_2_1_4 == 2, 0, cap_eq_ciclo1$II_12_2_1_4)
cap_eq_ciclo1$II_12_2_1_5 = ifelse(cap_eq_ciclo1$II_12_2_1_5 == 2, 0, cap_eq_ciclo1$II_12_2_1_5)
cap_eq_ciclo1$II_12_2_1_6 = ifelse(cap_eq_ciclo1$II_12_2_1_6 == 2, 0, cap_eq_ciclo1$II_12_2_1_6)
cap_eq_ciclo1$II_12_2_1_7 = ifelse(cap_eq_ciclo1$II_12_2_1_7 == 2, 0, cap_eq_ciclo1$II_12_2_1_7)
cap_eq_ciclo1$II_12_2_1_8 = ifelse(cap_eq_ciclo1$II_12_2_1_8 == 2, 0, cap_eq_ciclo1$II_12_2_1_8)
cap_eq_ciclo1$II_12_2_1_9 = ifelse(cap_eq_ciclo1$II_12_2_1_9 == 2, 0, cap_eq_ciclo1$II_12_2_1_9)
cap_eq_ciclo1$II_12_2_1_10 = ifelse(cap_eq_ciclo1$II_12_2_1_10 == 2, 0, cap_eq_ciclo1$II_12_2_1_10)
cap_eq_ciclo1$II_12_2_1_11 = ifelse(cap_eq_ciclo1$II_12_2_1_11 == 2, 0, cap_eq_ciclo1$II_12_2_1_11)
cap_eq_ciclo1$II_12_2_1_12 = ifelse(cap_eq_ciclo1$II_12_2_1_12 == 2, 0, cap_eq_ciclo1$II_12_2_1_12)
cap_eq_ciclo1$II_12_2_1_13 = ifelse(cap_eq_ciclo1$II_12_2_1_13 == 2, 0, cap_eq_ciclo1$II_12_2_1_13)
cap_eq_ciclo1$II_12_2_1_14 = ifelse(cap_eq_ciclo1$II_12_2_1_14 == 2, 0, cap_eq_ciclo1$II_12_2_1_14)
cap_eq_ciclo1$II_12_2_1_15 = ifelse(cap_eq_ciclo1$II_12_2_1_15 == 2, 0, cap_eq_ciclo1$II_12_2_1_15)
cap_eq_ciclo1$II_12_2_1_16 = ifelse(cap_eq_ciclo1$II_12_2_1_16 == 2, 0, cap_eq_ciclo1$II_12_2_1_16)
cap_eq_ciclo1$II_12_2_1_17 = ifelse(cap_eq_ciclo1$II_12_2_1_17 == 2, 0, cap_eq_ciclo1$II_12_2_1_17)
cap_eq_ciclo1$II_12_2_1_18 = ifelse(cap_eq_ciclo1$II_12_2_1_18 == 2, 0, cap_eq_ciclo1$II_12_2_1_18)
cap_eq_ciclo1$II_12_2_1_19 = ifelse(cap_eq_ciclo1$II_12_2_1_19 == 2, 0, cap_eq_ciclo1$II_12_2_1_19)
cap_eq_ciclo1$II_12_2_1_20 = ifelse(cap_eq_ciclo1$II_12_2_1_20 == 2, 0, cap_eq_ciclo1$II_12_2_1_20)
cap_eq_ciclo1$II_12_2_1_21 = ifelse(cap_eq_ciclo1$II_12_2_1_21 == 2, 0, cap_eq_ciclo1$II_12_2_1_21)

cap_eq_ciclo1$II_12_2_1_1a21soma = cap_eq_ciclo1$II_12_2_1_1 + cap_eq_ciclo1$II_12_2_1_2 + cap_eq_ciclo1$II_12_2_1_3 +
  cap_eq_ciclo1$II_12_2_1_4 + cap_eq_ciclo1$II_12_2_1_5 + cap_eq_ciclo1$II_12_2_1_6 + cap_eq_ciclo1$II_12_2_1_7 +
  cap_eq_ciclo1$II_12_2_1_8 + cap_eq_ciclo1$II_12_2_1_9 + cap_eq_ciclo1$II_12_2_1_10 + cap_eq_ciclo1$II_12_2_1_11 +
  cap_eq_ciclo1$II_12_2_1_12 + cap_eq_ciclo1$II_12_2_1_13 + cap_eq_ciclo1$II_12_2_1_14 + cap_eq_ciclo1$II_12_2_1_15 +
  cap_eq_ciclo1$II_12_2_1_16 + cap_eq_ciclo1$II_12_2_1_17 + cap_eq_ciclo1$II_12_2_1_18 + cap_eq_ciclo1$II_12_2_1_19 +
  cap_eq_ciclo1$II_12_2_1_20 + cap_eq_ciclo1$II_12_2_1_21

cap_eq_ciclo1$II_12_2_1_1a21 = case_when(cap_eq_ciclo1$II_12_2_1_1a21soma >= 1 ~ 1,
                                         cap_eq_ciclo1$II_12_2_1_1a21soma == 0 ~ 0)
cap_eq_ciclo2$II_9_5_1 = case_when(cap_eq_ciclo2$II_9_5_1 == 1 ~ 1,
                                   cap_eq_ciclo2$II_9_5_1 == 0 | cap_eq_ciclo2$II_9_5_1 == 2 ~ 0)
cap_eq_ciclo3$II.3.2.1 = case_when(cap_eq_ciclo3$II.3.2.1 == 1 ~ 1,
                                   cap_eq_ciclo3$II.3.2.1 == 0 | cap_eq_ciclo3$II.3.2.1 == 2 ~ 0)

#Q30
cap_eq_ciclo1$II_16_9 = case_when(cap_eq_ciclo1$II_16_9 == 1 ~ 1,
                                  cap_eq_ciclo1$II_16_9 == 2 ~ 0)
cap_eq_ciclo2$II_13_4 = case_when(cap_eq_ciclo2$II_13_4 == 1 ~ 1,
                                  cap_eq_ciclo2$II_13_4 == 2 ~ 0)
cap_eq_ciclo3$II.10.9.1 = case_when(cap_eq_ciclo3$II.10.9.1 == 1 ~ 1,
                                    cap_eq_ciclo3$II.10.9.1 == 0 | cap_eq_ciclo3$II.10.9.1 == 2 ~ 0)

dados_eq_ciclo1 = cap_eq_ciclo1 %>% 
  rename(IBGE = Ibge, Q21 = II_24_6_2, Q22 = II_24_6_3, Q23 = II_24_3_2, Q24 = II_25_5, Q25 = II_26_5,
         Q26 = II_31_1_6, Q27 = II_33_3, Q28 = II_15_10, Q29 = II_12_2_1_1a21, Q30 = II_16_9) %>% 
  mutate(Ciclo = rep(1, dim(cap_eq_ciclo1)[1])) %>% 
  select(IBGE,Ciclo,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30)

dados_eq_ciclo2 = cap_eq_ciclo2 %>% 
  rename(IBGE = Ibge, Q21 = II_19_4_2, Q22 = II_19_4_3, Q23 = II_14_7_7, Q24 = II_14_5_2, Q25 = II_14_5_3,
         Q26 = II_26_2_6, Q27 = II_27_2, Q28 = II_12_17, Q29 = II_9_5_1, Q30 = II_13_4) %>% 
  mutate(Ciclo = rep(2, dim(cap_eq_ciclo2)[1])) %>% 
  select(IBGE,Ciclo,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30)

dados_eq_ciclo3 = cap_eq_ciclo3 %>% 
  rename(Q21 = II.16.6.2, Q22 = II.16.6.3, Q23 = II.16.8.2, Q24 = II.17.7, Q25 = II.18.8,
         Q26 = II.26.3, Q27 = II.30.2, Q28 = II.10.5.3, Q29 = II.3.2.1, Q30 = II.10.9.1) %>% 
  mutate(Ciclo = rep(3, dim(cap_eq_ciclo3)[1])) %>% 
  select(IBGE,Ciclo,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30)

dados_eq = rbind(dados_eq_ciclo1,dados_eq_ciclo2,dados_eq_ciclo3)

#Q21
DescritivaCat(cap_eq_ciclo1$II_24_6_2)
DescritivaCat(cap_eq_ciclo2$II_19_4_2)
DescritivaCat(cap_eq_ciclo3$II.16.6.2)

#Q22
DescritivaCat(cap_eq_ciclo1$II_24_6_3)
DescritivaCat(cap_eq_ciclo2$II_19_4_3)
DescritivaCat(cap_eq_ciclo3$II.16.6.3)

#Q23
DescritivaCat(cap_eq_ciclo1$II_24_3_2)
DescritivaCat(cap_eq_ciclo2$II_14_7_7)
DescritivaCat(cap_eq_ciclo3$II.16.8.2)

#Q24
DescritivaCat(cap_eq_ciclo1$II_25_5)
DescritivaCat(cap_eq_ciclo2$II_14_5_2)
DescritivaCat(cap_eq_ciclo3$II.17.7)

#Q25
DescritivaCat(cap_eq_ciclo1$II_26_5)
DescritivaCat(cap_eq_ciclo2$II_14_5_3)
DescritivaCat(cap_eq_ciclo3$II.18.8)

#Q26
DescritivaCat(cap_eq_ciclo1$II_31_1_6)
DescritivaCat(cap_eq_ciclo2$II_26_2_6)
DescritivaCat(cap_eq_ciclo3$II.26.3)

#Q27
DescritivaCat(cap_eq_ciclo1$II_33_3)
DescritivaCat(cap_eq_ciclo2$II_27_2)
DescritivaCat(cap_eq_ciclo3$II.30.2)

#Q28
DescritivaCat(cap_eq_ciclo1$II_15_10)
DescritivaCat(cap_eq_ciclo2$II_12_17)
DescritivaCat(cap_eq_ciclo3$II.10.5.3)

#Q29
DescritivaCat(cap_eq_ciclo1$II_12_2_1_1)#até
DescritivaCat(cap_eq_ciclo1$II_12_2_1_21)###
DescritivaCat(cap_eq_ciclo2$II_9_5_1)
DescritivaCat(cap_eq_ciclo3$II.3.2.1)

#Q30
DescritivaCat(cap_eq_ciclo1$II_16_9)
DescritivaCat(cap_eq_ciclo2$II_13_4)
DescritivaCat(cap_eq_ciclo3$II.10.9.1)

####=================================
#### Junção dados estrutura e equipe
####=================================
dados_est_agg = dados_est %>% group_by(IBGE,Ciclo) %>%
  summarize(Q1_media = mean(Q1, na.rm = TRUE), Q2_media = mean(Q2, na.rm = TRUE), Q3_media = mean(Q3, na.rm = TRUE),
            Q4_media = mean(Q4, na.rm = TRUE), Q5_media = mean(Q5, na.rm = TRUE), Q6_media = mean(Q6, na.rm = TRUE),
            Q7_media = mean(Q7, na.rm = TRUE), Q8_media = mean(Q8, na.rm = TRUE),
            Q9_prop = sum(Q9 == 1, na.rm = TRUE) / sum(!is.na(Q9)),
            Q10_media = mean(Q10, na.rm = TRUE), Q11_media = mean(Q11, na.rm = TRUE),
            Q12_media = mean(Q12, na.rm = TRUE), Q13_media = mean(Q13, na.rm = TRUE),
            Q14_prop = sum(Q14 == 1, na.rm = TRUE) / sum(!is.na(Q14)),
            Q15_prop = sum(Q15 == 1, na.rm = TRUE) / sum(!is.na(Q15)),
            Q16_prop = sum(Q16 == 1, na.rm = TRUE) / sum(!is.na(Q16)),
            Q17_prop = sum(Q17 == 1, na.rm = TRUE) / sum(!is.na(Q17)),
            Q18_prop = sum(Q18 == 1, na.rm = TRUE) / sum(!is.na(Q18)),
            Q19_media = mean(Q19, na.rm = TRUE),
            Q20_prop = sum(Q20 == 1, na.rm = TRUE) / sum(!is.na(Q20)))

dados_eq_agg = dados_eq %>% group_by(IBGE,Ciclo) %>%
  summarize(Q21_prop = sum(Q21 == 1, na.rm = TRUE) / sum(!is.na(Q21)),
            Q22_prop = sum(Q22 == 1, na.rm = TRUE) / sum(!is.na(Q22)),
            Q23_prop = sum(Q23 == 1, na.rm = TRUE) / sum(!is.na(Q23)),
            Q24_prop = sum(Q24 == 1, na.rm = TRUE) / sum(!is.na(Q24)),
            Q25_prop = sum(Q25 == 1, na.rm = TRUE) / sum(!is.na(Q25)),
            Q26_prop = sum(Q26 == 1, na.rm = TRUE) / sum(!is.na(Q26)),
            Q27_prop = sum(Q27 == 1, na.rm = TRUE) / sum(!is.na(Q27)),
            Q28_prop = sum(Q28 == 1, na.rm = TRUE) / sum(!is.na(Q28)),
            Q29_prop = sum(Q29 == 1, na.rm = TRUE) / sum(!is.na(Q29)),
            Q30_prop = sum(Q30 == 1, na.rm = TRUE) / sum(!is.na(Q30)))

dados_est_eq = left_join(dados_est_agg, dados_eq_agg, by = c('IBGE'='IBGE','Ciclo'='Ciclo'))
write.xlsx(dados_est_eq %>% as.data.frame(), 'Dados estrutura e equipes PMAQ capitais.xlsx', rowNames = F)

