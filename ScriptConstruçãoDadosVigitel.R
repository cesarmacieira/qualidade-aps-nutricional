####==========================================================
#### Trabalho Catarina - Construção do banco de dados Vigitel
####==========================================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
setwd("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina")#Diretório

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages("openxlsx"); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages("purrr"); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse)}#Manipulação de dados
#if(!require(stringr)){ install.packages("stringr"); require(stringr)}#Strings
if(!require(ggplot2)){ install.packages("ggplot2"); require(ggplot2)}
if(!require(haven)){ install.packages("haven"); require(haven)}

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

QuiQuadrado_Fisher = function(x, y, type.sum, teste){
  t0 = table(x, y)
  if(type.sum==2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0("X", 1:dim(t0)[2])
  colnames(t1) = paste0("X", 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c("N", "%"), dim(t3)[2]/2))
  if(teste=="chisq") {
    Valor_p = chisq.test(t0)$p.value
  }
  if(teste=="fisher") {
    Valor_p = fisher.test(t0)$p.value
  } 
  if(teste=="chisq.simulate"){
    Valor_p = chisq.test(t0, simulate.p.value=TRUE, B=10000)$p.value
  }
  
  t4 = cbind(t3, Valor_p)
  return(t4)
}

KruskalTeste = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  p_valor = rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(z))
  
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = posthoc.kruskal.nemenyi.test(y ~ factor(z), dist="Chisq")$p.value
  CM = kwAllPairsNemenyiTest(y ~ factor(z), dist="Chisquare")$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

MannWhitney = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

WilcoxonDependente = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

TesteTpareado = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  return(tab)
}

TesteT = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = F)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

TesteDeNormalidade = function(x){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  AndersonDarling = round(ad.test(x)$p.value,3)
  KolmogorovSmirnov = round(ks.test(x, "pnorm", mean(x, na.rm = T), sd(x, na.rm = T))$p.value,3)
  Lilliefors = round(lillie.test(x)$p.value,3)
  CramerVonMises = round(cvm.test(x)$p.value,3)
  if(length(x) > 5000){
    ShapiroWilk = "N > 5000"
    ShapiroFrancia = "N > 5000"
  }else{
    ShapiroWilk = shapiro.test(x)$p.value
    ShapiroFrancia = sf.test(x)$p.value   
  }
  tabela = cbind(AndersonDarling,KolmogorovSmirnov,Lilliefors,CramerVonMises,
                 ShapiroWilk,ShapiroFrancia)
  colnames(tabela) = c('Anderson-Darling','Kolmogorov-Smirnov','Lilliefors','Cramer Von Mises','Shapiro-Wilk','Shapiro Francia')
  #row.names(tabela) = x
  return(tabela)
}

####=============================
#### Carregando o banco de dados 
####=============================
# vigitel_originais = haven::read_dta("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/vigitel2006_2022_indicadores_2023.04.24 RClaro.dta")
#  
# vigitel = vigitel_originais %>% filter(ano >= 2010 & ano <= 2019) %>% 
#    select(ordem,pesorake,ano,cidade,regiao,q6,q7,civil,q8a,q8b,q8_anos,q88,
#           q9,q9_i,q11,q11_i,q16,q25,q27,q18,q20,q26,q28,q29,q15,
#           r301_a,r301_b,r301_c,r301_d,r301_e,r301_g,r301_l,
#           r302_a,r302_b,r302_c,r302_d,r302_e,r302_f,r302_g,r302_h,r302_i,r302_j,r302_k,r302_l,r302_m,
#           r144a,r144b,q75,q76)
# write.xlsx(vigitel %>% as.data.frame(), 'Dados Catarina Vigitel 05-03-2024.xlsx', rowNames=F)

vigitel = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/Dados Catarina Vigitel 05-03-2024.xlsx", sheet = 1)

####=====================
#### Tratamento de dados
####=====================
vigitel$IMC = vigitel$q9/((vigitel$q11/100)*(vigitel$q11/100))
vigitel$IMC = ifelse(vigitel$q11 >= 700 | vigitel$q9 >= 700, NA, vigitel$IMC)

vigitel$IMC_i = vigitel$q9_i/((vigitel$q11_i/100)*(vigitel$q11_i/100))
vigitel$IMC_i = ifelse(vigitel$q11_i >= 700 | vigitel$q9_i >= 700, NA, vigitel$IMC_i)

vigitel$hortareg = ifelse(vigitel$q16 == 3 | vigitel$q16 == 4, 1, 0)

vigitel$frutareg = ifelse(vigitel$q25 == 3 | vigitel$q25 == 4 | vigitel$q27 == 3 | vigitel$q27 == 4, 1, 0)

vigitel$flvreg = ifelse(vigitel$frutareg == 1 & vigitel$hortareg == 1, 1, 0)

vigitel$cruadia = case_when(vigitel$q18 == 1 | vigitel$q18 == 2 ~ 1,
                            vigitel$q18 == 3 ~ 2,
                            TRUE  ~ 0)

vigitel$cozidadia = case_when(vigitel$q20 == 1 | vigitel$q20 == 2 ~ 1,
                              vigitel$q20 == 3 ~ 2,
                              TRUE  ~ 0)

vigitel$hortadia = vigitel$cruadia + vigitel$cozidadia

vigitel$sucodia = ifelse(vigitel$q26 >= 1 & vigitel$q26 <= 3, 1, 0)

vigitel$sofrutadia = case_when(vigitel$q28 == 1 ~ 1,
                               vigitel$q28 == 2 ~ 2,
                               vigitel$q28 == 3 ~ 3,
                               TRUE  ~ 0)

vigitel$frutadia = vigitel$sofrutadia + vigitel$sucodia

vigitel$flvdia = vigitel$hortadia + vigitel$frutadia

vigitel$flvreco = ifelse((vigitel$flvdia >= 5 & vigitel$flvdia <= 8) & vigitel$flvreg == 1, 1, 0)

vigitel$refritl5 = ifelse(vigitel$q29 == 3 | vigitel$q29 == 4, 1, 0)

vigitel$feijao5 = ifelse(vigitel$q15 == 3 | vigitel$q15 == 4, 1, 0)

vigitel$hart = ifelse(vigitel$q75 == 1, 1, 0)

vigitel$diab = ifelse(vigitel$q76 == 1, 1, 0)

# vigitel[c('r301_a','r301_b','r301_c','r301_d','r301_e','r301_g','r301_l','r302_a','r302_b','r302_c','r302_d','r302_e','r302_f','r302_g','r302_h','r302_i','r302_j','r302_k','r302_l','r302_m')] = 
#   lapply(vigitel[c('r301_a','r301_b','r301_c','r301_d','r301_e','r301_g','r301_l','r302_a','r302_b','r302_c','r302_d','r302_e','r302_f','r302_g','r302_h','r302_i','r302_j','r302_k','r302_l','r302_m')], as.numeric)
# vigitel$score_sf = rowSums(select(vigitel,r301_a,r301_b,r301_c,r301_d,r301_e,r301_g,r301_l), na.rm = TRUE)
# vigitel$score_sf_2cat = case_when(vigitel$score_sf_2cat < 5 ~ 0,
#                                   vigitel$score_sf_2cat >= 5 ~ 1)
# vigitel$score_upp = rowSums(select(vigitel,r302_a,r302_b,r302_c,r302_d,r302_e,r302_f,r302_g,r302_h,r302_i,r302_j,
#                                    r302_k,r302_l,r302_m), na.rm = TRUE)
# vigitel$score_upp_2cat = case_when(vigitel$score_upp < 5 ~ 0,
#                                    vigitel$score_upp >= 5 ~ 1)
# vigitel$r144a = as.numeric(vigitel$r144a)
# vigitel$cont_lanche_7 = case_when(vigitel$r144a == 1 ~ 1.5,
#                                   vigitel$r144a == 2 ~ 3.5,
#                                   vigitel$r144a == 3 ~ 5.5,
#                                   vigitel$r144a == 4 ~ 7.0,
#                                   TRUE  ~ vigitel$r144a)
# vigitel$lanche_7 = case_when(vigitel$r144b == 1 ~ vigitel$cont_lanche_7 + 1.5,
#                              vigitel$r144b == 2 ~ vigitel$cont_lanche_7 + 3.5,
#                              vigitel$r144b == 3 ~ vigitel$cont_lanche_7 + 5.5,
#                              vigitel$r144b == 4 ~ vigitel$cont_lanche_7 + 7.0,
#                              TRUE  ~ vigitel$cont_lanche_7)
# vigitel$lanche_7_2cat = case_when(vigitel$lanche_7 < 7 ~ 0,
#                                   vigitel$lanche_7 >= 7 ~ 1,
#                                   TRUE  ~ vigitel$lanche_7)

vigitel$cidade_2 = case_when(vigitel$cidade == '1'	~ 'Aracaju',
                             vigitel$cidade == '2'	~ 'Belém',
                             vigitel$cidade == '3'	~ 'Belo Horizonte',
                             vigitel$cidade == '4'	~ 'Boa vista',
                             vigitel$cidade == '5'	~ 'Campo Grande',
                             vigitel$cidade == '6'	~ 'Cuiabá',
                             vigitel$cidade == '7'	~ 'Curitiba',
                             vigitel$cidade == '8'	~ 'Florianópolis',
                             vigitel$cidade == '9'	~ 'Fortaleza',
                             vigitel$cidade == '10' ~ 'Goiânia',
                             vigitel$cidade == '11' ~ 'João Pessoa',
                             vigitel$cidade == '12' ~ 'Macapá',
                             vigitel$cidade == '13' ~ 'Maceió',
                             vigitel$cidade == '14' ~ 'Manaus',
                             vigitel$cidade == '15' ~ 'Natal',
                             vigitel$cidade == '16' ~ 'Palmas',
                             vigitel$cidade == '17' ~ 'Porto Alegre',
                             vigitel$cidade == '18' ~ 'Porto Velho',
                             vigitel$cidade == '19' ~ 'Recife',
                             vigitel$cidade == '20' ~ 'Rio Branco',
                             vigitel$cidade == '21' ~ 'Rio de Janeiro',
                             vigitel$cidade == '22' ~ 'Salvador',
                             vigitel$cidade == '23' ~ 'São Luís',
                             vigitel$cidade == '24' ~ 'São Paulo',
                             vigitel$cidade == '25' ~ 'Teresina',
                             vigitel$cidade == '26' ~ 'Vitória',
                             vigitel$cidade == '27' ~ 'Distrito Federal')

vigitel$q7_2 = case_when(vigitel$q7 == '1' ~ 'Masculino',
                         vigitel$q7 == '2' ~ 'Feminino')

vigitel$q8a_2 = case_when(vigitel$q8a == '1' ~ 'Curso primário',
                          vigitel$q8a == '2' ~ 'Admissão',
                          vigitel$q8a == '3' ~ 'Curso ginasial ou ginásio',
                          vigitel$q8a == '4' ~ '1º grau ou fundamental ou supletivo de 1º grau',
                          vigitel$q8a == '5' ~ '2º grau ou colégio ou técnico ou normal ou científico científico ou ensino médio ou supletivo de 2º grau',
                          vigitel$q8a == '6' ~ '3º grau ou curso superior',
                          vigitel$q8a == '7' ~ 'Pós-graduação (especialização, mestrado, doutorado)',
                          vigitel$q8a == '8' ~ 'Nunca estudou',
                          vigitel$q8a == '777' ~ 'Não sabe',
                          vigitel$q8a == '888' ~ 'Não quis responder')

dados = vigitel %>% select(ordem,pesorake,ano,cidade,cidade_2,regiao,q6,q7,q7_2,civil,q8a,q8a_2,q8b,q8_anos,q88,q9,q11,IMC,q9_i,q11_i,
                           IMC_i,q16,hortareg,q25,q27,frutareg,flvreg,q18,cruadia,q20,cozidadia,hortadia,q26,sucodia,
                           q28,sofrutadia,frutadia,flvdia,flvreco,q29,refritl5,q15,feijao5,q75,hart,q76,diab)




write.xlsx(dados %>% as.data.frame(), 'Dados Catarina 05-03-2024.xlsx', rowNames = F)
