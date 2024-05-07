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
#vigitel = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/Dados Catarina Vigitel para análises 21-03-2024.xlsx", sheet = 1)
vigitel = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel para análises 09-04-2024.xlsx", sheet = 1)
notas_pmaq = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Notas PMAQ 3 ciclos 2010 a 2019.xlsx", sheet = 1)
icsap = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/DadosICSAP_Capitais.xlsx",
                  sheet=1)
Pop_Leitos_Planos_ESF = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/População Leitos Planos Privados Cob ESF 2010-2019.xlsx",
                                  sheet=1)
Gini_IVS_IDHM = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Gini, IVS e IDHM 2010.xlsx",
                          sheet=1)
pmaq = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados estrutura e equipes PMAQ capitais.xlsx", sheet = 1)

####==================
#### Junção dos dados
####==================
icsap_agg = icsap %>%
  group_by(MUNIC_RES, Nome.do.Município, Nome.da.UF, Região, ANO_CMPT, SEXO, FX_ETARIA) %>%
  summarise(across(where(is.numeric) & !matches("TaxaICSAP"), sum),
            TaxaICSAP = mean(as.numeric(TaxaICSAP))) %>%
  as.data.frame()
icsap_agg$SEXO = case_when(icsap_agg$SEXO == 'F' ~ 'Feminino',
                           icsap_agg$SEXO == 'M' ~ 'Masculino')


vigitel_icsap = full_join(vigitel, icsap_agg, by = c('cidade'='Nome.do.Município','sexo'='SEXO',
                                                     'ano'='ANO_CMPT','idade_cat'='FX_ETARIA'))


vigitel_icsap = vigitel_icsap %>% group_by(cidade) %>% fill(MUNIC_RES, Nome.da.UF, Região) %>% as.data.frame()

vigitel_icsap_notas_pmaq = left_join(vigitel_icsap, notas_pmaq, by = c('MUNIC_RES'='IBGE','ano'='Ano'))

vigitel_icsap_notas_pmaq$Região = ifelse(vigitel_icsap_notas_pmaq$Região == 'Centro Oeste', 'Centro-Oeste', vigitel_icsap_notas_pmaq$Região)

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF = 
  left_join(vigitel_icsap_notas_pmaq, Pop_Leitos_Planos_ESF %>% mutate(IBGE = as.numeric(IBGE)) %>% rename(Cod_UF = Estado), 
            by = c('MUNIC_RES'='IBGE','ano'='Ano','cidade'='Município','Região'='Região'))

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM = 
  left_join(vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF, 
            Gini_IVS_IDHM %>% mutate(IBGE = as.numeric(IBGE)) %>% select(-Ano), 
            by = c('MUNIC_RES'='IBGE','cidade'='Município'))

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM_Porte = 
  vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM %>% 
  mutate(porte_mun = case_when(População <= 20000 ~ 'Pequeno porte I',
                               População > 20000 & População <= 50000 ~ 'Pequeno porte II',
                               População > 50000 & População <= 100000 ~ 'Médio porte',
                               População > 100000 & População <= 900000 ~ 'Grande porte',
                               População > 900000 ~ 'Metrópole'))

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM_Porte =
  vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM_Porte %>% 
  mutate(Ciclo = case_when(ano == 2010 | ano == 2011 | ano == 2012 ~ 1,
                           ano == 2013 | ano == 2014 | ano == 2015 ~ 2,
                           ano == 2016 | ano == 2017 | ano == 2018 | ano == 2019 ~ 3))

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM_Porte_est_eq = 
  left_join(vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM_Porte,
            pmaq, by = c('Ciclo'='Ciclo','MUNIC_RES'='IBGE'))

write.xlsx(vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM_Porte_est_eq %>% as.data.frame(), 
           'Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 09-04-2024.xlsx', rowNames = F)
