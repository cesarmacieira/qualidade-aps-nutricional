####==========================================================
#### Trabalho Catarina - Construção do banco de dados Vigitel
####==========================================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
tryCatch({setwd("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional")},
         error = function(e) { setwd("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional") })

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
vigitel = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel para análises 23-05-2024.xlsx", sheet = 1)},
                   error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados Catarina Vigitel para análises 23-05-2024.xlsx", sheet = 1) })

notas_pmaq = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Notas PMAQ 3 ciclos 2010 a 2019.xlsx", sheet = 1)},
                      error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Notas PMAQ 3 ciclos 2010 a 2019.xlsx", sheet = 1) })

icsap = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/DadosICSAP_Capitais.xlsx", sheet = 1)},
                 error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/DadosICSAP_Capitais.xlsx", sheet = 1) })

Pop_Leitos_Planos_ESF = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/População Leitos Planos Privados Cob ESF 2010-2019.xlsx", sheet = 1)},
                                 error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/População Leitos Planos Privados Cob ESF 2010-2019.xlsx", sheet = 1) })

Gini_IVS_IDHM = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Gini, IVS e IDHM 2010.xlsx", sheet = 1)},
                         error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Gini, IVS e IDHM 2010.xlsx", sheet = 1) })

pmaq = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados estrutura e equipes PMAQ capitais.xlsx", sheet = 1)},
                error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados estrutura e equipes PMAQ capitais.xlsx", sheet = 1) })

####==================
#### Junção dos dados
####==================
vigitel_icsap = full_join(vigitel, icsap, by = c('cidade_nome'='Nome.do.Município','ano'='ANO_CMPT')) %>% as.data.frame()

vigitel_icsap_notas_pmaq = left_join(vigitel_icsap, notas_pmaq, by = c('MUNIC_RES'='IBGE','ano'='Ano'))

vigitel_icsap_notas_pmaq$Região = ifelse(vigitel_icsap_notas_pmaq$Região == 'Centro Oeste', 'Centro-Oeste', vigitel_icsap_notas_pmaq$Região)

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF = 
  left_join(vigitel_icsap_notas_pmaq, Pop_Leitos_Planos_ESF %>% mutate(IBGE = as.numeric(IBGE)) %>% rename(Cod_UF = Estado), 
            by = c('MUNIC_RES'='IBGE','ano'='Ano','cidade_nome'='Município','Região'='Região'))

vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF_Gini_IVS_IDHM = 
  left_join(vigitel_icsap_notas_pmaq_pop_leitos_planos_ESF, 
            Gini_IVS_IDHM %>% mutate(IBGE = as.numeric(IBGE)) %>% select(-Ano), 
            by = c('MUNIC_RES'='IBGE','cidade_nome'='Município'))

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
           'Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 13-08-2024.xlsx', rowNames = F)
