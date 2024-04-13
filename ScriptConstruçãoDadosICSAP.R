####========================================================
#### Trabalho Catarina - Construção do banco de dados ICSAP
####========================================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
#setwd("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina")#Diretório
setwd('C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional')

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
# icsap = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/ICSAP-2010-2019.xlsx",
#                        sheet=1)
# Gini.IVS = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/Dados Gini e IVS 2010.xlsx",
#                      sheet=1, detectDates=TRUE)

icsap = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/ICSAP-2010-2019.xlsx",
                  sheet=1)
Gini.IVS = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Gini e IVS 2010.xlsx",
                     sheet=1, detectDates=TRUE)

####=====================
#### Tratamento de dados
####=====================
head(icsap)
DescritivaCat(icsap$FX_ETARIA)
icsap1 = icsap %>% filter(FX_ETARIA == 'DE 0 A 04 ANOS' | FX_ETARIA == 'DE 05 A 19 ANOS' | 
                            FX_ETARIA == 'DE 20 A 59 ANOS' | FX_ETARIA == 'DE 60 A 79 ANOS')

DescritivaCat(icsap1$FX_ETARIA)

#Colocando 0 nas colunas de internações
icsap1[c("ANEMIA","ANGINA","ASMA","DEFICIENCIAS_NUTRICIONAIS","DIABETES_MELITUS",
         "DOENCA_INFLAMATORIA_ORGAOS_PELVICOS_FEMININOS","DOENCAS_CEREBROVASCULARES",
         "DOENCAS_PREVENIVEIS_POR_IMUNIZACAO_E_CONDICOES_SENSIVEIS","DOENCAS_PULMONARES",
         "DOENCAS_RELACIONADAS_AO_PRE-NATAL_E_PARTO",
         "EPILEPSIAS","GASTROENTERITES_INFECCIOSAS_E_COMPLICACOES",
         "HIPERTENSAO","INFECCAO_DA_PELE_E_TECIDO_SUBCUTANEO","INFECCAO_NO_RIM_E_TRATO_URINARIO",
         "INFECCOES_DE_OUVIDO,_NARIZ_E_GARGANTA","INSUFICIENCIA_CARDIACA","PNEUMONIAS_BACTERIANAS",
         "ULCERA_GASTROINTESTINAL")] = 
  lapply(icsap1[c("ANEMIA","ANGINA","ASMA","DEFICIENCIAS_NUTRICIONAIS","DIABETES_MELITUS",
                  "DOENCA_INFLAMATORIA_ORGAOS_PELVICOS_FEMININOS","DOENCAS_CEREBROVASCULARES",
                  "DOENCAS_PREVENIVEIS_POR_IMUNIZACAO_E_CONDICOES_SENSIVEIS","DOENCAS_PULMONARES",
                  "DOENCAS_RELACIONADAS_AO_PRE-NATAL_E_PARTO",
                  "EPILEPSIAS","GASTROENTERITES_INFECCIOSAS_E_COMPLICACOES",
                  "HIPERTENSAO","INFECCAO_DA_PELE_E_TECIDO_SUBCUTANEO","INFECCAO_NO_RIM_E_TRATO_URINARIO",
                  "INFECCOES_DE_OUVIDO,_NARIZ_E_GARGANTA","INSUFICIENCIA_CARDIACA","PNEUMONIAS_BACTERIANAS",
                  "ULCERA_GASTROINTESTINAL")], function(x) ifelse(is.na(x) == TRUE,0,x))

Gini.IVS$IBGE = as.numeric(Gini.IVS$Município.com.6.dígitos)
Gini.IVS$IVS = as.numeric(Gini.IVS$IVS)
Gini.IVS$IDHM = as.numeric(Gini.IVS$IDHM)
DadosICSAP = left_join(icsap1 %>% select(-c(NomeUF,X34,TaxaICSAPPadronizada10000sem79anos)),
                       Gini.IVS %>% select(IBGE,Nome.da.UF,Nome.do.Município), by=c("MUNIC_RES"="IBGE"))
#write.xlsx(DadosICSAP, 'DadosICSAP.xlsx')

#Capitais
DadosICSAP_Capitais = DadosICSAP %>%
  select(MUNIC_RES,Nome.do.Município,Nome.da.UF,Região,ANO_CMPT,POP,FX_ETARIA,SEXO,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,
         DIABETES_MELITUS,HIPERTENSAO) %>% 
  filter(Nome.do.Município == 'Porto Velho' | Nome.do.Município == 'Manaus' |
           (Nome.do.Município == 'Rio Branco' & Nome.da.UF == 'Acre') | (Nome.do.Município == 'Campo Grande' & Nome.da.UF == 'Mato Grosso do Sul')|
           Nome.do.Município == 'Macapá' | (Nome.da.UF == 'Distrito Federal' & Nome.do.Município == 'Brasília') |
           (Nome.do.Município == 'Boa Vista' & Nome.da.UF == 'Roraima') | Nome.do.Município == 'Cuiabá' |
           (Nome.do.Município == 'Palmas' & Nome.da.UF == 'Tocantins') | Nome.do.Município == 'São Paulo' |
           Nome.do.Município == 'Teresina' | Nome.do.Município == 'Rio de Janeiro' |
           (Nome.do.Município == 'Belém' & Nome.da.UF == 'Pará') | Nome.do.Município == 'Goiânia' |
           Nome.do.Município == 'Salvador' | Nome.do.Município == 'Florianópolis' |
           Nome.do.Município == 'São Luís' | Nome.do.Município == 'Maceió' |
           Nome.do.Município == 'Porto Alegre' | (Nome.do.Município == 'Curitiba' & Nome.da.UF == 'Paraná') |
           Nome.do.Município == 'Belo Horizonte' | Nome.do.Município == 'Fortaleza' |
           Nome.do.Município == 'Recife' | Nome.do.Município == 'João Pessoa' |
           Nome.do.Município == 'Aracaju' | Nome.do.Município == 'Natal' |
           Nome.do.Município == 'Vitória') %>%
  mutate(SomaICSAP = rowSums(select(., ANEMIA, DEFICIENCIAS_NUTRICIONAIS, DIABETES_MELITUS, HIPERTENSAO)),
         TaxaICSAP = SomaICSAP/POP,
         FX_ETARIA = case_when(FX_ETARIA == 'DE 0 A 04 ANOS' ~ '0 a 4 anos',
                               FX_ETARIA == 'DE 05 A 19 ANOS' ~ '5 a 19 anos',
                               FX_ETARIA == 'DE 20 A 59 ANOS' ~ '20 a 59 anos',
                               FX_ETARIA == 'DE 60 A 79 ANOS' ~ '60 a 79 anos'))
DadosICSAP_Capitais %>% filter(Nome.do.Município == 'Brasília')
write.xlsx(DadosICSAP_Capitais, 'DadosICSAP_Capitais.xlsx')

#Estados
# DadosICSAP_Estados = DadosICSAP %>%
#   select(Nome.da.UF,Região,ANO_CMPT,POP,FX_ETARIA,SEXO,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO) %>%
#   group_by(FX_ETARIA,SEXO,Nome.da.UF,Região,ANO_CMPT) %>% 
#   summarise(Pop_Total = sum(POP, na.rm = T),
#             ANEMIA_Total = sum(ANEMIA, na.rm = T),
#             DEFICIENCIAS_NUTRICIONAIS_Total = sum(DEFICIENCIAS_NUTRICIONAIS, na.rm = T),
#             DIABETES_MELITUS_Total = sum(DIABETES_MELITUS, na.rm = T),
#             HIPERTENSAO_Total = sum(HIPERTENSAO, na.rm = T)) %>% as.data.frame() %>% 
#   mutate(SomaICSAP = rowSums(select(.,ANEMIA_Total,DEFICIENCIAS_NUTRICIONAIS_Total,DIABETES_MELITUS_Total,HIPERTENSAO_Total)),
#          TaxaICSAP = SomaICSAP/Pop_Total)
# write.xlsx(DadosICSAP_Estados, 'DadosICSAP_Estados.xlsx')

#Regiões
# DadosICSAP_Regioes = DadosICSAP %>%
#   select(Região,ANO_CMPT,POP,FX_ETARIA,SEXO,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO) %>%
#   group_by(FX_ETARIA,SEXO,Região,ANO_CMPT) %>% 
#   summarise(Pop_Total = sum(POP, na.rm = T),
#             ANEMIA_Total = sum(ANEMIA, na.rm = T),
#             DEFICIENCIAS_NUTRICIONAIS_Total = sum(DEFICIENCIAS_NUTRICIONAIS, na.rm = T),
#             DIABETES_MELITUS_Total = sum(DIABETES_MELITUS, na.rm = T),
#             HIPERTENSAO_Total = sum(HIPERTENSAO, na.rm = T)) %>% as.data.frame() %>% 
#   mutate(SomaICSAP = rowSums(select(.,ANEMIA_Total,DEFICIENCIAS_NUTRICIONAIS_Total,DIABETES_MELITUS_Total,HIPERTENSAO_Total)),
#          TaxaICSAP = SomaICSAP/Pop_Total)
# write.xlsx(DadosICSAP_Regioes, 'DadosICSAP_Regioes.xlsx')

####========
#### Extras
####========
# DadosICSAP_Estados = DadosICSAP %>%
#   select(Nome.da.UF,Região,ANO_CMPT,POP,FX_ETARIA,SEXO,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO) %>%
#   group_by(FX_ETARIA,SEXO,Nome.da.UF,Região,ANO_CMPT) %>% 
#   summarise(Pop_Total = sum(POP, na.rm = T),
#             AIH_GERAL_Total = sum(AIH_GERAL, na.rm = T),
#             AIH_PARTO_Total = sum(AIH_PARTO, na.rm = T),
#             AIH_LIVRE_Total = sum(AIH_LIVRE, na.rm = T),
#             ANEMIA_Total = sum(ANEMIA, na.rm = T),
#             ANGINA_Total = sum(ANGINA, na.rm = T),
#             ASMA_Total = sum(ASMA, na.rm = T),
#             DEFICIENCIAS_NUTRICIONAIS_Total = sum(DEFICIENCIAS_NUTRICIONAIS, na.rm = T),
#             DIABETES_MELITUS_Total = sum(DIABETES_MELITUS, na.rm = T),
#             DOENCA_INFLAMATORIA_ORGAOS_PELVICOS_FEMININOS_Total = sum(DOENCA_INFLAMATORIA_ORGAOS_PELVICOS_FEMININOS, na.rm = T),
#             DOENCAS_CEREBROVASCULARES_Total = sum(DOENCAS_CEREBROVASCULARES, na.rm = T),
#             DOENCAS_PREVENIVEIS_POR_IMUNIZACAO_E_CONDICOES_SENSIVEIS_Total = sum(DOENCAS_PREVENIVEIS_POR_IMUNIZACAO_E_CONDICOES_SENSIVEIS, na.rm = T),
#             DOENCAS_PULMONARES_Total = sum(DOENCAS_PULMONARES, na.rm = T),
#             DOENCAS_RELACIONADAS_AO_PRE_NATAL_E_PARTO_Total = sum(`DOENCAS_RELACIONADAS_AO_PRE-NATAL_E_PARTO`, na.rm = T),
#             EPILEPSIAS_Total = sum(EPILEPSIAS, na.rm = T),
#             GASTROENTERITES_INFECCIOSAS_E_COMPLICACOES_Total = sum(GASTROENTERITES_INFECCIOSAS_E_COMPLICACOES, na.rm = T),
#             HIPERTENSAO_Total = sum(HIPERTENSAO, na.rm = T), 
#             INFECCAO_DA_PELE_E_TECIDO_SUBCUTANEO_Total = sum(INFECCAO_DA_PELE_E_TECIDO_SUBCUTANEO, na.rm = T),
#             INFECCAO_NO_RIM_E_TRATO_URINARIO_Total = sum(INFECCAO_NO_RIM_E_TRATO_URINARIO, na.rm = T),
#             INFECCOES_DE_OUVIDO_NARIZ_E_GARGANTA_Total = sum(`INFECCOES_DE_OUVIDO,_NARIZ_E_GARGANTA`, na.rm = T),
#             INSUFICIENCIA_CARDIACA_Total = sum(INSUFICIENCIA_CARDIACA, na.rm = T),
#             PNEUMONIAS_BACTERIANAS_Total = sum(PNEUMONIAS_BACTERIANAS, na.rm = T),
#             ULCERA_GASTROINTESTINAL_Total = sum(ULCERA_GASTROINTESTINAL, na.rm = T),
#             ICSAP_Total = sum(ICSAP, na.rm = T),
#             TAXAICSAP10000habfaixaetária_Media = mean(TAXAICSAP10000habfaixaetária, na.rm = T),
#             Fração_Pop_Brasileira_Media = mean(Fração_Pop_Brasileira, na.rm = T)) %>% as.data.frame() %>% 
#   mutate(SomaICSAP = rowSums(select(.,ANEMIA_Total,DEFICIENCIAS_NUTRICIONAIS_Total,DIABETES_MELITUS_Total,HIPERTENSAO_Total)),
#          TaxaICSAP = SomaICSAP/Pop_Total)
