####========================================================
#### Trabalho Catarina - Construção do banco de dados ICSAP
####========================================================
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
icsap = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/ICSAP-2010-2019.xlsx", sheet = 1)},
                 error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/ICSAP-2010-2019.xlsx", sheet = 1) })
Gini.IVS = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Gini e IVS 2010.xlsx", sheet = 1, detectDates=TRUE)},
                    error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados Gini e IVS 2010.xlsx", sheet = 1, detectDates=TRUE) })

####=====================
#### Tratamento de dados
####=====================
icsap1 = icsap %>% filter(FX_ETARIA == 'DE 20 A 59 ANOS' | FX_ETARIA == 'DE 60 A 79 ANOS')
icsap2 = icsap1 %>% filter(MUNIC_RES == 120040 | MUNIC_RES == 270430 | MUNIC_RES == 160030 | 
                             MUNIC_RES == 130260 | MUNIC_RES == 292740 | MUNIC_RES == 230440 | 
                             MUNIC_RES == 530010 | MUNIC_RES == 320530 | MUNIC_RES == 520870 | 
                             MUNIC_RES == 211130 | MUNIC_RES == 510340 | MUNIC_RES == 500270 | 
                             MUNIC_RES == 310620 | MUNIC_RES == 150140 | MUNIC_RES == 250750 | 
                             MUNIC_RES == 410690 | MUNIC_RES == 261160 | MUNIC_RES == 221100 |
                             MUNIC_RES == 330455 | MUNIC_RES == 240810 | MUNIC_RES == 431490 | 
                             MUNIC_RES == 110020 | MUNIC_RES == 140010 | MUNIC_RES == 420540 | 
                             MUNIC_RES == 355030 | MUNIC_RES == 280030 | MUNIC_RES == 172100)

#Colocando 0 nas colunas de internações
icsap2[c("ANEMIA","ANGINA","ASMA","DEFICIENCIAS_NUTRICIONAIS","DIABETES_MELITUS",
         "DOENCA_INFLAMATORIA_ORGAOS_PELVICOS_FEMININOS","DOENCAS_CEREBROVASCULARES",
         "DOENCAS_PREVENIVEIS_POR_IMUNIZACAO_E_CONDICOES_SENSIVEIS","DOENCAS_PULMONARES",
         "DOENCAS_RELACIONADAS_AO_PRE-NATAL_E_PARTO",
         "EPILEPSIAS","GASTROENTERITES_INFECCIOSAS_E_COMPLICACOES",
         "HIPERTENSAO","INFECCAO_DA_PELE_E_TECIDO_SUBCUTANEO","INFECCAO_NO_RIM_E_TRATO_URINARIO",
         "INFECCOES_DE_OUVIDO,_NARIZ_E_GARGANTA","INSUFICIENCIA_CARDIACA","PNEUMONIAS_BACTERIANAS",
         "ULCERA_GASTROINTESTINAL")] = 
  lapply(icsap2[c("ANEMIA","ANGINA","ASMA","DEFICIENCIAS_NUTRICIONAIS","DIABETES_MELITUS",
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
Gini.IVS1 = Gini.IVS %>% filter(IBGE == 120040 | IBGE == 270430 | IBGE == 160030 | 
                                  IBGE == 130260 | IBGE == 292740 | IBGE == 230440 | 
                                  IBGE == 530010 | IBGE == 320530 | IBGE == 520870 | 
                                  IBGE == 211130 | IBGE == 510340 | IBGE == 500270 | 
                                  IBGE == 310620 | IBGE == 150140 | IBGE == 250750 | 
                                  IBGE == 410690 | IBGE == 261160 | IBGE == 221100 |
                                  IBGE == 330455 | IBGE == 240810 | IBGE == 431490 | 
                                  IBGE == 110020 | IBGE == 140010 | IBGE == 420540 | 
                                  IBGE == 355030 | IBGE == 280030 | IBGE == 172100)

icsap2_agg = icsap2 %>% 
  select(MUNIC_RES,ANO_CMPT,Região,POP,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO) %>% 
  group_by(MUNIC_RES,ANO_CMPT,Região) %>% 
  summarise(POP = sum(POP, na.rm = T),
            ANEMIA = sum(ANEMIA, na.rm = T), 
            DEFICIENCIAS_NUTRICIONAIS = sum(DEFICIENCIAS_NUTRICIONAIS, na.rm = T),
            DIABETES_MELITUS = sum(DIABETES_MELITUS, na.rm = T),
            HIPERTENSAO = sum(HIPERTENSAO, na.rm = T)) %>% as.data.frame()

DadosICSAP = left_join(icsap2_agg,
                       Gini.IVS1 %>% select(IBGE,Nome.da.UF,Nome.do.Município), 
                       by=c("MUNIC_RES"="IBGE"))
#write.xlsx(DadosICSAP, 'DadosICSAP.xlsx')

#Capitais
DadosICSAP_Capitais = DadosICSAP %>%
  mutate(SomaICSAP = rowSums(select(., ANEMIA, DEFICIENCIAS_NUTRICIONAIS, DIABETES_MELITUS, HIPERTENSAO)),
         TaxaICSAP = SomaICSAP/POP,
         TaxaANEMIA = ANEMIA/POP,
         TaxaDEFICIENCIAS_NUTRICIONAIS = DEFICIENCIAS_NUTRICIONAIS/POP,
         TaxaDIABETES_MELITUS = DIABETES_MELITUS/POP,
         TaxaHIPERTENSAO = HIPERTENSAO/POP) %>% 
  mutate(TaxaICSAP = TaxaICSAP*1000, TaxaANEMIA = TaxaANEMIA*1000,
         TaxaDEFICIENCIAS_NUTRICIONAIS = TaxaDEFICIENCIAS_NUTRICIONAIS*1000,
         TaxaDIABETES_MELITUS = TaxaDIABETES_MELITUS*1000,
         TaxaHIPERTENSAO = TaxaHIPERTENSAO*1000)
DadosICSAP_Capitais %>% filter(Nome.do.Município == 'Porto Velho')
write.xlsx(DadosICSAP_Capitais, 'DadosICSAP_Capitais.xlsx')
