####==========================================================
#### Trabalho Catarina - Construção do banco de dados Vigitel
####==========================================================
####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
#setwd("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina")#Diretório
tryCatch({
  setwd("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional")
}, error = function(e) {
  setwd("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional")
})

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

####=============================
#### Carregando o banco de dados 
####=============================
#vigitel = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/Dados Catarina Vigitel para análises 20-03-2024.xlsx", sheet = 1)
vigitel = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel para análises 20-03-2024.xlsx", sheet = 1)
pmaq = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Notas PMAQ 3 ciclos 2010 a 2019.xlsx", sheet = 1)
icsap = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/DadosICSAP_Capitais.xlsx",
                  sheet=1)

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

vigitel_icsap = left_join(vigitel, icsap_agg, by = c('cidade'='Nome.do.Município','sexo'='SEXO',
                                                     'ano'='ANO_CMPT','idade_cat'='FX_ETARIA'))
vigitel_icsap_pmaq = left_join(vigitel_icsap, pmaq, by = c('MUNIC_RES'='IBGE','ano'='Ano'))

#write.xlsx(vigitel_icsap_pmaq %>% as.data.frame(), 'Dados Catarina Vigitel ICSAP PMAQ 20-03-2024.xlsx', rowNames = F)
