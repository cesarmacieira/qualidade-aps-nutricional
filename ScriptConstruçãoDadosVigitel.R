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
#vigitel_originais = tryCatch({haven::read_dta("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/vigitel2006_2022_indicadores_2023.04.24 RClaro.dta")},
#                             error = function(e) { haven::read_dta("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/vigitel2006_2022_indicadores_2023.04.24 RClaro.dta") })

# vigitel_originais %>% filter(ano == 2014) %>% select(q76) %>% map(DescritivaCat)
# vigitel_originais %>% filter(ano == 2018) %>% select(q15) %>% map(DescritivaCat)

# vigitel = vigitel_originais %>% filter(ano >= 2010 & ano <= 2019) %>% 
#   select(ordem,pesorake,ano,cidade,regiao,q6,q7,civil,q8a,q8b,q8_anos,q88,
#          q9,q9_i,q11,q11_i,q16,q25,q27,q18,q20,q26,q28,q29,q15,
#          r301_a,r301_b,r301_c,r301_d,r301_e,r301_g,r301_l,
#          r302_a,r302_b,r302_c,r302_d,r302_e,r302_f,r302_g,r302_h,r302_i,r302_j,r302_k,r302_l,r302_m,
#          r144a,r144b,q75,q76,q76a)
# write.xlsx(vigitel %>% as.data.frame(), 'Dados Catarina Vigitel 21-03-2024.xlsx', rowNames=F)

vigitel = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel 21-03-2024.xlsx", sheet = 1)},
                   error = function(e) { read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados Catarina Vigitel 21-03-2024.xlsx", sheet = 1) })

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

vigitel$diab = case_when((vigitel$q76 != 1 & vigitel$ano != 2014) ~ 0,
                         (vigitel$q76 == 1 & vigitel$ano != 2014) ~ 1,
                         (vigitel$q76a != 1 & vigitel$ano == 2014) ~ 0, 
                         (vigitel$q76a == 1 & vigitel$ano == 2014) ~ 1)

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
                             vigitel$cidade == '4'	~ 'Boa Vista',
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
                             vigitel$cidade == '27' ~ 'Brasília')

vigitel$q7_2 = case_when(vigitel$q7 == '1' ~ 'Masculino',
                         vigitel$q7 == '2' ~ 'Feminino')

vigitel$sexo = case_when(vigitel$q7_2 == 'Masculino' ~ 1,
                         vigitel$q7_2 == 'Feminino' ~ 0)

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

DescritivaCat(vigitel$q6)
vigitel$idade_cat = case_when(vigitel$q6 <= 19 ~ '19 anos ou menos',
                              vigitel$q6 >= 20 & vigitel$q6 <= 59 ~ '20 a 59 anos',
                              vigitel$q6 >= 60 & vigitel$q6 <= 79 ~ '60 a 79 anos',
                              vigitel$q6 >= 80 ~ '80 anos ou mais')

vigitel$idade_cat_60a79 = case_when(vigitel$q6 >= 20 & vigitel$q6 <= 59 ~ 0,
                                    vigitel$q6 >= 60 & vigitel$q6 <= 79 ~ 1)

vigitel$civil_uniaoest_casado = case_when(vigitel$civil == '1' | vigitel$civil == '4' | vigitel$civil == '5' ~ 0,
                                          vigitel$civil == '2' | vigitel$civil == '3' ~ 1)

vigitel$plano_saude_nao = case_when(vigitel$q88 == '1' | vigitel$q88 == 'Baixo 2' ~ 0,
                                    vigitel$q88 == '3' ~ 1)

vigitel$IMC_cat = case_when( (vigitel$IMC < 18.5 & vigitel$q6 <= 59) ~ 'Baixo peso',
                             (vigitel$IMC < 22 & vigitel$q6 > 59) ~ 'Baixo peso',
                             (vigitel$IMC >= 18.5 & vigitel$IMC < 25 & vigitel$q6 <= 59) ~ 'Eutrofia',
                             (vigitel$IMC >= 22 & vigitel$IMC < 27 & vigitel$q6 > 59) ~ 'Eutrofia',
                             (vigitel$IMC >= 25 & vigitel$q6 <= 59) ~ 'Excesso de peso',
                             (vigitel$IMC >= 27 & vigitel$q6 > 59) ~ 'Excesso de peso')
vigitel$IMC_cat_baixo = case_when(vigitel$IMC_cat == 'Eutrofia' | vigitel$IMC_cat == 'Excesso de peso' ~ 0,
                                  vigitel$IMC_cat == 'Baixo peso' ~ 1)
vigitel$IMC_cat_excesso = case_when(vigitel$IMC_cat == 'Eutrofia' | vigitel$IMC_cat == 'Baixo peso' ~ 0,
                                    vigitel$IMC_cat == 'Excesso de peso' ~ 1)

vigitel$IMC_i_cat = case_when( (vigitel$IMC_i < 18.5 & vigitel$q6 <= 59) ~ 'Baixo peso',
                               (vigitel$IMC_i < 22 & vigitel$q6 > 59) ~ 'Baixo peso',
                               (vigitel$IMC_i >= 18.5 & vigitel$IMC_i < 25 & vigitel$q6 <= 59) ~ 'Eutrofia',
                               (vigitel$IMC_i >= 22 & vigitel$IMC_i < 27 & vigitel$q6 > 59) ~ 'Eutrofia',
                               (vigitel$IMC_i >= 25 & vigitel$q6 <= 59) ~ 'Excesso de peso',
                               (vigitel$IMC_i >= 27 & vigitel$q6 > 59) ~ 'Excesso de peso')
vigitel$IMC_i_cat_baixo = case_when(vigitel$IMC_i_cat == 'Eutrofia' | vigitel$IMC_i_cat == 'Excesso de peso' ~ 0,
                                    vigitel$IMC_i_cat == 'Baixo peso' ~ 1)
vigitel$IMC_i_cat_excesso = case_when(vigitel$IMC_i_cat == 'Eutrofia' | vigitel$IMC_i_cat == 'Baixo peso' ~ 0,
                                      vigitel$IMC_i_cat == 'Excesso de peso' ~ 1)

vigitel$cruadia_cat = case_when(vigitel$cruadia == 1 | vigitel$cruadia == 2 ~ 0,
                                vigitel$cruadia == 3 ~ 1)
vigitel$cozidadia_cat = case_when(vigitel$cozidadia == 1 | vigitel$cozidadia == 2 ~ 0,
                                  vigitel$cozidadia == 3 ~ 1)

dados = vigitel %>% filter(idade_cat == '20 a 59 anos' | idade_cat == '60 a 79 anos') %>% #| idade_cat == '80 anos ou mais') %>% 
  select(ordem,pesorake,ano,cidade,cidade_2,q6,idade_cat_60a79,idade_cat,sexo,q7,q7_2,civil,civil_uniaoest_casado,
         q8a,q8a_2,q8b,q8_anos,q88,plano_saude_nao,q9,q11,IMC,IMC_cat,IMC_cat_baixo,IMC_cat_excesso,
         q9_i,q11_i,IMC_i,IMC_i_cat,IMC_i_cat_baixo,IMC_i_cat_excesso,
         q16,hortareg,q25,q27,frutareg,flvreg,q18,cruadia,cruadia_cat,q20,cozidadia,cozidadia_cat,hortadia,q26,sucodia,
         q28,sofrutadia,frutadia,flvdia,flvreco,q29,refritl5,q15,feijao5,q75,hart,q76,diab)

write.xlsx(dados %>% as.data.frame(), 'Dados Catarina 23-05-2024.xlsx', rowNames = F)

####==================================
#### Agrupando por ano, cidade e sexo
####==================================
dados_sexo_prop = dados %>% filter(!is.na(sexo)) %>% group_by(ano, cidade, cidade_2) %>% 
  summarize(sexo_M_prop = sum(sexo * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_idade_60a79_prop = dados %>% filter(!is.na(idade_cat_60a79)) %>% group_by(ano, cidade_2) %>% 
  summarize(idade_60a79_prop = weighted.mean(idade_cat_60a79, pesorake)) %>% as.data.frame()
dados_civil_uniaoest_casado_prop = dados %>% filter(!is.na(civil_uniaoest_casado)) %>% group_by(ano, cidade_2) %>% 
  summarize(civil_uniaoest_casado_prop = sum(civil_uniaoest_casado * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_anos_de_estudo = dados %>% filter(!is.na(q8_anos)) %>% group_by(ano, cidade_2) %>% 
  summarize(anos_de_estudo = weighted.mean(q8_anos, pesorake)) %>% as.data.frame()
dados_plano_saude_nao_prop = dados %>% filter(!is.na(plano_saude_nao)) %>% group_by(ano, cidade_2) %>% 
  summarize(plano_saude_nao_prop = sum(plano_saude_nao * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_IMC_media = dados %>% filter(!is.na(IMC)) %>% group_by(ano, cidade_2) %>% 
  summarize(IMC_media = weighted.mean(IMC, pesorake)) %>% as.data.frame()
dados_IMC_cat_baixo_prop = dados %>% filter(!is.na(IMC_cat_baixo)) %>% group_by(ano, cidade_2) %>% 
  summarize(IMC_cat_baixo_prop = sum(IMC_cat_baixo * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_IMC_cat_excesso_prop = dados %>% filter(!is.na(IMC_cat_excesso)) %>% group_by(ano, cidade_2) %>% 
  summarize(IMC_cat_excesso_prop = sum(IMC_cat_excesso * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_IMC_i_media = dados %>% filter(!is.na(IMC_i)) %>% group_by(ano, cidade_2) %>% 
  summarize(IMC_i_media = weighted.mean(IMC_i, pesorake)) %>% as.data.frame()
dados_IMC_i_cat_baixo_prop = dados %>% filter(!is.na(IMC_i_cat_baixo)) %>% group_by(ano, cidade_2) %>% 
  summarize(IMC_i_cat_baixo_prop = sum(IMC_i_cat_baixo * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_IMC_i_cat_excesso_prop = dados %>% filter(!is.na(IMC_i_cat_excesso)) %>% group_by(ano, cidade_2) %>% 
  summarize(IMC_i_cat_excesso_prop = sum(IMC_i_cat_excesso * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_hortareg_prop = dados %>% filter(!is.na(hortareg)) %>% group_by(ano, cidade_2) %>% 
  summarize(hortareg_prop = sum(hortareg * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_frutareg_prop = dados %>% filter(!is.na(frutareg)) %>% group_by(ano, cidade_2) %>% 
  summarize(frutareg_prop = sum(frutareg * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_flvreg_prop = dados %>% filter(!is.na(flvreg)) %>% group_by(ano, cidade_2) %>% 
  summarize(flvreg_prop = sum(flvreg * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_cruadia_cat_prop = dados %>% filter(!is.na(cruadia_cat)) %>% group_by(ano, cidade_2) %>% 
  summarize(cruadia_cat_prop = sum(cruadia_cat * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_cozidadia_cat_prop = dados %>% filter(!is.na(cozidadia_cat)) %>% group_by(ano, cidade_2) %>% 
  summarize(cozidadia_cat_prop = sum(cozidadia_cat * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_hortadia_media = dados %>% filter(!is.na(hortadia)) %>% group_by(ano, cidade_2) %>% 
  summarize(hortadia_media = weighted.mean(hortadia, pesorake)) %>% as.data.frame()
dados_sucodia_media = dados %>% filter(!is.na(sucodia)) %>% group_by(ano, cidade_2) %>% 
  summarize(sucodia_media = weighted.mean(sucodia, pesorake)) %>% as.data.frame()
dados_sofrutadia_media = dados %>% filter(!is.na(sofrutadia)) %>% group_by(ano, cidade_2) %>% 
  summarize(sofrutadia_media = weighted.mean(sofrutadia, pesorake)) %>% as.data.frame()
dados_frutadia_media = dados %>% filter(!is.na(frutadia)) %>% group_by(ano, cidade_2) %>% 
  summarize(frutadia_media = weighted.mean(frutadia, pesorake)) %>% as.data.frame()
dados_flvdia_media = dados %>% filter(!is.na(flvdia)) %>% group_by(ano, cidade_2) %>% 
  summarize(flvdia_media = weighted.mean(flvdia, pesorake)) %>% as.data.frame()
dados_flvreco_prop = dados %>% filter(!is.na(flvreco)) %>% group_by(ano, cidade_2) %>% 
  summarize(flvreco_prop = sum(flvreco * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_refritl5_prop = dados %>% filter(!is.na(refritl5)) %>% group_by(ano, cidade_2) %>% 
  summarize(refritl5_prop = sum(refritl5 * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_feijao5_prop = dados %>% filter(!is.na(feijao5)) %>% group_by(ano, cidade_2) %>% 
  summarize(feijao5_prop = sum(feijao5 * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_hart_prop = dados %>% filter(!is.na(hart)) %>% group_by(ano, cidade_2) %>% 
  summarize(hart_prop = sum(hart * pesorake) / sum(pesorake)) %>% as.data.frame()
dados_diab_prop = dados %>% filter(!is.na(diab)) %>% group_by(ano, cidade_2) %>% 
  summarize(diab_prop = sum(diab * pesorake) / sum(pesorake)) %>% as.data.frame()

dados_agg0 = left_join(dados_sexo_prop, dados_idade_60a79_prop)
dados_agg1 = left_join(dados_agg0, dados_civil_uniaoest_casado_prop)
dados_agg2 = left_join(dados_agg1, dados_anos_de_estudo)
dados_agg3 = left_join(dados_agg2, dados_plano_saude_nao_prop)
dados_agg4 = left_join(dados_agg3, dados_IMC_media)
dados_agg5 = left_join(dados_agg4, dados_IMC_cat_baixo_prop)
dados_agg6 = left_join(dados_agg5, dados_IMC_cat_excesso_prop)
dados_agg7 = left_join(dados_agg6, dados_IMC_i_media)
dados_agg8 = left_join(dados_agg7, dados_IMC_i_cat_baixo_prop)
dados_agg9 = left_join(dados_agg8, dados_IMC_i_cat_excesso_prop)
dados_agg10 = left_join(dados_agg9, dados_hortareg_prop)
dados_agg11 = left_join(dados_agg10, dados_frutareg_prop)
dados_agg12 = left_join(dados_agg11, dados_flvreg_prop)
dados_agg13 = left_join(dados_agg12, dados_cruadia_cat_prop)
dados_agg14 = left_join(dados_agg13, dados_cozidadia_cat_prop)
dados_agg15 = left_join(dados_agg14, dados_hortadia_media)
dados_agg16 = left_join(dados_agg15, dados_sucodia_media)
dados_agg17 = left_join(dados_agg16, dados_sofrutadia_media)
dados_agg18 = left_join(dados_agg17, dados_frutadia_media)
dados_agg19 = left_join(dados_agg18, dados_flvdia_media)
dados_agg20 = left_join(dados_agg19, dados_flvreco_prop)
dados_agg21 = left_join(dados_agg20, dados_refritl5_prop)
dados_agg22 = left_join(dados_agg21, dados_feijao5_prop)
dados_agg23 = left_join(dados_agg22, dados_hart_prop)
dados_agg24 = left_join(dados_agg23, dados_diab_prop)

dados_agg25 = dados_agg24 %>% rename(cidade_nome = cidade_2)

write.xlsx(dados_agg25 %>% as.data.frame(), 'Dados Catarina Vigitel para análises 23-05-2024.xlsx', rowNames = F)
