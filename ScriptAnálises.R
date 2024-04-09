####==============================
#### Trabalho Catarina - Análises
####==============================
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
if(!require(plm)){ install.packages("plm"); require(plm)}

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

AnovaTeste = function(y, z, more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result <- summary(aov(y ~ factor(z)))
  p_valor_anova <- anova_result[[1]]$"Pr(>F)"[1]
  if(!require(PMCMRplus)){ 
    install.packages("PMCMRplus")
    require(PMCMRplus) 
  }
  CM <- pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p_ANOVA")
  rownames(tab)= levels(factor(z))
  
  model=list(tabela=tab, C.Multiplas=CM)
  model
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
#dados = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/Dados Catarina Vigitel ICSAP PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM 28-03-2024.xlsx", sheet = 1)
dados = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte 28-03-2024.xlsx", sheet = 1)

####==========
#### Análises
####==========
#cruadia_cat_prop, cozidadia_cat_prop e sucodia_media são compostos por 0's ou 1's

Tabela1 = 
  do.call(rbind,dados %>% select(idade_media,civil_uniaoest_casado_prop,anos_de_estudo,plano_saude_nao_prop,IMC_media,
                                 IMC_cat_baixo_prop,IMC_cat_excesso_prop,IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,
                                 hortareg_prop,frutareg_prop,flvreg_prop,hortadia_media,
                                 sofrutadia_media,frutadia_media,flvdia_media,flvreco_prop,refritl5_prop,feijao5_prop,
                                 hart_media,diab_prop,POP,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO,
                                 SomaICSAP,TaxaICSAP,Nota,População,Leitos.SUS,Taxa.Cob.Planos.Priv,Cobertura.ESF,IVS,IDHM,Gini) %>% 
            map(TesteDeNormalidade))
write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx')

####=====================
#### Comparações por ano
####=====================
variaveis = c("idade_media","civil_uniaoest_casado_prop","anos_de_estudo","plano_saude_nao_prop",
              "IMC_media","IMC_cat_baixo_prop","IMC_cat_excesso_prop","IMC_i_media",
              "IMC_i_cat_baixo_prop","IMC_i_cat_excesso_prop","hortareg_prop","frutareg_prop",
              "flvreg_prop","hortadia_media","sofrutadia_media","frutadia_media",
              "flvdia_media","flvreco_prop","refritl5_prop","feijao5_prop",
              "hart_media","diab_prop","POP","ANEMIA","DEFICIENCIAS_NUTRICIONAIS",
              "DIABETES_MELITUS","HIPERTENSAO","SomaICSAP","TaxaICSAP","Nota",
              "População","Leitos.SUS","Taxa.Cob.Planos.Priv","Cobertura.ESF","IVS","IDHM","Gini")
variaveis2 = c("idade_media","civil_uniaoest_casado_prop","anos_de_estudo","plano_saude_nao_prop",
               "IMC_media","IMC_cat_baixo_prop","IMC_cat_excesso_prop","IMC_i_media",
               "IMC_i_cat_baixo_prop","IMC_i_cat_excesso_prop","hortareg_prop","frutareg_prop",
               "flvreg_prop","hortadia_media","sofrutadia_media","frutadia_media",
               "flvdia_media","flvreco_prop","refritl5_prop",#"feijao5_prop",
               "hart_media","diab_prop","POP","ANEMIA","DEFICIENCIAS_NUTRICIONAIS",
               "DIABETES_MELITUS","HIPERTENSAO","SomaICSAP","TaxaICSAP","Nota",
               "População","Leitos.SUS","Taxa.Cob.Planos.Priv","Cobertura.ESF","IVS","IDHM","Gini")

AnovaTeste(dados$plano_saude_nao_prop,dados$ano)$tabela
AnovaTeste(dados$IMC_i_media,dados$ano)$tabela

KruskalTeste(dados$feijao5_prop,dados$ano)$C.Multiplas
round(AnovaTeste(dados$plano_saude_nao_prop,dados$ano)$C.Multiplas,4)
round(AnovaTeste(dados$IMC_i_media,dados$ano)$C.Multiplas,4)

Tabela2 = do.call(rbind,lapply(variaveis, function(variavel) {KruskalTeste(dados[[variavel]], dados$ano)$tabela}))
Tabela3 = do.call(rbind,lapply(variaveis2, function(variavel) {KruskalTeste(dados[[variavel]], dados$ano)$C.Multiplas}))
#write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)
#write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)
#write.xlsx(KruskalTeste(dados$feijao5_prop,dados$ano)$C.Multiplas %>% as.data.frame(),'CM Feijão5_prop.xlsx', rowNames = T)

dados2010 = dados %>% filter(ano == 2010)
dados2011 = dados %>% filter(ano == 2011)
dados2012 = dados %>% filter(ano == 2012)
dados2013 = dados %>% filter(ano == 2013)
dados2014 = dados %>% filter(ano == 2014)
dados2015 = dados %>% filter(ano == 2015)
dados2016 = dados %>% filter(ano == 2016)
dados2017 = dados %>% filter(ano == 2017)
dados2018 = dados %>% filter(ano == 2018)
dados2019 = dados %>% filter(ano == 2019)

dados_sumarizados = dados %>%
  group_by(Região, sexo, idade_cat, ano) %>%
  summarise(media_idade = mean(idade_media), total_populacao = sum(POP),
            media_civil_uniaoest_casado_prop = mean(civil_uniaoest_casado_prop), media_anos_de_estudo = mean(anos_de_estudo),
            media_plano_saude_nao_prop = mean(plano_saude_nao_prop), media_IMC_media = mean(IMC_media), 
            media_IMC_cat_baixo_prop = mean(IMC_cat_baixo_prop), media_IMC_cat_excesso_prop = mean(IMC_cat_excesso_prop),
            media_IMC_i_media = mean(IMC_i_media), media_IMC_i_cat_baixo_prop = mean(IMC_i_cat_baixo_prop),
            media_IMC_i_cat_excesso_prop = mean(IMC_i_cat_excesso_prop), media_hortareg_prop = mean(hortareg_prop),
            media_frutareg_prop = mean(frutareg_prop), media_flvreg_prop = mean(flvreg_prop),
            media_cruadia_cat_prop = mean(cruadia_cat_prop), media_cozidadia_cat_prop = mean(cozidadia_cat_prop),
            media_hortadia_media = mean(hortadia_media), media_sucodia_media = mean(sucodia_media),
            media_sofrutadia_media = mean(sofrutadia_media), media_frutadia_media = mean(frutadia_media),
            media_flvdia_media = mean(flvdia_media), media_flvreco_prop = mean(flvreco_prop), media_refritl5_prop = mean(refritl5_prop),
            media_feijao5_prop = mean(feijao5_prop), media_hart_media = mean(hart_media),
            media_diab_prop = mean(diab_prop), media_ANEMIA = mean(ANEMIA),
            media_DEFICIENCIAS_NUTRICIONAIS = mean(DEFICIENCIAS_NUTRICIONAIS),
            media_DIABETES_MELITUS = mean(DIABETES_MELITUS),
            media_HIPERTENSAO = mean(HIPERTENSAO), media_SomaICSAP = mean(SomaICSAP),
            media_TaxaICSAP = mean(TaxaICSAP), media_Nota = mean(Nota),
            soma_População = sum(População), soma_Leitos.SUS = sum(Leitos.SUS),
            media_Taxa.Cob.Planos.Priv = mean(Taxa.Cob.Planos.Priv),
            media_Cobertura.ESF = mean(Cobertura.ESF),
            media_IVS = mean(IVS), media_IDHM = mean(IDHM), media_Gini = mean(Gini))


write.xlsx(dados_sumarizados %>% as.data.frame(), 'Dados sumarizados por região, sexo, faixa etária e ano.xlsx', rowNames = F)

####=========
#### Modelos
####=========
dados %>% select(ano,sexo,Região,hortareg_prop,Nota,idade_media,civil_uniaoest_casado_prop,anos_de_estudo,plano_saude_nao_prop) %>% head


dados$ano = as.factor(dados$ano)

modelo_prais_winsten = plm(hortareg_prop ~ sexo + Região + Nota + idade_media + civil_uniaoest_casado_prop + 
                             anos_de_estudo + plano_saude_nao_prop, data = dados, index = c("ano"))
summary(modelo_prais_winsten)



####============================
#### Comparações por ano e sexo
####============================


KruskalAnoSexo = function(variavel) {
  resultados = lapply(list(dados2010, dados2011, dados2012, dados2013, dados2014, dados2015, 
                           dados2016, dados2017, dados2018, dados2019), function(df) {
                             KruskalTeste(df[[variavel]], df$Região)$tabela
                           })
  return(resultados)
}

KruskalAnoSexoCM = function(variavel) {
  resultados = lapply(list(dados2010, dados2011, dados2012, dados2013, dados2014, dados2015, 
                           dados2016, dados2017, dados2018, dados2019), function(df) {
                             KruskalTeste(df[[variavel]], df$Região)$C.Multiplas
                           })
  return(resultados)
}

MannWhitneyAnoSexo = function(variavel) {
  resultados = lapply(list(dados2010, dados2011, dados2012, dados2013, dados2014, dados2015, 
                           dados2016, dados2017, dados2018, dados2019), function(df) {
                             MannWhitney(df[[variavel]], df$sexo)
                             })
  return(resultados)
}

TesteTAnoSexo = function(variavel) {
  resultados = lapply(list(dados2010, dados2011, dados2012, dados2013, dados2014, dados2015, 
                           dados2016, dados2017, dados2018, dados2019), function(df) {
                             TesteT(df[[variavel]], df$sexo)
                           })
  return(resultados)
}

Tabela4 = do.call(rbind,KruskalAnoSexo("IMC_cat_baixo_prop"))
Tabela5 = do.call(rbind,KruskalAnoSexo("IMC_cat_excesso_prop"))
Tabela6 = do.call(rbind,KruskalAnoSexo("hortareg_prop"))
Tabela6.1 = do.call(rbind,KruskalAnoSexoCM("hortareg_prop"))

Tabela7 = do.call(rbind,KruskalAnoSexo("frutareg_prop"))
Tabela8 = do.call(rbind,KruskalAnoSexo("flvreg_prop"))
Tabela9 = do.call(rbind,KruskalAnoSexo("refritl5_prop"))
Tabela10 = do.call(rbind,KruskalAnoSexo("hart_media"))
Tabela11 = do.call(rbind,KruskalAnoSexo("diab_prop"))
Tabela12 = do.call(rbind,KruskalAnoSexo("ANEMIA"))
Tabela13 = do.call(rbind,KruskalAnoSexo("DEFICIENCIAS_NUTRICIONAIS"))
Tabela14 = do.call(rbind,KruskalAnoSexo("DIABETES_MELITUS"))
Tabela15 = do.call(rbind,KruskalAnoSexo("HIPERTENSAO"))
Tabela16 = do.call(rbind,KruskalAnoSexo("TaxaICSAP"))

write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)
write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)
write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)
write.xlsx(Tabela6.1 %>% as.data.frame(), 'Tabela 6.1.xlsx', rowNames = T)
write.xlsx(Tabela7 %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)
write.xlsx(Tabela8 %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)
write.xlsx(Tabela9 %>% as.data.frame(), 'Tabela 9.xlsx', rowNames = T)
write.xlsx(Tabela10 %>% as.data.frame(), 'Tabela 10.xlsx', rowNames = T)
write.xlsx(Tabela11 %>% as.data.frame(), 'Tabela 11.xlsx', rowNames = T)
write.xlsx(Tabela12 %>% as.data.frame(), 'Tabela 12.xlsx', rowNames = T)
write.xlsx(Tabela13 %>% as.data.frame(), 'Tabela 13.xlsx', rowNames = T)
write.xlsx(Tabela14 %>% as.data.frame(), 'Tabela 14.xlsx', rowNames = T)
write.xlsx(Tabela15 %>% as.data.frame(), 'Tabela 15.xlsx', rowNames = T)
write.xlsx(Tabela16 %>% as.data.frame(), 'Tabela 16.xlsx', rowNames = T)


####=========
#### Modelos
####=========
# 1 - Evolução da proporção de diagnóstico de diabetes e hipertensão
# 2 - Evolução do IMC (média, baixo peso e excesso de peso)
# 3 - Evolução dos indicadores de consumo alimentar
# 4 - Evolução das taxas de ICSAP
# 5 - Evolução das notas do PMAQ