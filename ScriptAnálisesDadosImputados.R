####==============================
#### Trabalho Catarina - Análises
####==============================
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
if(!require(geepack)){ install.packages("geepack"); require(geepack)}
#if(!require(mice)){ install.packages("mice"); require(mice)}

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

DescritivaNumMais2Grupos = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  rownames(tab)= levels(factor(z))
  tab
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

FriedmanTeste = function(y, z, id, more = F){
  dados = data.frame(y = y, grupos = z, id = id)
  dados_agg = dados %>% select(y,grupos,id) %>% group_by(grupos,id) %>%
    summarize(media = mean(y, na.rm = TRUE))
  tab = matrix(NA, length(levels(factor(dados_agg$grupos))), 10)
  for(i in 1:length(levels(factor(dados_agg$grupos)))){ 
    desc = tapply(dados_agg$media, factor(dados_agg$grupos),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  p_valor = rep(friedman.test(media ~ grupos | id, data = dados_agg)$p.value, length(levels(factor(dados_agg$grupos))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(dados_agg$grupos))
  dados_CM = dados_agg %>% na.omit()
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = pairwise.wilcox.test(dados_CM$media, factor(dados_CM$grupos), p.adjust.method = "bonferroni")$p.value
  CM = frdAllPairsConoverTest(y = dados_CM$media, groups = dados_CM$grupos, 
                              blocks = dados_CM$id, p.adjust.method = 'none')$p.value
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

AnovaIndepTeste = function(y, z, CM_teste = "bonferroni", more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = summary(aov(y ~ factor(z)))
  p_valor_anova = anova_result[[1]]$"Pr(>F)"[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  if(CM_teste == "tukey") {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == "bonferroni") {
    if(!require(PMCMRplus)){ 
      install.packages("PMCMRplus")
      require(PMCMRplus) 
    }
    CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  }
  tab = cbind(tab, p_valor_anova)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p_ANOVA")
  rownames(tab)= levels(factor(z))
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

AnovaDepTeste = function(y, z, unid_amostral, CM_teste = "tukey", more = FALSE){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  anova_result = aov(y ~ factor(z) + Error(factor(unid_amostral)), data = data.frame(y, z, unid_amostral))
  p_valor_anova = summary(anova_result)[[1]]$"Pr(>F)"[1]
  #CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  if(CM_teste == "tukey") {
    CM = TukeyHSD(aov(y ~ factor(z)))$`factor(z)`
  } else if(CM_teste == "bonferroni") {
    if(!require(PMCMRplus)){install.packages("PMCMRplus"); require(PMCMRplus)}
    CM = pairwise.t.test(y, factor(z), p.adjust.method = "bonferroni")$p.value
  }
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

TesteTindep = function(y, x, more = F) {
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

TesteDeNormalidadeGrupos = function(y, z){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  dados = data.frame(y = y, Grupos = as.factor(z))
  if(dim(dados)[1] < 5000){
    result = dados %>% group_by(Grupos)%>% na.omit() %>%
      summarise(ShapiroWilk = round(shapiro.test(y)$p.value,3),
                ShapiroFrancia = round(sf.test(y)$p.value,3),
                AndersonDarling = round(ad.test(y)$p.value,3),
                KolmogorovSmirnov = round(ks.test(y, "pnorm", 
                                                  mean(y, na.rm = T), 
                                                  sd(y, na.rm = T))$p.value,3),
                Lilliefors = round(lillie.test(y)$p.value,3),
                CramerVonMises = round(cvm.test(y)$p.value,3)) %>% na.omit()
  }else{
    result = dados %>% group_by(Grupos) %>% na.omit() %>%
      summarise(ShapiroWilk = "N > 5000",
                ShapiroFrancia = "N > 5000",
                AndersonDarling = round(ad.test(y)$p.value,3),
                KolmogorovSmirnov = round(ks.test(y, "pnorm", 
                                                  mean(y, na.rm = T), 
                                                  sd(y, na.rm = T))$p.value,3),
                Lilliefors = round(lillie.test(y)$p.value,3),
                CramerVonMises = round(cvm.test(y)$p.value,3))
  }
  return(result)
}

HomogeneidadeVariancias = function(y, z){
  if(!require(car)){ install.packages("car"); require(car)}
  valor_p_Levene = leveneTest(y ~ as.factor(z))$`Pr(>F)`[1]
  return(valor_p_Levene)
}

TabelaGEEGama = function(modelo,casasdecimaisExpB=F){
  options(OutDec=",")
  if(casasdecimaisExpB == F){
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = exp(summary(modelo)$coefficients[,1]),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),3),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }else{
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = round(exp(summary(modelo)$coefficients[,1]),casasdecimaisExpB),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C." = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"; ",
                                        round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }
  return(Tabela)
}

TabelaGEENormal = function(modelo){
  options(OutDec=",")
  Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                      "β" = summary(modelo)$coefficients[,1],
                      "I.C. (95%)" = paste0("[",round(summary(modelo)$coefficients[,1]-(1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                            round(summary(modelo)$coefficients[,1]+(1.96*summary(modelo)$coefficients[,2]),3),"]"),
                      "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  return(Tabela)
}

####=============================
#### Carregando o banco de dados 
####=============================
dados_completos = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados com imputações.xlsx", sheet = 1)},
                           error = function(e) {read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados com imputações.xlsx", sheet = 1)})

####=============
#### Correlações
####=============
dados_completos = dados_completos %>% mutate(Indicador = IMC_i_cat_excesso_prop + flvreg_prop + flvreco_prop + 
                                               refritl5_prop + feijao5_prop + hart_prop + diab_prop)

dados_corr = 
  dados_completos %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                             IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                             feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix
Tabela25 = cbind(Hmisc::rcorr(dados_corr, type = 'spearman')$r, Hmisc::rcorr(dados_corr, type = 'spearman')$P)
#write.xlsx(Tabela25 %>% as.data.frame(),'Tabela 25.xlsx', rowNames = T)

dados2010 = dados_completos %>% filter(ano == 2010)
dados2011 = dados_completos %>% filter(ano == 2011)
dados2012 = dados_completos %>% filter(ano == 2012)
dados2013 = dados_completos %>% filter(ano == 2013)
dados2014 = dados_completos %>% filter(ano == 2014)
dados2015 = dados_completos %>% filter(ano == 2015)
dados2016 = dados_completos %>% filter(ano == 2016)
dados2017 = dados_completos %>% filter(ano == 2017)
dados2018 = dados_completos %>% filter(ano == 2018)
dados2019 = dados_completos %>% filter(ano == 2019)

dados_corr2010 = 
  dados2010 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2011 = 
  dados2011 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2012 = 
  dados2012 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

Tabela26 = rbind(cbind(Hmisc::rcorr(dados_corr2010, type = 'spearman')$r, Hmisc::rcorr(dados_corr2010, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2011, type = 'spearman')$r, Hmisc::rcorr(dados_corr2011, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2012, type = 'spearman')$r, Hmisc::rcorr(dados_corr2012, type = 'spearman')$P))
#write.xlsx(Tabela26 %>% as.data.frame(),'Tabela 26.xlsx', rowNames = T)

dados_corr2013 = 
  dados2013 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2014 = 
  dados2014 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2015 = 
  dados2015 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

Tabela27 = rbind(cbind(Hmisc::rcorr(dados_corr2013, type = 'spearman')$r, Hmisc::rcorr(dados_corr2013, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2014, type = 'spearman')$r, Hmisc::rcorr(dados_corr2014, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2015, type = 'spearman')$r, Hmisc::rcorr(dados_corr2015, type = 'spearman')$P))
#write.xlsx(Tabela27 %>% as.data.frame(),'Tabela 27.xlsx', rowNames = T)

dados_corr2016 = 
  dados2016 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2017 = 
  dados2017 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2018 = 
  dados2018 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2019 = 
  dados2019 %>% select(Indicador,IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,
                       IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

Tabela28 = rbind(cbind(Hmisc::rcorr(dados_corr2016, type = 'spearman')$r, Hmisc::rcorr(dados_corr2016, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2017, type = 'spearman')$r, Hmisc::rcorr(dados_corr2017, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2018, type = 'spearman')$r, Hmisc::rcorr(dados_corr2018, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2019, type = 'spearman')$r, Hmisc::rcorr(dados_corr2019, type = 'spearman')$P))
#write.xlsx(Tabela28 %>% as.data.frame(),'Tabela 28.xlsx', rowNames = T)

####====================
#### Modelo - Indicador
####====================
####========
#### Normal
####========
hist(dados_completos$Indicador)
ks.test(dados_completos$Indicador, "pnorm", mean(dados_completos$Indicador, na.rm = T), sd(dados_completos$Indicador, na.rm = T))

fit_Indicador = lm(Indicador ~ factor(sexo)*factor(ano), data=dados_completos)
res_Indicador0 = subset(fit_Indicador$residuals, dados_completos$ano == 2010)
res_Indicador1 = subset(fit_Indicador$residuals, dados_completos$ano == 2011)
res_Indicador2 = subset(fit_Indicador$residuals, dados_completos$ano == 2012)
res_Indicador3 = subset(fit_Indicador$residuals, dados_completos$ano == 2013)
res_Indicador4 = subset(fit_Indicador$residuals, dados_completos$ano == 2014)
res_Indicador5 = subset(fit_Indicador$residuals, dados_completos$ano == 2015)
res_Indicador6 = subset(fit_Indicador$residuals, dados_completos$ano == 2016)
res_Indicador7 = subset(fit_Indicador$residuals, dados_completos$ano == 2017)
res_Indicador8 = subset(fit_Indicador$residuals, dados_completos$ano == 2018)
res_Indicador9 = subset(fit_Indicador$residuals, dados_completos$ano == 2019)

res_Indicador = data.frame(res_Indicador0, res_Indicador1, res_Indicador2, res_Indicador3, res_Indicador4, res_Indicador5, res_Indicador6, res_Indicador7, res_Indicador8, res_Indicador9)
cor(res_Indicador)
cov(res_Indicador)

uni_Ind1 = geeglm(Indicador ~ sexo, id = cidade, 
                  data = dados_completos %>% select(Indicador,sexo,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind2 = geeglm(Indicador ~ idade_cat, id = cidade, 
                  data = dados_completos %>% select(Indicador,idade_cat,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind3 = geeglm(Indicador ~ anos_de_estudo, id = cidade, 
                  data = dados_completos %>% select(Indicador,anos_de_estudo,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind4 = geeglm(Indicador ~ plano_saude_nao_prop, id = cidade, 
                  data = dados_completos %>% select(Indicador,plano_saude_nao_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind5 = geeglm(Indicador ~ Nota, id = cidade, 
                  data = dados_completos %>% select(Indicador,sexo,Nota,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind6 = geeglm(Indicador ~ IVS, id = cidade, 
                  data = dados_completos %>% select(Indicador,sexo,idade_cat,anos_de_estudo,
                                                    plano_saude_nao_prop,Nota,IVS,IDHM,Gini,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind7 = geeglm(Indicador ~ IDHM, id = cidade, 
                  data = dados_completos %>% select(Indicador,sexo,idade_cat,anos_de_estudo,
                                                    plano_saude_nao_prop,Nota,IVS,IDHM,Gini,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_Ind8 = geeglm(Indicador ~ Gini, id = cidade, 
                  data = dados_completos %>% select(Indicador,sexo,idade_cat,anos_de_estudo,
                                                    plano_saude_nao_prop,Nota,IVS,IDHM,Gini,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_completos$Indicador,dados_completos$IVS)
cor.test(dados_completos$Indicador,dados_completos$IDHM)
cor.test(dados_completos$Indicador,dados_completos$Gini)

Tabela29.1 = rbind(TabelaGEENormal(uni_Ind1),TabelaGEENormal(uni_Ind2),
                   TabelaGEENormal(uni_Ind3),TabelaGEENormal(uni_Ind4),
                   TabelaGEENormal(uni_Ind5),#TabelaGEENormal(uni_Ind6),
                   TabelaGEENormal(uni_Ind7)
                   #TabelaGEENormal(uni_Ind8)
)
#write.xlsx(Tabela29.1 %>% as.data.frame(), 'Tabela 29.1.xlsx', rowNames = F)

#Sem interação
multi_semint_Ind1 = geeglm(Indicador ~ sexo + 
                             idade_cat + #anos_de_estudo + 
                             plano_saude_nao_prop + Nota + 
                             IVS, 
                           id = cidade, data = dados_completos %>% 
                             select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                           family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_Ind1)
#write.xlsx(TabelaGEENormal(multi_semint_Ind1) %>% as.data.frame(), 'Tabela 29.2.xlsx', rowNames = F)

hist(multi_semint_Ind1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_Ind1$residuals),max(multi_semint_Ind1$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_Ind1$residuals),max(multi_semint_Ind1$residuals), length.out = 100),
            mean=mean(multi_semint_Ind1$residuals),sd=sd(multi_semint_Ind1$residuals)))

qqnorm(multi_semint_Ind1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_Ind1$residuals)

ks.test(multi_semint_Ind1$residuals, "pnorm", mean(multi_semint_Ind1$residuals, na.rm = T), sd(multi_semint_Ind1$residuals, na.rm = T))

#Com interação:IVS não
multi_Ind1 = geeglm(Indicador ~ sexo*idade_cat + Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop, 
                    id = cidade, data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_Ind1)

#plano_saude_nao_prop*IDHM
multi_Ind2 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_Ind2)

#anos_de_estudo*Nota
multi_Ind3 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_Ind3)

#anos_de_estudo*plano_saude_nao_prop
multi_Ind4 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*IDHM +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_Ind4)

#idade_cat*anos_de_estudo
multi_Ind5 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*IDHM +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_Ind5)

#Nota*Gini
multi_diab6 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab6)

#sexo*Gini
multi_diab7 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                       idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab7)

#idade_cat*Gini
multi_diab8 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                       idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab8)

#idade_cat*Nota
multi_diab9 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                       idade_cat*plano_saude_nao_prop +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab9)

#plano_saude_nao_prop*Gini
multi_diab10 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab10)

#sexo*plano_saude_nao_prop
multi_diab11 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + Nota +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab11)

#sexo*plano_saude_nao_prop
multi_diab12 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab12)
#write.xlsx(TabelaGEENormal(multi_diab12) %>% as.data.frame(), 'Tabela 23.3.xlsx', rowNames = F)

hist(multi_diab12$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_diab12$residuals),max(multi_diab12$residuals), length.out = 100),
      dnorm(seq(min(multi_diab12$residuals),max(multi_diab12$residuals), length.out = 100),
            mean=mean(multi_diab12$residuals),sd=sd(multi_diab12$residuals)))

qqnorm(multi_diab12$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_diab12$residuals)

ks.test(multi_diab12$residuals, "pnorm", mean(multi_diab12$residuals, na.rm = T), sd(multi_diab12$residuals, na.rm = T))


####=======
#### Gamma
####=======
unig_Indicador1 = geeglm(Indicador ~ sexo, id = cidade, 
                         data = dados_completos %>% select(Indicador,sexo,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador2 = geeglm(Indicador ~ idade_cat, id = cidade, 
                         data = dados_completos %>% select(Indicador,idade_cat,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador3 = geeglm(Indicador ~ anos_de_estudo, id = cidade, 
                         data = dados_completos %>% select(Indicador,anos_de_estudo,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador4 = geeglm(Indicador ~ plano_saude_nao_prop, id = cidade, 
                         data = dados_completos %>% select(Indicador,plano_saude_nao_prop,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador5 = geeglm(Indicador ~ Nota, id = cidade, 
                         data = dados_completos %>% select(Indicador,sexo,Nota,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador6 = geeglm(Indicador ~ IVS, id = cidade, 
                         data = dados_completos %>% select(Indicador,sexo,idade_cat,anos_de_estudo,
                                                           plano_saude_nao_prop,Nota,IVS,IDHM,Gini,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador7 = geeglm(Indicador ~ IDHM, id = cidade, 
                         data = dados_completos %>% select(Indicador,sexo,idade_cat,anos_de_estudo,
                                                           plano_saude_nao_prop,Nota,IVS,IDHM,Gini,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

unig_Indicador8 = geeglm(Indicador ~ Gini, id = cidade, 
                         data = dados_completos %>% select(Indicador,sexo,idade_cat,anos_de_estudo,
                                                           plano_saude_nao_prop,Nota,IVS,IDHM,Gini,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")

cor.test(dados_completos$Indicador,dados_completos$IVS)
cor.test(dados_completos$Indicador,dados_completos$IDHM)
cor.test(dados_completos$Indicador,dados_completos$Gini)

Tabela30.1 = rbind(TabelaGEEGama(unig_Indicador1),TabelaGEEGama(unig_Indicador2),
                   TabelaGEEGama(unig_Indicador3),TabelaGEEGama(unig_Indicador4),
                   TabelaGEEGama(unig_Indicador5),#TabelaGEEGama(unig_Indicador6),
                   TabelaGEEGama(unig_Indicador7)
                   #TabelaGEEGama(unig_Indicador8)
)
#write.xlsx(Tabela30.1 %>% as.data.frame(), 'Tabela 30.1.xlsx', rowNames = F)

#Sem interação
multig_semint_Indicador1 = geeglm(Indicador ~ sexo + 
                                    idade_cat + anos_de_estudo + 
                                    plano_saude_nao_prop + Nota,# + IDHM, 
                                  id = cidade, data = dados_completos %>% 
                                    select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                  family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_Indicador1)
#write.xlsx(TabelaGEEGama(multig_semint_Indicador1) %>% as.data.frame(), 'Tabela 30.2.xlsx', rowNames = F)

multig_Indicador1 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota + sexo*IDHM +
                             idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                             anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                             plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                             Nota*IDHM, 
                           id = cidade, data = dados_completos %>% 
                             select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                           family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_Indicador1)

multig_Indicador1 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                             idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                             anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                             plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                             Nota*Gini, 
                           id = cidade, data = dados_completos %>% 
                             select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                           family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_Indicador1)


#Com interação
multig_Indicador1 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                             idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                             anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                             plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                             Nota*Gini, 
                           id = cidade, data = dados_completos %>% 
                             select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                           family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_Indicador1)

#plano_saude_nao_prop*Nota
multi_Indicador2 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_Indicador2)

#idade_cat*plano_saude_nao_prop
multi_Indicador3 = geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_Indicador3)

#anos_de_estudo*Nota
multi_TaxaICSAP4 = geeglm(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP4)

#sexo*plano_saude_nao_prop
multi_TaxaICSAP5 = geeglm(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP5)
#write.xlsx(TabelaGEEGama(multi_TaxaICSAP5) %>% as.data.frame(), 'Tabela 24.3.xlsx', rowNames = F)

hnp::hnp(multi_TaxaICSAP5, resid.type='pearson')

####===========
#### Testes
####===========

library(glmmTMB)
dados_completos %>% 
  select(Indicador, sexo, idade_cat, anos_de_estudo, plano_saude_nao_prop, Nota, Gini, cidade) %>% head()
modelo_misto_beta = glmmTMB(Indicador ~ sexo + idade_cat + anos_de_estudo + plano_saude_nao_prop + Nota + Gini +
                              (1 | cidade),
                            family = beta_family(link = "logit"),
                            data = dados_completos %>% 
                              select(Indicador, sexo, idade_cat, anos_de_estudo, plano_saude_nao_prop, Nota, Gini, cidade) %>% 
                              na.omit())

Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
feijao5_prop,hart_prop,diab_prop,anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini

if(!require(lme4)){ install.packages("lme4"); require(lme4)}
geeglm(Indicador ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
         plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
         Nota*Gini, 
       id = cidade, data = dados_completos %>% 
         select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
       family = Gamma(link = "log"), corstr = "exchangeable")

geeglm(Indicador ~ sexo + idade_cat + anos_de_estudo + plano_saude_nao_prop + Nota + Gini, 
       id = cidade, data = dados_completos %>% 
         select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
       family = Gamma(link = "log"), corstr = "exchangeable")

#Gini nao, IDHm nao

modeloGLMM0 = glmer(Indicador ~ (1|cidade_nome) + sexo*idade_cat + sexo*IVS +
                      idade_cat*anos_de_estudo + Nota + 
                      plano_saude_nao_prop*IVS,
                    family = Gamma(link="log"), data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome), nAGQ=0)
summary(modeloGLMM0)
#########################################
modeloGLMM0 = glmer(Indicador ~ (1|cidade_nome) + sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                      idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                      plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                      Nota*IVS,
                    family = Gamma(link="log"), data = dados_completos %>% 
                      select(Indicador,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome), nAGQ=0)
####=========
#### Modelos
####=========
dados %>% select(ano,sexo,Região,hortareg_prop,Nota,idade_media,civil_uniaoest_casado_prop,anos_de_estudo,plano_saude_nao_prop) %>% head


dados$ano = as.factor(dados$ano)

modelo_prais_winsten = plm(hortareg_prop ~ sexo + Região + Nota + idade_media + civil_uniaoest_casado_prop + 
                             anos_de_estudo + plano_saude_nao_prop, data = dados, index = c("ano"))
summary(modelo_prais_winsten)