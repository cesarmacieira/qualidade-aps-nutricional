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

TabelaGLMMBeta = function(modelo){
  options(OutDec=",")
  Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients$cond),
                      "β" = summary(modelo)$coefficients$cond[,1],
                      "Exp β" = exp(summary(modelo)$coefficients$cond[,1]),
                      "Alteração" = (exp(summary(modelo)$coefficients$cond[,1]) - 1),
                      "I.C." = paste0("[",round(exp(summary(modelo)$coefficients$cond[,1]-1.96*summary(modelo)$coefficients$cond[,2]),3),"; ",
                                      round(exp(summary(modelo)$coefficients$cond[,1]+1.96*summary(modelo)$coefficients$cond[,2]),3),"]"),
                      "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients$cond[,1]-1.96*summary(modelo)$coefficients$cond[,2])-1)*100,2),"%; ",
                                                  round((exp(summary(modelo)$coefficients$cond[,1]+1.96*summary(modelo)$coefficients$cond[,2])-1)*100,2),"%]"),
                      "Valor-p" = round(summary(modelo)$coefficients$cond[,4],4))
  return(Tabela)
}

####=============================
#### Carregando o banco de dados 
####=============================
dados = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 09-04-2024.xlsx", sheet = 1)},
                 error = function(e) {read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 09-04-2024.xlsx", sheet = 1)})

#dados1 = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 06-05-2024.xlsx", sheet = 1)

####============================
#### Distribuição das variáveis
####============================
variaveis = c("idade_media","civil_uniaoest_casado_prop","anos_de_estudo","plano_saude_nao_prop",
              "IMC_media","IMC_cat_baixo_prop","IMC_cat_excesso_prop","IMC_i_media",
              "IMC_i_cat_baixo_prop","IMC_i_cat_excesso_prop","hortareg_prop","frutareg_prop",
              "flvreg_prop","hortadia_media","sofrutadia_media","frutadia_media",
              "flvdia_media","flvreco_prop","refritl5_prop","feijao5_prop",
              "hart_prop","diab_prop","POP","ANEMIA","DEFICIENCIAS_NUTRICIONAIS",
              "DIABETES_MELITUS","HIPERTENSAO","SomaICSAP","TaxaICSAP","Nota",
              "População","Leitos.SUS","Taxa.Cob.Planos.Priv","Cobertura.ESF","IVS","IDHM","Gini",
              "Q1_media","Q2_media","Q3_media","Q4_media","Q5_media","Q6_media","Q7_media","Q8_media","Q9_prop","Q10_media",
              "Q11_media","Q12_media","Q13_media","Q14_prop","Q15_prop","Q16_prop","Q17_prop","Q18_prop","Q19_media","Q20_prop",
              "Q21_prop","Q22_prop","Q23_prop","Q24_prop","Q25_prop","Q26_prop","Q27_prop","Q28_prop","Q29_prop","Q30_prop")

#cruadia_cat_prop, cozidadia_cat_prop e sucodia_media são compostos por 0's ou 1's
Tabela1 = 
  do.call(rbind,dados %>% select(idade_media,civil_uniaoest_casado_prop,anos_de_estudo,plano_saude_nao_prop,IMC_media,
                                 IMC_cat_baixo_prop,IMC_cat_excesso_prop,IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,
                                 hortareg_prop,frutareg_prop,flvreg_prop,hortadia_media,
                                 sofrutadia_media,frutadia_media,flvdia_media,flvreco_prop,refritl5_prop,feijao5_prop,
                                 hart_prop,diab_prop,POP,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO,
                                 SomaICSAP,TaxaICSAP,Nota,População,Leitos.SUS,Taxa.Cob.Planos.Priv,Cobertura.ESF,IVS,IDHM,Gini,
                                 Q1_media,Q2_media,Q3_media,Q4_media,Q5_media,Q6_media,Q7_media,Q8_media,Q9_prop,Q10_media,
                                 Q11_media,Q12_media,Q13_media,Q14_prop,Q15_prop,Q16_prop,Q17_prop,Q18_prop,Q19_media,Q20_prop,
                                 Q21_prop,Q22_prop,Q23_prop,Q24_prop,Q25_prop,Q26_prop,Q27_prop,Q28_prop,Q29_prop,Q30_prop) %>% 
            map(TesteDeNormalidade))
#write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx')

#Tabela1 = lapply(variaveis, function(var) {TesteDeNormalidadeGrupos(dados[[var]], dados$ano)}) %>% bind_rows()
#HomogeneidadeVariancias(dados$plano_saude_nao_prop, dados$ano)
#HomogeneidadeVariancias(dados$IMC_i_media, dados$ano)

####==================
#### Dados por região
####==================
#20 a 79 anos
dados_20a79 = dados %>% filter(idade_cat == '20 a 59 anos' | idade_cat == '60 a 79 anos')
dados_20a79_inc = dados_20a79 %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina' | cidade_nome == 'Macapá')
dados_20a79_comp = dados_20a79 %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina' & cidade_nome != 'Macapá')

dados_CO = dados_20a79 %>% filter(Região == 'Centro-Oeste')

dados_ND = dados_20a79 %>% filter(Região == 'Nordeste')
dados_ND_inc = dados_20a79 %>% filter(Região == 'Nordeste') %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina')
dados_ND_comp = dados_20a79 %>% filter(Região == 'Nordeste') %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina')

dados_NT = dados_20a79 %>% filter(Região == 'Norte')
dados_NT_inc = dados_20a79 %>% filter(Região == 'Norte') %>% filter(cidade_nome == 'Macapá')
dados_NT_comp = dados_20a79 %>% filter(Região == 'Norte') %>% filter(cidade_nome != 'Macapá')

dados_Sud = dados_20a79 %>% filter(Região == 'Sudeste')
dados_Sul = dados_20a79 %>% filter(Região == 'Sul')

#0 a 19 anos
dados_0a19 = dados %>% filter(idade_cat == '0 a 4 anos' | idade_cat == '5 a 19 anos')

dados_CO_0a19 = dados_0a19 %>% filter(Região == 'Centro-Oeste')
dados_ND_0a19 = dados_0a19 %>% filter(Região == 'Nordeste')
dados_ND_inc_0a19 = dados_0a19 %>% filter(Região == 'Nordeste') %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina')
dados_ND_comp_0a19 = dados_0a19 %>% filter(Região == 'Nordeste') %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina')

dados_NT_0a19 = dados_0a19 %>% filter(Região == 'Norte')
dados_NT_inc_0a19 = dados_0a19 %>% filter(Região == 'Norte') %>% filter(cidade_nome == 'Macapá')
dados_NT_comp_0a19 = dados_0a19 %>% filter(Região == 'Norte') %>% filter(cidade_nome != 'Macapá')

dados_Sud_0a19 = dados_0a19 %>% filter(Região == 'Sudeste')
dados_Sul_0a19 = dados_0a19 %>% filter(Região == 'Sul')

#80 anos ou mais
dados_80mais = dados %>% filter(idade_cat == '80 anos ou mais')
dados_CO_80mais = dados_80mais %>% filter(Região == 'Centro-Oeste')
dados_ND_80mais = dados_80mais %>% filter(Região == 'Nordeste')
dados_ND_inc_80mais = dados_80mais %>% filter(Região == 'Nordeste') %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina')
dados_ND_comp_80mais = dados_80mais %>% filter(Região == 'Nordeste') %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina')

dados_NT_80mais = dados_80mais %>% filter(Região == 'Norte')
dados_NT_inc_80mais = dados_80mais %>% filter(Região == 'Norte') %>% filter(cidade_nome == 'Macapá')
dados_NT_comp_80mais = dados_80mais %>% filter(Região == 'Norte') %>% filter(cidade_nome != 'Macapá')

dados_Sud_80mais = dados_80mais %>% filter(Região == 'Sudeste')
dados_Sul_80mais = dados_80mais %>% filter(Região == 'Sul')

####=========
#### Modelos
####=========
#Respostas: IMC_i_cat_excesso_prop, flvreg_prop, flvreco_prop, refritl5_prop, feijao5_prop, hart_prop, diab_prop e TaxaICSAP
#Explicativas: sexo, faixa etária, anos de estudo, plano de saúde, Nota (IVS, IDH, Gini)

####========================
#### IMC_i_cat_excesso_prop
####========================
uni_imc1 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + sexo, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,sexo,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc2 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + idade_cat, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,idade_cat,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc3 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc4 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc5 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + Nota, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,Nota,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc6 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + IVS, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,IVS,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc7 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + IDHM, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,IDHM,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc8 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + Gini, 
                   data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,Gini,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))

Tabela31 = rbind(TabelaGLMMBeta(uni_imc1),TabelaGLMMBeta(uni_imc2),
                 TabelaGLMMBeta(uni_imc3),TabelaGLMMBeta(uni_imc4),
                 TabelaGLMMBeta(uni_imc5),#TabelaGLMMBeta(uni_imc6),
                 TabelaGLMMBeta(uni_imc7)#,TabelaGLMMBeta(uni_imc8)
                 )
#write.xlsx(Tabela31 %>% as.data.frame(), 'Tabela 31.1.xlsx', rowNames = F)

#Sem interação
multi_semint_imc1 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + sexo + idade_cat + #anos_de_estudo + 
                             plano_saude_nao_prop + Nota + 
                             IDHM, data = dados_20a79 %>% 
                             select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                           family = beta_family(link = "logit"))
summary(multi_semint_imc1)
#write.xlsx(TabelaGLMMBeta(multi_semint_imc1) %>% as.data.frame(), 'Tabela 31.2.xlsx', rowNames = F)

#Com interação
multi_imc1 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
summary(multi_imc1)

#sexoMasculino:IDHM
multi_imc2 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc2)

#anos_de_estudo*plano_saude_nao_prop
multi_imc3 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                       anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc3)


#plano_saude_nao_prop*IDHM
multi_imc4 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                       anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       plano_saude_nao_prop*Nota +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc4)

#idade_cat*Nota
multi_imc5 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                       anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       plano_saude_nao_prop*Nota +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc5)

#plano_saude_nao_prop*Nota
multi_imc6 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                       anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc6)

#anos_de_estudo*IDHM
multi_imc7 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                       anos_de_estudo*Nota +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc7)

#sexo*plano_saude_nao_prop
multi_imc8 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                       anos_de_estudo*Nota +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc8)

#sexo*Nota
multi_imc9 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + 
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                       anos_de_estudo*Nota +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados_20a79 %>% 
                       select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc9)

#anos_de_estudo*Nota
multi_imc10 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + 
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                        Nota*IDHM + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_imc10)
#write.xlsx(TabelaGLMMBeta(multi_imc10) %>% as.data.frame(), 'Tabela 31.3.xlsx', rowNames = F)

####=============
#### flvreg_prop
####=============
uni_flvreg1 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + sexo, 
                      data = dados_20a79 %>% select(flvreg_prop,sexo,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg2 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + idade_cat, 
                      data = dados_20a79 %>% select(flvreg_prop,idade_cat,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg3 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                      data = dados_20a79 %>% select(flvreg_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg4 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                      data = dados_20a79 %>% select(flvreg_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg5 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + Nota, 
                      data = dados_20a79 %>% select(flvreg_prop,Nota,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg6 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + IVS, 
                      data = dados_20a79 %>% select(flvreg_prop,IVS,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg7 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + IDHM, 
                      data = dados_20a79 %>% select(flvreg_prop,IDHM,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg8 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + Gini, 
                      data = dados_20a79 %>% select(flvreg_prop,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))

Tabela32 = rbind(TabelaGLMMBeta(uni_flvreg1),TabelaGLMMBeta(uni_flvreg2),
                 TabelaGLMMBeta(uni_flvreg3),TabelaGLMMBeta(uni_flvreg4),
                 TabelaGLMMBeta(uni_flvreg5),#TabelaGLMMBeta(uni_flvreg6),
                 TabelaGLMMBeta(uni_flvreg7)#,TabelaGLMMBeta(uni_flvreg8)
)
#write.xlsx(Tabela32 %>% as.data.frame(), 'Tabela 32.1.xlsx', rowNames = F)

#Sem interação
multi_semint_flvreg1 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + sexo + idade_cat + anos_de_estudo + 
                                 plano_saude_nao_prop + #Nota + 
                                 IDHM, data = dados_20a79 %>% 
                                 select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                               family = beta_family(link = "logit"))
summary(multi_semint_flvreg1)
#write.xlsx(TabelaGLMMBeta(multi_semint_flvreg1) %>% as.data.frame(), 'Tabela 32.2.xlsx', rowNames = F)

#Com interação
multi_flvreg1 = glmmTMB(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg1)

#sexo*Nota
multi_flvreg2 = glmmTMB(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg2)

#idade_cat*IDHM
multi_flvreg3 = glmmTMB(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg3)

#sexo*plano_saude_nao_prop
multi_flvreg4 = glmmTMB(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg4)

#plano_saude_nao_prop*IDHM
multi_flvreg5 = glmmTMB(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg5)

#anos_de_estudo*Nota
multi_flvreg6 = glmmTMB(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg6)

#sexo*idade_cat
multi_flvreg7 = glmmTMB(flvreg_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg7)

#sexo*anos_de_estudo
multi_flvreg8 = glmmTMB(flvreg_prop ~ sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg8)

#plano_saude_nao_prop*Nota
multi_flvreg9 = glmmTMB(flvreg_prop ~ sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg9)
#write.xlsx(TabelaGLMMBeta(multi_flvreg9) %>% as.data.frame(), 'Tabela 32.3.xlsx', rowNames = F)

####==============
#### flvreco_prop
####==============
uni_flvreco1 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + sexo, 
                       data = dados_20a79 %>% select(flvreco_prop,sexo,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco2 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + idade_cat, 
                       data = dados_20a79 %>% select(flvreco_prop,idade_cat,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco3 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                       data = dados_20a79 %>% select(flvreco_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco4 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                       data = dados_20a79 %>% select(flvreco_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco5 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + Nota, 
                       data = dados_20a79 %>% select(flvreco_prop,Nota,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco6 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + IVS, 
                       data = dados_20a79 %>% select(flvreco_prop,IVS,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco7 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + IDHM, 
                       data = dados_20a79 %>% select(flvreco_prop,IDHM,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco8 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + Gini, 
                       data = dados_20a79 %>% select(flvreco_prop,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))

Tabela33 = rbind(TabelaGLMMBeta(uni_flvreco1),TabelaGLMMBeta(uni_flvreco2),
                 TabelaGLMMBeta(uni_flvreco3),TabelaGLMMBeta(uni_flvreco4),
                 TabelaGLMMBeta(uni_flvreco5),#TabelaGLMMBeta(uni_flvreco6),
                 TabelaGLMMBeta(uni_flvreco7)#,TabelaGLMMBeta(uni_flvreco8)
)
#write.xlsx(Tabela33 %>% as.data.frame(), 'Tabela 33.1.xlsx', rowNames = F)

#Sem interação: Nota e anos_de_estudo
multi_semint_flvreco1 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + sexo + idade_cat + anos_de_estudo + 
                                 plano_saude_nao_prop + #Nota + 
                                 IDHM, data = dados_20a79 %>% 
                                 select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                               family = beta_family(link = "logit"))
summary(multi_semint_flvreco1)
#write.xlsx(TabelaGLMMBeta(multi_semint_flvreco1) %>% as.data.frame(), 'Tabela 33.2.xlsx', rowNames = F)

#Com interação
multi_flvreco1 = glmmTMB(flvreco_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreco1)

#sexo*idade_cat
multi_flvreco2 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco2)

#idade_cat*IDHM
multi_flvreco3 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco3)

#plano_saude_nao_prop*IDHM
multi_flvreco4 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco4)

#anos_de_estudo*Nota
multi_flvreco5 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco5)

#sexo*Nota
multi_flvreco6 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco6)

#sexo*plano_saude_nao_prop
multi_flvreco7 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco7)

#Nota*IDHM
multi_flvreco8 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco8)

#plano_saude_nao_prop*Nota
multi_flvreco9 = glmmTMB(flvreco_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco9)
#write.xlsx(TabelaGLMMBeta(multi_flvreco9) %>% as.data.frame(), 'Tabela 33.3.xlsx', rowNames = F)

####===============
#### refritl5_prop
####===============
uni_refritl51 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + sexo, 
                        data = dados_20a79 %>% select(refritl5_prop,sexo,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl52 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + idade_cat, 
                        data = dados_20a79 %>% select(refritl5_prop,idade_cat,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl53 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                        data = dados_20a79 %>% select(refritl5_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl54 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                        data = dados_20a79 %>% select(refritl5_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl55 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + Nota, 
                        data = dados_20a79 %>% select(refritl5_prop,Nota,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl56 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + IVS, 
                        data = dados_20a79 %>% select(refritl5_prop,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl57 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + IDHM, 
                        data = dados_20a79 %>% select(refritl5_prop,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl58 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + Gini, 
                        data = dados_20a79 %>% select(refritl5_prop,Gini,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))

Tabela34 = rbind(TabelaGLMMBeta(uni_refritl51),TabelaGLMMBeta(uni_refritl52),
                 TabelaGLMMBeta(uni_refritl53),TabelaGLMMBeta(uni_refritl54),
                 TabelaGLMMBeta(uni_refritl55),#TabelaGLMMBeta(uni_refritl56),
                 TabelaGLMMBeta(uni_refritl57)#,TabelaGLMMBeta(uni_refritl58)
)
#write.xlsx(Tabela34 %>% as.data.frame(), 'Tabela 34.1.xlsx', rowNames = F)

#Sem interação
multi_semint_refritl51 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + sexo + idade_cat + anos_de_estudo + 
                                   #plano_saude_nao_prop + 
                                   Nota + 
                                   IDHM, data = dados_20a79 %>% 
                                   select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                                 family = beta_family(link = "logit"))
summary(multi_semint_refritl51)
#write.xlsx(TabelaGLMMBeta(multi_semint_refritl51) %>% as.data.frame(), 'Tabela 34.2.xlsx', rowNames = F)

#Com interação
multi_refritl51 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_refritl51)

#plano_saude_nao_prop*IDHM
multi_refritl52 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl52)

#anos_de_estudo*IDHM
multi_refritl53 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                            plano_saude_nao_prop*Nota +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl53)

#anos_de_estudo*Nota
multi_refritl54 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            plano_saude_nao_prop*Nota +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl54)

#sexo*Nota
multi_refritl55 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            plano_saude_nao_prop*Nota +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl55)

#plano_saude_nao_prop*Nota
multi_refritl56 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl56)

#anos_de_estudo*plano_saude_nao_prop
multi_refritl57 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl57)

#sexo*IDHM
multi_refritl58 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl58)

#sexo*anos_de_estudo
multi_refritl59 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl59)

#idade_cat*Nota
multi_refritl510 = glmmTMB(refritl5_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*IDHM +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl510)
#write.xlsx(TabelaGLMMBeta(multi_refritl510) %>% as.data.frame(), 'Tabela 34.3.xlsx', rowNames = F)

####==============
#### feijao5_prop
####==============
uni_feijao1 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + sexo, 
                      data = dados_20a79 %>% select(feijao5_prop,sexo,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao2 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + idade_cat, 
                      data = dados_20a79 %>% select(feijao5_prop,idade_cat,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao3 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                      data = dados_20a79 %>% select(feijao5_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao4 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                      data = dados_20a79 %>% select(feijao5_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao5 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + Nota, 
                      data = dados_20a79 %>% select(feijao5_prop,Nota,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao6 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + IVS, 
                      data = dados_20a79 %>% select(feijao5_prop,IVS,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao7 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + IDHM, 
                      data = dados_20a79 %>% select(feijao5_prop,IDHM,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_feijao8 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + Gini, 
                      data = dados_20a79 %>% select(feijao5_prop,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))

Tabela35 = rbind(TabelaGLMMBeta(uni_feijao1),TabelaGLMMBeta(uni_feijao2),
                 TabelaGLMMBeta(uni_feijao3),TabelaGLMMBeta(uni_feijao4),
                 TabelaGLMMBeta(uni_feijao5),TabelaGLMMBeta(uni_feijao6)
                 #TabelaGLMMBeta(uni_feijao7),TabelaGLMMBeta(uni_feijao8)
)
#write.xlsx(Tabela35 %>% as.data.frame(), 'Tabela 35.1.xlsx', rowNames = F)

#Sem interação
multi_semint_feijao1 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + sexo + idade_cat + anos_de_estudo + 
                                 #plano_saude_nao_prop + 
                                 #Nota + 
                                 IVS, data = dados_20a79 %>% 
                                 select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                               family = beta_family(link = "logit"))
summary(multi_semint_feijao1)
#write.xlsx(TabelaGLMMBeta(multi_semint_feijao1) %>% as.data.frame(), 'Tabela 35.2.xlsx', rowNames = F)

#Com interação
multi_feijao1 = glmmTMB(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao1)

#plano_saude_nao_prop*IVS
multi_feijao2 = glmmTMB(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota +
                          Nota*IVS + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao2)

#idade_cat*IVS
multi_feijao3 = glmmTMB(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota +
                          Nota*IVS + 
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao3)


#Nota*IVS
multi_feijao4 = glmmTMB(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota +
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao4)

#sexo*idade_cat
multi_feijao5 = glmmTMB(feijao5_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota +
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao5)

#sexo*Nota
multi_feijao6 = glmmTMB(feijao5_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota +
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao6)

#sexo*IVS
multi_feijao7 = glmmTMB(feijao5_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota +
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao7)

#anos_de_estudo*IVS
multi_feijao8 = glmmTMB(feijao5_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + IVS +
                          plano_saude_nao_prop*Nota +
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao8)

#idade_cat*plano_saude_nao_prop
multi_feijao9 = glmmTMB(feijao5_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                          idade_cat*anos_de_estudo + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + IVS +
                          plano_saude_nao_prop*Nota +
                          (1 | cidade_nome), data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_feijao9)
#write.xlsx(TabelaGLMMBeta(multi_feijao9) %>% as.data.frame(), 'Tabela 35.3.xlsx', rowNames = F)

####======
#### hart
####======
uni_hart1 = glmmTMB(hart_prop ~ (1 | cidade_nome) + sexo, 
                    data = dados_20a79 %>% select(hart_prop,sexo,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart2 = glmmTMB(hart_prop ~ (1 | cidade_nome) + idade_cat, 
                    data = dados_20a79 %>% select(hart_prop,idade_cat,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart3 = glmmTMB(hart_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                    data = dados_20a79 %>% select(hart_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart4 = glmmTMB(hart_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                    data = dados_20a79 %>% select(hart_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart5 = glmmTMB(hart_prop ~ (1 | cidade_nome) + Nota, 
                    data = dados_20a79 %>% select(hart_prop,Nota,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart6 = glmmTMB(hart_prop ~ (1 | cidade_nome) + IVS, 
                    data = dados_20a79 %>% select(hart_prop,IVS,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart7 = glmmTMB(hart_prop ~ (1 | cidade_nome) + IDHM, 
                    data = dados_20a79 %>% select(hart_prop,IDHM,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_hart8 = glmmTMB(hart_prop ~ (1 | cidade_nome) + Gini, 
                    data = dados_20a79 %>% select(hart_prop,Gini,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))

Tabela36 = rbind(TabelaGLMMBeta(uni_hart1),TabelaGLMMBeta(uni_hart2),
                 TabelaGLMMBeta(uni_hart3),TabelaGLMMBeta(uni_hart4),
                 TabelaGLMMBeta(uni_hart5),#TabelaGLMMBeta(uni_hart6),
                 #TabelaGLMMBeta(uni_hart7),
                 TabelaGLMMBeta(uni_hart8)
)
#write.xlsx(Tabela36 %>% as.data.frame(), 'Tabela 36.1.xlsx', rowNames = F)

#Sem interação
multi_semint_hart1 = glmmTMB(hart_prop ~ (1 | cidade_nome) + sexo + idade_cat + anos_de_estudo + 
                               plano_saude_nao_prop, 
                               #Nota + Gini, 
                             data = dados_20a79 %>% 
                               select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                             family = beta_family(link = "logit"))
summary(multi_semint_hart1)
#write.xlsx(TabelaGLMMBeta(multi_semint_hart1) %>% as.data.frame(), 'Tabela 36.2.xlsx', rowNames = F)

#Com interação
multi_hart1 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart1)

#anos_de_estudo*Gini
multi_hart2 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart2)

#sexo*Nota
multi_hart3 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart3)

#idade_cat*anos_de_estudo
multi_hart4 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart4)

#idade_cat*Nota
multi_hart5 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart5)

#plano_saude_nao_prop*Gini
multi_hart6 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart6)

#Nota*Gini
multi_hart7 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota +
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart7)

#sexo*plano_saude_nao_prop
multi_hart8 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota +
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart8)

#sexo*anos_de_estudo
multi_hart9 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota +
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_hart9)

#anos_de_estudo*Nota
multi_hart10 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*Gini +
                         idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                         anos_de_estudo*plano_saude_nao_prop +
                         plano_saude_nao_prop*Nota +
                         (1 | cidade_nome), data = dados_20a79 %>% 
                         select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
summary(multi_hart10)

#plano_saude_nao_prop*Nota
multi_hart11 = glmmTMB(hart_prop ~ sexo*idade_cat + sexo*Gini +
                         idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                         anos_de_estudo*plano_saude_nao_prop +
                         Nota +
                         (1 | cidade_nome), data = dados_20a79 %>% 
                         select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
summary(multi_hart11)
#write.xlsx(TabelaGLMMBeta(multi_hart11) %>% as.data.frame(), 'Tabela 36.3.xlsx', rowNames = F)

####======
#### diab
####======
uni_diab1 = glmmTMB(diab_prop ~ (1 | cidade_nome) + sexo, 
                    data = dados_20a79 %>% select(diab_prop,sexo,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab2 = glmmTMB(diab_prop ~ (1 | cidade_nome) + idade_cat, 
                    data = dados_20a79 %>% select(diab_prop,idade_cat,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab3 = glmmTMB(diab_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                    data = dados_20a79 %>% select(diab_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab4 = glmmTMB(diab_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                    data = dados_20a79 %>% select(diab_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab5 = glmmTMB(diab_prop ~ (1 | cidade_nome) + Nota, 
                    data = dados_20a79 %>% select(diab_prop,Nota,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab6 = glmmTMB(diab_prop ~ (1 | cidade_nome) + IVS, 
                    data = dados_20a79 %>% select(diab_prop,IVS,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab7 = glmmTMB(diab_prop ~ (1 | cidade_nome) + IDHM, 
                    data = dados_20a79 %>% select(diab_prop,IDHM,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))
uni_diab8 = glmmTMB(diab_prop ~ (1 | cidade_nome) + Gini, 
                    data = dados_20a79 %>% select(diab_prop,Gini,cidade_nome) %>% na.omit(), 
                    family = beta_family(link = "logit"))

Tabela37 = rbind(TabelaGLMMBeta(uni_diab1),TabelaGLMMBeta(uni_diab2),
                 TabelaGLMMBeta(uni_diab3),TabelaGLMMBeta(uni_diab4),
                 TabelaGLMMBeta(uni_diab5),#TabelaGLMMBeta(uni_diab6),
                 #TabelaGLMMBeta(uni_diab7),
                 TabelaGLMMBeta(uni_diab8)
)
#write.xlsx(Tabela37 %>% as.data.frame(), 'Tabela 37.1.xlsx', rowNames = F)

#Sem interação
multi_semint_diab1 = glmmTMB(diab_prop ~ (1 | cidade_nome) + #sexo + 
                               idade_cat + #anos_de_estudo + 
                               #plano_saude_nao_prop +
                               #Nota + 
                               Gini, 
                             data = dados_20a79 %>% 
                               select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                             family = beta_family(link = "logit"))
summary(multi_semint_diab1)
#write.xlsx(TabelaGLMMBeta(multi_semint_diab1) %>% as.data.frame(), 'Tabela 37.2.xlsx', rowNames = F)

#Com interação
multi_diab1 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab1)

#sexo*Gini
multi_diab2 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab2)

#plano_saude_nao_prop*Gini
multi_diab3 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + 
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab3)

#idade_cat*Nota
multi_diab4 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + 
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab4)

#sexo*plano_saude_nao_prop
multi_diab5 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + 
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab5)

#idade_cat*anos_de_estudo
multi_diab6 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + 
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab6)

#idade_cat*Gini
multi_diab7 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + 
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab7)

#anos_de_estudo*Gini
multi_diab8 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota + 
                        Nota*Gini + 
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab8)

#Nota*Gini
multi_diab9 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                        plano_saude_nao_prop*Nota + Gini +
                        (1 | cidade_nome), data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
summary(multi_diab9)

#plano_saude_nao_prop*Nota
multi_diab10 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*Nota +
                         idade_cat*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                         Gini +
                         (1 | cidade_nome), data = dados_20a79 %>% 
                         select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
summary(multi_diab10)

#sexo*Nota
multi_diab11 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo +
                         idade_cat*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                         Gini +
                         (1 | cidade_nome), data = dados_20a79 %>% 
                         select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
summary(multi_diab11)

#anos_de_estudo*Nota
multi_diab12 = glmmTMB(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo +
                         idade_cat*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop +
                         Gini +
                         (1 | cidade_nome), data = dados_20a79 %>% 
                         select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
summary(multi_diab12)
#write.xlsx(TabelaGLMMBeta(multi_diab11) %>% as.data.frame(), 'Tabela 37.3.xlsx', rowNames = F)

####===========
#### TaxaICSAP
####===========
uni_TaxaICSAP1 = glmer(TaxaICSAP ~ (1 | cidade_nome) + sexo,
                       data = dados_20a79 %>% select(TaxaICSAP,sexo,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP2 = glmer(TaxaICSAP ~ (1 | cidade_nome) + idade_cat,
                       data = dados_20a79 %>% select(TaxaICSAP,idade_cat,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP3 = glmer(TaxaICSAP ~ (1 | cidade_nome) + anos_de_estudo,
                       data = dados_20a79 %>% select(TaxaICSAP,anos_de_estudo,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP4 = glmer(TaxaICSAP ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                       data = dados_20a79 %>% select(TaxaICSAP,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP5 = glmer(TaxaICSAP ~ (1 | cidade_nome) + Nota,
                       data = dados_20a79 %>% select(TaxaICSAP,Nota,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP6 = glmer(TaxaICSAP ~ (1 | cidade_nome) + IVS, 
                       data = dados_20a79 %>% select(TaxaICSAP,IVS,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP7 = glmer(TaxaICSAP ~ (1 | cidade_nome) + IDHM,
                       data = dados_20a79 %>% select(TaxaICSAP,IDHM,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

uni_TaxaICSAP8 = glmer(TaxaICSAP ~ (1 | cidade_nome) + Gini,
                       data = dados_20a79 %>% select(TaxaICSAP,Gini,cidade_nome) %>% na.omit(), 
                       family = Gamma(link = "log"))

cor.test(dados_20a79$TaxaICSAP,dados_20a79$IVS)
cor.test(dados_20a79$TaxaICSAP,dados_20a79$IDHM)
cor.test(dados_20a79$TaxaICSAP,dados_20a79$Gini)

Tabela38.1 = rbind(TabelaGEEGama(uni_TaxaICSAP1),TabelaGEEGama(uni_TaxaICSAP2),
                   TabelaGEEGama(uni_TaxaICSAP3),TabelaGEEGama(uni_TaxaICSAP4),
                   TabelaGEEGama(uni_TaxaICSAP5),#TabelaGEEGama(uni_TaxaICSAP6)
                   TabelaGEEGama(uni_TaxaICSAP7)#TabelaGEEGama(uni_TaxaICSAP8)
)
#write.xlsx(Tabela38.1 %>% as.data.frame(), 'Tabela 38.1.xlsx', rowNames = F)

#Sem interação: Nota
multi_semint_TaxaICSAP1 = glmer(TaxaICSAP ~ sexo + 
                                  idade_cat + #anos_de_estudo + 
                                  #plano_saude_nao_prop + 
                                  Nota + 
                                  IDHM + 
                                  (1 | cidade_nome), 
                                data = dados_20a79 %>% 
                                  select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                                family = Gamma(link = "log"))
summary(multi_semint_TaxaICSAP1)
write.xlsx(TabelaGEEGama(multi_semint_TaxaICSAP1) %>% as.data.frame(), 'Tabela 38.2.xlsx', rowNames = F)

#Com interação
multi_TaxaICSAP1 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP1)

#idade_cat*plano_saude_nao_prop
multi_TaxaICSAP2 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP2)

#anos_de_estudo*IDHM
multi_TaxaICSAP3 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP3)

#sexo*IDHM
multi_TaxaICSAP4 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP4)

#plano_saude_nao_prop*IDHM
multi_TaxaICSAP5 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP5)

#anos_de_estudo*plano_saude_nao_prop
multi_TaxaICSAP6 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP6)

#sexo*Nota
multi_TaxaICSAP7 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP7)

#sexo*plano_saude_nao_prop
multi_TaxaICSAP8 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP8)

#idade_cat*anos_de_estudo
multi_TaxaICSAP9 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo +
                           idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*Nota +
                           plano_saude_nao_prop*Nota +
                           Nota*IDHM +
                           (1 | cidade_nome), data = dados_20a79 %>% 
                           select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP9)

#idade_cat*Nota
multi_TaxaICSAP10 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo +
                            idade_cat*IDHM +
                            anos_de_estudo*Nota +
                            plano_saude_nao_prop*Nota +
                            Nota*IDHM +
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP10)

#Nota*IDHM
multi_TaxaICSAP11 = glmer(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo +
                            idade_cat*IDHM +
                            anos_de_estudo*Nota +
                            plano_saude_nao_prop*Nota +
                            (1 | cidade_nome), data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = Gamma(link = "log"), nAGQ=0)
summary(multi_TaxaICSAP11)
#write.xlsx(TabelaGEEGama(multi_TaxaICSAP5) %>% as.data.frame(), 'Tabela 38.3.xlsx', rowNames = F)

#hnp::hnp(multi_TaxaICSAP5, resid.type='pearson')

####===============================================
#### Imputação dos dados ausentes na nota e feijão
####===============================================
#Nota:
#Maceió, Macapá, Teresina e São Luís em 2010
# dados_Nota_NA = dados_20a79 %>% select(Nota,cidade,cidade_nome,Estado,ano,sexo,idade_cat,IVS,IDHM,Gini,Cobertura.ESF,porte_mun,
#                                        Ciclo,Taxa.Cob.Planos.Priv,Leitos.SUS,TaxaICSAP,
#                                        Q1_media,Q2_media,Q3_media,Q4_media,Q5_media,Q6_media,Q7_media,Q8_media,Q9_prop,Q10_media,
#                                        Q11_media,Q12_media,Q13_media,Q14_prop,Q15_prop,Q16_prop,Q17_prop,Q18_prop,Q19_media,Q20_prop)
# dados_Nota_NA_para_imp = dados_Nota_NA %>% select(-c(cidade,Estado,sexo,idade_cat,IVS,IDHM,Gini,Cobertura.ESF,porte_mun,
#                                                      Ciclo,Taxa.Cob.Planos.Priv,Leitos.SUS))
# dados_Nota_semNA = mice::complete(mice::mice(dados_Nota_NA_para_imp, method = "rf", m = 5))
# 
# dados_Nota_semNA$cidade = dados_20a79$cidade
# dados_Nota_semNA$Estado = dados_20a79$Estado
# dados_Nota_semNA$sexo = dados_20a79$sexo
# dados_Nota_semNA$idade_cat = dados_20a79$idade_cat
# dados_Nota_semNA$IVS = dados_20a79$IVS
# dados_Nota_semNA$IDHM = dados_20a79$IDHM
# dados_Nota_semNA$Gini = dados_20a79$Gini
# dados_Nota_semNA$Cobertura.ESF = dados_20a79$Cobertura.ESF
# dados_Nota_semNA$porte_mun = dados_20a79$porte_mun
# dados_Nota_semNA$Ciclo = dados_20a79$Ciclo
# dados_Nota_semNA$Taxa.Cob.Planos.Priv = dados_20a79$Taxa.Cob.Planos.Priv
# dados_Nota_semNA$Leitos.SUS = dados_20a79$Leitos.SUS
# dados_Nota_semNA$TaxaICSAP = dados_20a79$TaxaICSAP
# 
# valor_mais_repetido_alagoas = dados_Nota_semNA %>% filter(cidade_nome == 'Maceió') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>% 
#   count(Nota) %>% slice(which.max(n)) %>% pull(Nota)
# valor_mais_repetido_amapa = dados_Nota_semNA %>% filter(cidade_nome == 'Macapá') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>% 
#   count(Nota) %>% slice(which.max(n)) %>% pull(Nota)
# valor_mais_repetido_maranhao = dados_Nota_semNA %>% filter(cidade_nome == 'São Luís') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>%  
#   count(Nota) %>% slice(which.max(n)) %>% pull(Nota)
# valor_mais_repetido_piaui = dados_Nota_semNA %>% filter(cidade_nome == 'Teresina') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>%  
#   count(Nota) %>% slice(which.max(n)) %>% pull(Nota)
# 
# dados_alagoas_semNA = dados_Nota_semNA %>% filter(cidade_nome == 'Maceió') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>% 
#   mutate(Nota = if_else(Nota != valor_mais_repetido_alagoas, valor_mais_repetido_alagoas, Nota))
# dados_amapa_semNA = dados_Nota_semNA %>% filter(cidade_nome == 'Macapá') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>% 
#   mutate(Nota = if_else(Nota != valor_mais_repetido_amapa, valor_mais_repetido_amapa, Nota))
# dados_maranhao_semNA = dados_Nota_semNA %>% filter(cidade_nome == 'São Luís') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>% 
#   mutate(Nota = if_else(Nota != valor_mais_repetido_maranhao, valor_mais_repetido_maranhao, Nota))
# dados_piaui_semNA = dados_Nota_semNA %>% filter(cidade_nome == 'Teresina') %>% filter(ano == 2010 | ano == 2011 | ano == 2012) %>% 
#   mutate(Nota = if_else(Nota != valor_mais_repetido_piaui, valor_mais_repetido_piaui, Nota))
# 
# dados_Nota_NA_imp = rbind(dados_alagoas_semNA,dados_amapa_semNA,dados_maranhao_semNA,dados_piaui_semNA,
#                           dados_Nota_semNA %>% filter(cidade_nome == 'Maceió') %>% filter(ano != 2010 & ano != 2011 & ano != 2012),
#                           dados_Nota_semNA %>% filter(cidade_nome == 'Macapá') %>% filter(ano != 2010 & ano != 2011 & ano != 2012),
#                           dados_Nota_semNA %>% filter(cidade_nome == 'São Luís') %>% filter(ano != 2010 & ano != 2011 & ano != 2012),
#                           dados_Nota_semNA %>% filter(cidade_nome == 'Teresina') %>% filter(ano != 2010 & ano != 2011 & ano != 2012),
#                           dados_Nota_NA %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'Macapá' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina'))
# 
# #Feijão: ano de 2018
# dados_feijao_NA = dados_20a79 %>% select(cidade,ano,
#                                          IMC_media,IMC_cat_baixo_prop,IMC_cat_excesso_prop,IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,
#                                          frutareg_prop,flvreg_prop,cruadia_cat_prop,cozidadia_cat_prop,hortadia_media,sucodia_media,sofrutadia_media,
#                                          frutadia_media,flvdia_media,flvreco_prop,refritl5_prop,feijao5_prop,hart_prop,diab_prop,
#                                          sexo,idade_cat,anos_de_estudo,civil_uniaoest_casado_prop,plano_saude_nao_prop)
# dados_feijao_semNA = mice::complete(mice::mice(dados_feijao_NA, method = "rf", m = 5))
# 
# dados_completos = left_join(dados_Nota_NA_imp, dados_feijao_semNA, by=c('ano'='ano','cidade'='cidade','sexo'='sexo','idade_cat'='idade_cat'))

#dados_completos %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'Macapá' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina')
#write.xlsx(dados_completos %>% as.data.frame(), 'Dados com imputações.xlsx')

####========
#### Extras
####========
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
            media_feijao5_prop = mean(feijao5_prop), media_hart_prop = mean(hart_prop),
            media_diab_prop = mean(diab_prop), media_ANEMIA = mean(ANEMIA),
            media_DEFICIENCIAS_NUTRICIONAIS = mean(DEFICIENCIAS_NUTRICIONAIS),
            media_DIABETES_MELITUS = mean(DIABETES_MELITUS),
            media_HIPERTENSAO = mean(HIPERTENSAO), media_SomaICSAP = mean(SomaICSAP),
            media_TaxaICSAP = mean(TaxaICSAP), media_Nota = mean(Nota),
            soma_População = sum(População), soma_Leitos.SUS = sum(Leitos.SUS),
            media_Taxa.Cob.Planos.Priv = mean(Taxa.Cob.Planos.Priv),
            media_Cobertura.ESF = mean(Cobertura.ESF),
            media_IVS = mean(IVS), media_IDHM = mean(IDHM), media_Gini = mean(Gini))

#write.xlsx(dados_sumarizados %>% as.data.frame(), 'Dados sumarizados por região, sexo, faixa etária e ano.xlsx', rowNames = F)