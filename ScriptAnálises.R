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
if(!require(lme4)){ install.packages("lme4"); require(lme4)}
if(!require(glmmTMB)){ install.packages("glmmTMB"); require(glmmTMB)}

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
dados = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 23-05-2024.xlsx", sheet = 1)},
                           error = function(e) {read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 23-05-2024.xlsx", sheet = 1)})

####============================
#### Distribuição das variáveis
####============================
variaveis = c("sexo_M_prop","idade_60a79_prop","civil_uniaoest_casado_prop","anos_de_estudo","plano_saude_nao_prop",
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
  do.call(rbind,dados %>% select(sexo_M_prop,idade_60a79_prop,civil_uniaoest_casado_prop,anos_de_estudo,plano_saude_nao_prop,IMC_media,
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

Tabela1 = lapply(variaveis, function(var) {TesteDeNormalidadeGrupos(dados[[var]], dados$ano)}) %>% bind_rows()
#HomogeneidadeVariancias(dados$plano_saude_nao_prop, dados$ano)
#HomogeneidadeVariancias(dados$IMC_i_media, dados$ano)

####==================
#### Dados por região
####==================
dados_inc = dados %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina' | cidade_nome == 'Macapá')
dados_comp = dados %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina' & cidade_nome != 'Macapá')

dados_CO = dados %>% filter(Região == 'Centro-Oeste')

dados_ND = dados %>% filter(Região == 'Nordeste')
dados_ND_inc = dados %>% filter(Região == 'Nordeste') %>% filter(cidade_nome == 'Maceió' | cidade_nome == 'São Luís' | cidade_nome == 'Teresina')
dados_ND_comp = dados %>% filter(Região == 'Nordeste') %>% filter(cidade_nome != 'Maceió' & cidade_nome != 'São Luís' & cidade_nome != 'Teresina')

dados_NT = dados %>% filter(Região == 'Norte')
dados_NT_inc = dados %>% filter(Região == 'Norte') %>% filter(cidade_nome == 'Macapá')
dados_NT_comp = dados %>% filter(Região == 'Norte') %>% filter(cidade_nome != 'Macapá')

dados_Sud = dados %>% filter(Região == 'Sudeste')
dados_Sul = dados %>% filter(Região == 'Sul')

####=========
#### Modelos
####=========
#Respostas: IMC_i_cat_excesso_prop, flvreg_prop, flvreco_prop, refritl5_prop, feijao5_prop, hart_prop, diab_prop e TaxaICSAP
#Explicativas: sexo, faixa etária, anos de estudo, plano de saúde, Nota (IVS, IDH, Gini)

####========================
#### IMC_i_cat_excesso_prop
####========================
hist(dados$IMC_i_cat_excesso_prop)
ks.test(dados$IMC_i_cat_excesso_prop, "pnorm", mean(dados$IMC_i_cat_excesso_prop, na.rm = T), sd(dados$IMC_i_cat_excesso_prop, na.rm = T))

fit_IMC = lm(IMC_i_cat_excesso_prop ~ factor(ano), data=dados)
res_IMC0 = subset(fit_IMC$residuals, dados$ano == 2010)
res_IMC1 = subset(fit_IMC$residuals, dados$ano == 2011)
res_IMC2 = subset(fit_IMC$residuals, dados$ano == 2012)
res_IMC3 = subset(fit_IMC$residuals, dados$ano == 2013)
res_IMC4 = subset(fit_IMC$residuals, dados$ano == 2014)
res_IMC5 = subset(fit_IMC$residuals, dados$ano == 2015)
res_IMC6 = subset(fit_IMC$residuals, dados$ano == 2016)
res_IMC7 = subset(fit_IMC$residuals, dados$ano == 2017)
res_IMC8 = subset(fit_IMC$residuals, dados$ano == 2018)
res_IMC9 = subset(fit_IMC$residuals, dados$ano == 2019)

res_IMC = data.frame(res_IMC0, res_IMC1, res_IMC2, res_IMC3, res_IMC4, res_IMC5, res_IMC6, res_IMC7, res_IMC8, res_IMC9)
cor(res_IMC)
cov(res_IMC)

uni_imc1 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + sexo_M_prop, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,sexo_M_prop,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc2 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + idade_60a79_prop, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,idade_60a79_prop,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc3 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc4 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc5 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + Nota, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,Nota,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc6 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + IVS, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,IVS,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc7 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + IDHM, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,IDHM,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))
uni_imc8 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + Gini, 
                   data = dados %>% select(IMC_i_cat_excesso_prop,Gini,cidade_nome) %>% na.omit(), 
                   family = beta_family(link = "logit"))

cor.test(dados$IMC_i_cat_excesso_prop,dados$IVS)
cor.test(dados$IMC_i_cat_excesso_prop,dados$IDHM)
cor.test(dados$IMC_i_cat_excesso_prop,dados$Gini)

Tabela1.1 = rbind(TabelaGLMMBeta(uni_imc1),TabelaGLMMBeta(uni_imc2),
                 TabelaGLMMBeta(uni_imc3),TabelaGLMMBeta(uni_imc4),
                 TabelaGLMMBeta(uni_imc5),#TabelaGLMMBeta(uni_imc6),
                 TabelaGLMMBeta(uni_imc7)#,TabelaGLMMBeta(uni_imc8)
)
#write.xlsx(Tabela1.1 %>% as.data.frame(), 'Tabela 1.1.xlsx', rowNames = F)

#Sem interação
multi_semint_imc1 = glmmTMB(IMC_i_cat_excesso_prop ~ (1 | cidade_nome) + sexo_M_prop + idade_60a79_prop + anos_de_estudo + 
                              #plano_saude_nao_prop + Nota + 
                              IDHM, data = dados %>% 
                              select(IMC_i_cat_excesso_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                            family = beta_family(link = "logit"))
summary(multi_semint_imc1)
#write.xlsx(TabelaGLMMBeta(multi_semint_imc1) %>% as.data.frame(), 'Tabela 1.2.xlsx', rowNames = F)

#Com interação
multi_imc1 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados %>% 
                       select(IMC_i_cat_excesso_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc1)

#anos_de_estudo:Nota
#sexo_M_prop*Nota
#idade_60a79_prop*Nota
#anos_de_estudo*IDHM
#sexo_M_prop*plano_saude_nao_prop
#idade_60a79_prop*anos_de_estudo
#sexo_M_prop*IDHM
#sexo_M_prop*anos_de_estudo
#idade_60a79_prop*IDHM
#anos_de_estudo*plano_saude_nao_prop
#plano_saude_nao_prop*Nota
#sexo_M_prop*idade_60a79_prop
#plano_saude_nao_prop*IDHM
multi_imc2 = glmmTMB(IMC_i_cat_excesso_prop ~ sexo_M_prop +
                       idade_60a79_prop*plano_saude_nao_prop +
                       anos_de_estudo +
                       Nota*IDHM + 
                       (1 | cidade_nome), data = dados %>% 
                       select(IMC_i_cat_excesso_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                     family = beta_family(link = "logit"))
summary(multi_imc2)
#write.xlsx(TabelaGLMMBeta(multi_imc2) %>% as.data.frame(), 'Tabela 1.3.xlsx', rowNames = F)

####=============
#### flvreg_prop
####=============
####======
#### Beta
####======
hist(dados$flvreg_prop)
ks.test(dados$flvreg_prop, "pnorm", mean(dados$flvreg_prop, na.rm = T), sd(dados$flvreg_prop, na.rm = T))

fit_flvreg = lm(flvreg_prop ~ factor(ano), data=dados)
res_flvreg0 = subset(fit_flvreg$residuals, dados$ano == 2010)
res_flvreg1 = subset(fit_flvreg$residuals, dados$ano == 2011)
res_flvreg2 = subset(fit_flvreg$residuals, dados$ano == 2012)
res_flvreg3 = subset(fit_flvreg$residuals, dados$ano == 2013)
res_flvreg4 = subset(fit_flvreg$residuals, dados$ano == 2014)
res_flvreg5 = subset(fit_flvreg$residuals, dados$ano == 2015)
res_flvreg6 = subset(fit_flvreg$residuals, dados$ano == 2016)
res_flvreg7 = subset(fit_flvreg$residuals, dados$ano == 2017)
res_flvreg8 = subset(fit_flvreg$residuals, dados$ano == 2018)
res_flvreg9 = subset(fit_flvreg$residuals, dados$ano == 2019)

res_flvreg = data.frame(res_flvreg0, res_flvreg1, res_flvreg2, res_flvreg3, res_flvreg4, res_flvreg5, res_flvreg6, res_flvreg7, res_flvreg8, res_flvreg9)
cor(res_flvreg)
cov(res_flvreg)

uni_flvreg1 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + sexo_M_prop, 
                      data = dados %>% select(flvreg_prop,sexo_M_prop,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg2 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + idade_60a79_prop, 
                      data = dados %>% select(flvreg_prop,idade_60a79_prop,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg3 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                      data = dados %>% select(flvreg_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg4 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                      data = dados %>% select(flvreg_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg5 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + Nota, 
                      data = dados %>% select(flvreg_prop,Nota,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg6 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + IVS, 
                      data = dados %>% select(flvreg_prop,IVS,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg7 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + IDHM, 
                      data = dados %>% select(flvreg_prop,IDHM,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))
uni_flvreg8 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + Gini, 
                      data = dados %>% select(flvreg_prop,Gini,cidade_nome) %>% na.omit(), 
                      family = beta_family(link = "logit"))

cor.test(dados$flvreg_prop,dados$IVS)
cor.test(dados$flvreg_prop,dados$IDHM)
cor.test(dados$flvreg_prop,dados$Gini)

Tabela2.1 = rbind(TabelaGLMMBeta(uni_flvreg1),TabelaGLMMBeta(uni_flvreg2),
                  TabelaGLMMBeta(uni_flvreg3),TabelaGLMMBeta(uni_flvreg4),
                  TabelaGLMMBeta(uni_flvreg5),#TabelaGLMMBeta(uni_flvreg6),
                  TabelaGLMMBeta(uni_flvreg7)#,TabelaGLMMBeta(uni_flvreg8)
)
#write.xlsx(Tabela2.1 %>% as.data.frame(), 'Tabela 2.1.xlsx', rowNames = F)

#Sem interação
multi_semint_flvreg1 = glmmTMB(flvreg_prop ~ (1 | cidade_nome) + #sexo_M_prop + #idade_60a79_prop + 
                                 anos_de_estudo + 
                              plano_saude_nao_prop, #Nota, #IDHM, 
                                data = dados %>% 
                              select(flvreg_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                            family = beta_family(link = "logit"))
summary(multi_semint_flvreg1)
#write.xlsx(TabelaGLMMBeta(multi_semint_flvreg1) %>% as.data.frame(), 'Tabela 2.2.xlsx', rowNames = F)

#Com interação
multi_flvreg1 = glmmTMB(flvreg_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                          idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM + 
                          (1 | cidade_nome), data = dados %>% 
                          select(flvreg_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg1)

#plano_saude_nao_prop*Nota
#sexo_M_prop*Nota
#Nota*IDHM
#anos_de_estudo*plano_saude_nao_prop
#sexo_M_prop*plano_saude_nao_prop
#anos_de_estudo*IDHM
#idade_60a79_prop*Nota
#idade_60a79_prop*anos_de_estudo
#plano_saude_nao_prop*IDHM
#sexo_M_prop*IDHM
#sexo_M_prop*idade_60a79_prop
#idade_60a79_prop*plano_saude_nao_prop
#idade_60a79_prop*IDHM
#idade_60a79_prop
#IDHM
multi_flvreg2 = glmmTMB(flvreg_prop ~ sexo_M_prop*anos_de_estudo +
                          plano_saude_nao_prop + 
                          anos_de_estudo*Nota +
                          (1 | cidade_nome), data = dados %>% 
                          select(flvreg_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
summary(multi_flvreg2)
#write.xlsx(TabelaGLMMBeta(multi_flvreg2) %>% as.data.frame(), 'Tabela 2.3.xlsx', rowNames = F)

####========
#### Normal
####========
uni_n_flvreg1 = geeglm(flvreg_prop ~ sexo_M_prop, id = cidade, 
                       data = dados %>% select(flvreg_prop,sexo_M_prop,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg2 = geeglm(flvreg_prop ~ idade_60a79_prop, id = cidade, 
                       data = dados %>% select(flvreg_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg3 = geeglm(flvreg_prop ~ anos_de_estudo, id = cidade, 
                       data = dados %>% select(flvreg_prop,anos_de_estudo,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg4 = geeglm(flvreg_prop ~ plano_saude_nao_prop, id = cidade, 
                       data = dados %>% select(flvreg_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg5 = geeglm(flvreg_prop ~ Nota, id = cidade, 
                       data = dados %>% select(flvreg_prop,Nota,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg6 = geeglm(flvreg_prop ~ IVS, id = cidade, 
                       data = dados %>% select(flvreg_prop,IVS,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg7 = geeglm(flvreg_prop ~ IDHM, id = cidade, 
                       data = dados %>% select(flvreg_prop,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreg8 = geeglm(flvreg_prop ~ Gini, id = cidade, 
                       data = dados %>% select(flvreg_prop,Gini,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados$flvreg_prop,dados$IVS)
cor.test(dados$flvreg_prop,dados$IDHM)
cor.test(dados$flvreg_prop,dados$Gini)

Tabela2.4 = rbind(TabelaGEENormal(uni_n_flvreg1),TabelaGEENormal(uni_n_flvreg2),
                  TabelaGEENormal(uni_n_flvreg3),TabelaGEENormal(uni_n_flvreg4),
                  TabelaGEENormal(uni_n_flvreg5),#TabelaGEENormal(uni_n_flvreg6),
                  TabelaGEENormal(uni_n_flvreg7)#TabelaGEENormal(uni_n_flvreg8)
)
#write.xlsx(Tabela2.4 %>% as.data.frame(), 'Tabela 2.4.xlsx', rowNames = F)

#Sem interação
multi_n_semint_flvreg1 = geeglm(flvreg_prop ~ #sexo_M_prop + #idade_60a79_prop + 
                                  #anos_de_estudo + 
                                  plano_saude_nao_prop + #Nota + 
                                  IDHM, 
                                id = cidade, data = dados %>% 
                                  select(flvreg_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_semint_flvreg1)
#write.xlsx(TabelaGEENormal(multi_n_semint_flvreg1) %>% as.data.frame(), 'Tabela 2.5.xlsx', rowNames = F)

hist(multi_n_semint_flvreg1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_n_semint_flvreg1$residuals),max(multi_n_semint_flvreg1$residuals), length.out = 100),
      dnorm(seq(min(multi_n_semint_flvreg1$residuals),max(multi_n_semint_flvreg1$residuals), length.out = 100),
            mean=mean(multi_n_semint_flvreg1$residuals),sd=sd(multi_n_semint_flvreg1$residuals)))

ks.test(multi_n_semint_flvreg1$residuals, "pnorm", mean(multi_n_semint_flvreg1$residuals, na.rm = T), 
        sd(multi_n_semint_flvreg1$residuals, na.rm = T))
qqnorm(multi_n_semint_flvreg1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_n_semint_flvreg1$residuals)

#Com interação
multi_n_flvreg1 = geeglm(flvreg_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                           idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados %>% 
                           select(flvreg_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg1)

#idade_60a79_prop*Nota
#sexo_M_prop*plano_saude_nao_prop
#anos_de_estudo*IDHM
#idade_60a79_prop*anos_de_estudo
#sexo_M_prop*IDHM
#anos_de_estudo*plano_saude_nao_prop
#idade_60a79_prop*IDHM
#plano_saude_nao_prop*Nota

multi_n_flvreg2 = geeglm(flvreg_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*Nota +
                           idade_60a79_prop*plano_saude_nao_prop +
                           anos_de_estudo*Nota +
                           plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados %>% 
                           select(flvreg_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_flvreg2)
#write.xlsx(TabelaGEENormal(multi_n_flvreg2) %>% as.data.frame(), 'Tabela 2.6.xlsx', rowNames = F)

hist(multi_flvreg2$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_flvreg2$residuals),max(multi_flvreg2$residuals), length.out = 100),
      dnorm(seq(min(multi_flvreg2$residuals),max(multi_flvreg2$residuals), length.out = 100),
            mean=mean(multi_flvreg2$residuals),sd=sd(multi_flvreg2$residuals)))

qqnorm(multi_flvreg2$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_flvreg2$residuals)

ks.test(multi_flvreg2$residuals, "pnorm", mean(multi_flvreg2$residuals, na.rm = T), sd(multi_flvreg2$residuals, na.rm = T))

####==============
#### flvreco_prop
####==============
####======
#### Beta
####======
hist(dados$flvreco_prop)
ks.test(dados$flvreco_prop, "pnorm", mean(dados$flvreco_prop, na.rm = T), sd(dados$flvreco_prop, na.rm = T))

fit_flvreco = lm(flvreco_prop ~ factor(ano), data=dados)
res_flvreco0 = subset(fit_flvreco$residuals, dados$ano == 2010)
res_flvreco1 = subset(fit_flvreco$residuals, dados$ano == 2011)
res_flvreco2 = subset(fit_flvreco$residuals, dados$ano == 2012)
res_flvreco3 = subset(fit_flvreco$residuals, dados$ano == 2013)
res_flvreco4 = subset(fit_flvreco$residuals, dados$ano == 2014)
res_flvreco5 = subset(fit_flvreco$residuals, dados$ano == 2015)
res_flvreco6 = subset(fit_flvreco$residuals, dados$ano == 2016)
res_flvreco7 = subset(fit_flvreco$residuals, dados$ano == 2017)
res_flvreco8 = subset(fit_flvreco$residuals, dados$ano == 2018)
res_flvreco9 = subset(fit_flvreco$residuals, dados$ano == 2019)

res_flvreco = data.frame(res_flvreco0, res_flvreco1, res_flvreco2, res_flvreco3, res_flvreco4, res_flvreco5, res_flvreco6, res_flvreco7, res_flvreco8, res_flvreco9)
cor(res_flvreco)
cov(res_flvreco)

uni_flvreco1 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + sexo_M_prop, 
                       data = dados %>% select(flvreco_prop,sexo_M_prop,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco2 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + idade_60a79_prop, 
                       data = dados %>% select(flvreco_prop,idade_60a79_prop,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco3 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                       data = dados %>% select(flvreco_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco4 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                       data = dados %>% select(flvreco_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco5 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + Nota, 
                       data = dados %>% select(flvreco_prop,Nota,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco6 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + IVS, 
                       data = dados %>% select(flvreco_prop,IVS,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco7 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + IDHM, 
                       data = dados %>% select(flvreco_prop,IDHM,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))
uni_flvreco8 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + Gini, 
                       data = dados %>% select(flvreco_prop,Gini,cidade_nome) %>% na.omit(), 
                       family = beta_family(link = "logit"))

cor.test(dados$flvreco_prop,dados$IVS)
cor.test(dados$flvreco_prop,dados$IDHM)
cor.test(dados$flvreco_prop,dados$Gini)

Tabela3.1 = rbind(TabelaGLMMBeta(uni_flvreco1),TabelaGLMMBeta(uni_flvreco2),
                  TabelaGLMMBeta(uni_flvreco3),TabelaGLMMBeta(uni_flvreco4),
                  TabelaGLMMBeta(uni_flvreco5),#TabelaGLMMBeta(uni_flvreco6),
                  TabelaGLMMBeta(uni_flvreco7)#,TabelaGLMMBeta(uni_flvreco8)
)
#write.xlsx(Tabela3.1 %>% as.data.frame(), 'Tabela 3.1.xlsx', rowNames = F)

#Sem interação
multi_semint_flvreco1 = glmmTMB(flvreco_prop ~ (1 | cidade_nome) + #sexo_M_prop + idade_60a79_prop + 
                                  anos_de_estudo + 
                                  plano_saude_nao_prop,# + Nota + IDHM, 
                                data = dados %>% 
                                  select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                                family = beta_family(link = "logit"))
summary(multi_semint_flvreco1)
#write.xlsx(TabelaGLMMBeta(multi_semint_flvreco1) %>% as.data.frame(), 'Tabela 3.2.xlsx', rowNames = F)

#Com interação
multi_flvreco1 = glmmTMB(flvreco_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                           idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados %>% 
                           select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco1)

#anos_de_estudo*IDHM
#Nota*IDHM
#sexo_M_prop*IDHM
#anos_de_estudo*plano_saude_nao_prop
#idade_60a79_prop*plano_saude_nao_prop
#sexo_M_prop*Nota
#idade_60a79_prop*Nota
#plano_saude_nao_prop*IDHM
#plano_saude_nao_prop*Nota
#idade_60a79_prop*IDHM
#idade_60a79_prop*anos_de_estudo
#sexo_M_prop*plano_saude_nao_prop
#sexo_M_prop*idade_60a79_prop
#idade_60a79_prop
#IDHM
#sexo_M_prop*anos_de_estudo
#sexo_M_prop
multi_flvreco2 = glmmTMB(flvreco_prop ~  plano_saude_nao_prop +
                           anos_de_estudo*Nota +
                           (1 | cidade_nome), data = dados %>% 
                           select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_flvreco2)
#write.xlsx(TabelaGLMMBeta(multi_flvreco2) %>% as.data.frame(), 'Tabela 3.3.xlsx', rowNames = F)

####========
#### Normal
####========
uni_n_flvreco1 = geeglm(flvreco_prop ~ sexo_M_prop, id = cidade, 
                        data = dados %>% select(flvreco_prop,sexo_M_prop,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco2 = geeglm(flvreco_prop ~ idade_60a79_prop, id = cidade, 
                        data = dados %>% select(flvreco_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco3 = geeglm(flvreco_prop ~ anos_de_estudo, id = cidade, 
                        data = dados %>% select(flvreco_prop,anos_de_estudo,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco4 = geeglm(flvreco_prop ~ plano_saude_nao_prop, id = cidade, 
                        data = dados %>% select(flvreco_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco5 = geeglm(flvreco_prop ~ Nota, id = cidade, 
                        data = dados %>% select(flvreco_prop,Nota,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco6 = geeglm(flvreco_prop ~ IVS, id = cidade, 
                        data = dados %>% select(flvreco_prop,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco7 = geeglm(flvreco_prop ~ IDHM, id = cidade, 
                        data = dados %>% select(flvreco_prop,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_flvreco8 = geeglm(flvreco_prop ~ Gini, id = cidade, 
                        data = dados %>% select(flvreco_prop,Gini,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados$flvreco_prop,dados$IVS)
cor.test(dados$flvreco_prop,dados$IDHM)
cor.test(dados$flvreco_prop,dados$Gini)

Tabela3.4 = rbind(TabelaGEENormal(uni_n_flvreco1),TabelaGEENormal(uni_n_flvreco2),
                  TabelaGEENormal(uni_n_flvreco3),TabelaGEENormal(uni_n_flvreco4),
                  TabelaGEENormal(uni_n_flvreco5),#TabelaGEENormal(uni_n_flvreco6),
                  TabelaGEENormal(uni_n_flvreco7)#TabelaGEENormal(uni_n_flvreco8)
)
#write.xlsx(Tabela3.4 %>% as.data.frame(), 'Tabela 3.4.xlsx', rowNames = F)

#Sem interação
multi_n_semint_flvreco1 = geeglm(flvreco_prop ~ #sexo_M_prop + idade_60a79_prop + 
                                   anos_de_estudo + 
                                   plano_saude_nao_prop + #Nota + 
                                   IDHM, 
                                 id = cidade, data = dados %>% 
                                   select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                 family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_semint_flvreco1)
#write.xlsx(TabelaGEENormal(multi_n_semint_flvreco1) %>% as.data.frame(), 'Tabela 3.5.xlsx', rowNames = F)

hist(multi_n_semint_flvreco1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_n_semint_flvreco1$residuals),max(multi_n_semint_flvreco1$residuals), length.out = 100),
      dnorm(seq(min(multi_n_semint_flvreco1$residuals),max(multi_n_semint_flvreco1$residuals), length.out = 100),
            mean=mean(multi_n_semint_flvreco1$residuals),sd=sd(multi_n_semint_flvreco1$residuals)))

ks.test(multi_n_semint_flvreco1$residuals, "pnorm", mean(multi_n_semint_flvreco1$residuals, na.rm = T), 
        sd(multi_n_semint_flvreco1$residuals, na.rm = T))
qqnorm(multi_n_semint_flvreco1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_n_semint_flvreco1$residuals)

#Com interação
multi_n_flvreco1 = geeglm(flvreco_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados %>% 
                            select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_flvreco1)

#sexo_M_prop*anos_de_estudo
#sexo_M_prop*IDHM
#sexo_M_prop*plano_saude_nao_prop
#idade_60a79_prop*Nota
#idade_60a79_prop*plano_saude_nao_prop
#plano_saude_nao_prop*IDHM
#anos_de_estudo*plano_saude_nao_prop
#idade_60a79_prop*IDHM
#anos_de_estudo*IDHM
#sexo_M_prop*idade_60a79_prop

multi_n_flvreco2 = geeglm(flvreco_prop ~ sexo_M_prop*Nota + 
                            idade_60a79_prop*anos_de_estudo +
                            anos_de_estudo*Nota +
                            plano_saude_nao_prop*Nota +
                            Nota*IDHM, 
                          id = cidade, data = dados %>% 
                            select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_flvreco2)
#write.xlsx(TabelaGEENormal(multi_n_flvreco2) %>% as.data.frame(), 'Tabela 3.6.xlsx', rowNames = F)

hist(multi_flvreco2$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_flvreco2$residuals),max(multi_flvreco2$residuals), length.out = 100),
      dnorm(seq(min(multi_flvreco2$residuals),max(multi_flvreco2$residuals), length.out = 100),
            mean=mean(multi_flvreco2$residuals),sd=sd(multi_flvreco2$residuals)))

qqnorm(multi_flvreco2$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_flvreco2$residuals)

ks.test(multi_flvreco2$residuals, "pnorm", mean(multi_flvreco2$residuals, na.rm = T), sd(multi_flvreco2$residuals, na.rm = T))

####=======
#### Gamma
####=======
unig_flvreco1 = geeglm(flvreco_prop ~ sexo_M_prop, id = cidade, 
                       data = dados %>% select(flvreco_prop,sexo_M_prop,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco2 = geeglm(flvreco_prop ~ idade_60a79_prop, id = cidade, 
                       data = dados %>% select(flvreco_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco3 = geeglm(flvreco_prop ~ anos_de_estudo, id = cidade, 
                       data = dados %>% select(flvreco_prop,anos_de_estudo,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco4 = geeglm(flvreco_prop ~ plano_saude_nao_prop, id = cidade, 
                       data = dados %>% select(flvreco_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco5 = geeglm(flvreco_prop ~ Nota, id = cidade, 
                       data = dados %>% select(flvreco_prop,Nota,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco6 = geeglm(flvreco_prop ~ IVS, id = cidade, 
                       data = dados %>% select(flvreco_prop,IVS,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco7 = geeglm(flvreco_prop ~ IDHM, id = cidade, 
                       data = dados %>% select(flvreco_prop,IDHM,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_flvreco8 = geeglm(flvreco_prop ~ Gini, id = cidade, 
                       data = dados %>% select(flvreco_prop,Gini,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

Tabela3.7 = rbind(TabelaGEEGama(unig_flvreco1),TabelaGEEGama(unig_flvreco2),
                   TabelaGEEGama(unig_flvreco3),TabelaGEEGama(unig_flvreco4),
                   TabelaGEEGama(unig_flvreco5),#TabelaGEEGama(unig_flvreco6),
                   TabelaGEEGama(unig_flvreco7)#TabelaGEEGama(unig_flvreco8)
)
#write.xlsx(Tabela3.7 %>% as.data.frame(), 'Tabela 3.7.xlsx', rowNames = F)

#Sem interação
multig_semint_flvreco1 = geeglm(flvreco_prop ~ #sexo_M_prop + idade_60a79_prop + 
                                  anos_de_estudo + 
                                  plano_saude_nao_prop + #Nota + 
                                  IDHM, 
                                id = cidade, data = dados %>% 
                                  select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_flvreco1)
#write.xlsx(TabelaGEEGama(multig_semint_flvreco1) %>% as.data.frame(), 'Tabela 3.8.xlsx', rowNames = F)

#Com interação
multig_flvreco1 = geeglm(flvreco_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                           idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados %>% 
                           select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_flvreco1)

#sexo_M_prop*anos_de_estudo
#sexo_M_prop*plano_saude_nao_prop
#sexo_M_prop*IDHM
#idade_60a79_prop*Nota
#idade_60a79_prop*IDHM
#plano_saude_nao_prop*IDHM
#anos_de_estudo*plano_saude_nao_prop
#anos_de_estudo*IDHM
#idade_60a79_prop*plano_saude_nao_prop
#sexo_M_prop*idade_60a79_prop
#plano_saude_nao_prop*Nota
multig_flvreco2 = geeglm(flvreco_prop ~ sexo_M_prop*Nota +
                           idade_60a79_prop*anos_de_estudo +
                           anos_de_estudo*Nota +
                           plano_saude_nao_prop +
                           Nota*IDHM, 
                         id = cidade, data = dados %>% 
                           select(flvreco_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_flvreco2)
#write.xlsx(TabelaGEEGama(multig_flvreco2) %>% as.data.frame(), 'Tabela 3.9.xlsx', rowNames = F)

####===============
#### refritl5_prop
####===============
####======
#### Beta
####======
hist(dados$refritl5_prop)
ks.test(dados$refritl5_prop, "pnorm", mean(dados$refritl5_prop, na.rm = T), sd(dados$refritl5_prop, na.rm = T))

fit_refritl5 = lm(refritl5_prop ~ factor(ano), data=dados)
res_refritl50 = subset(fit_refritl5$residuals, dados$ano == 2010)
res_refritl51 = subset(fit_refritl5$residuals, dados$ano == 2011)
res_refritl52 = subset(fit_refritl5$residuals, dados$ano == 2012)
res_refritl53 = subset(fit_refritl5$residuals, dados$ano == 2013)
res_refritl54 = subset(fit_refritl5$residuals, dados$ano == 2014)
res_refritl55 = subset(fit_refritl5$residuals, dados$ano == 2015)
res_refritl56 = subset(fit_refritl5$residuals, dados$ano == 2016)
res_refritl57 = subset(fit_refritl5$residuals, dados$ano == 2017)
res_refritl58 = subset(fit_refritl5$residuals, dados$ano == 2018)
res_refritl59 = subset(fit_refritl5$residuals, dados$ano == 2019)

res_refritl5 = data.frame(res_refritl50, res_refritl51, res_refritl52, res_refritl53, res_refritl54, res_refritl55, res_refritl56, res_refritl57, res_refritl58, res_refritl59)
cor(res_refritl5)
cov(res_refritl5)

uni_refritl51 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + sexo_M_prop, 
                        data = dados %>% select(refritl5_prop,sexo_M_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl52 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + idade_60a79_prop, 
                        data = dados %>% select(refritl5_prop,idade_60a79_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl53 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                        data = dados %>% select(refritl5_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl54 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                        data = dados %>% select(refritl5_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl55 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + Nota, 
                        data = dados %>% select(refritl5_prop,Nota,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl56 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + IVS, 
                        data = dados %>% select(refritl5_prop,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl57 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + IDHM, 
                        data = dados %>% select(refritl5_prop,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_refritl58 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + Gini, 
                        data = dados %>% select(refritl5_prop,Gini,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))

cor.test(dados$refritl5_prop,dados$IVS)
cor.test(dados$refritl5_prop,dados$IDHM)
cor.test(dados$refritl5_prop,dados$Gini)

Tabela4.1 = rbind(TabelaGLMMBeta(uni_refritl51),TabelaGLMMBeta(uni_refritl52),
                  TabelaGLMMBeta(uni_refritl53),TabelaGLMMBeta(uni_refritl54),
                  TabelaGLMMBeta(uni_refritl55),#TabelaGLMMBeta(uni_refritl56),
                  #TabelaGLMMBeta(uni_refritl57),
                  TabelaGLMMBeta(uni_refritl58)
)
write.xlsx(Tabela4.1 %>% as.data.frame(), 'Tabela 4.1.xlsx', rowNames = F)

#Sem interação
multi_semint_refritl51 = glmmTMB(refritl5_prop ~ (1 | cidade_nome) + #sexo_M_prop + 
                                   idade_60a79_prop + 
                                   anos_de_estudo + 
                                   #plano_saude_nao_prop + 
                                   Nota + Gini, 
                                 data = dados %>% 
                                   select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                                 family = beta_family(link = "logit"))
summary(multi_semint_refritl51)
write.xlsx(TabelaGLMMBeta(multi_semint_refritl51) %>% as.data.frame(), 'Tabela 4.2.xlsx', rowNames = F)

#Com interação
multi_refritl51 = glmmTMB(refritl5_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                            Nota*Gini + 
                            (1 | cidade_nome), data = dados %>% 
                            select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl51)

#anos_de_estudo*Nota
#sexo_M_prop*Gini
#idade_60a79_prop*plano_saude_nao_prop
#anos_de_estudo*plano_saude_nao_prop 
#idade_60a79_prop*anos_de_estudo
#Nota*Gini
#idade_60a79_prop*Gini
#anos_de_estudo*Gini
#sexo_M_prop*idade_60a79_prop
#sexo_M_prop*plano_saude_nao_prop
#plano_saude_nao_prop*Gini
#idade_60a79_prop*Nota
#sexo_M_prop*Nota
multi_refritl52 = glmmTMB(refritl5_prop ~ sexo_M_prop*anos_de_estudo +
                            idade_60a79_prop +
                            plano_saude_nao_prop*Nota + Gini +
                            (1 | cidade_nome), data = dados %>% 
                            select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_refritl52)
write.xlsx(TabelaGLMMBeta(multi_refritl52) %>% as.data.frame(), 'Tabela 4.3.xlsx', rowNames = F)

####========
#### Normal
####========
uni_n_refritl51 = geeglm(refritl5_prop ~ sexo_M_prop, id = cidade, 
                         data = dados %>% select(refritl5_prop,sexo_M_prop,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl52 = geeglm(refritl5_prop ~ idade_60a79_prop, id = cidade, 
                         data = dados %>% select(refritl5_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl53 = geeglm(refritl5_prop ~ anos_de_estudo, id = cidade, 
                         data = dados %>% select(refritl5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl54 = geeglm(refritl5_prop ~ plano_saude_nao_prop, id = cidade, 
                         data = dados %>% select(refritl5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl55 = geeglm(refritl5_prop ~ Nota, id = cidade, 
                         data = dados %>% select(refritl5_prop,Nota,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl56 = geeglm(refritl5_prop ~ IVS, id = cidade, 
                         data = dados %>% select(refritl5_prop,IVS,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl57 = geeglm(refritl5_prop ~ Gini, id = cidade, 
                         data = dados %>% select(refritl5_prop,Gini,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_refritl58 = geeglm(refritl5_prop ~ Gini, id = cidade, 
                         data = dados %>% select(refritl5_prop,Gini,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados$refritl5_prop,dados$IVS)
cor.test(dados$refritl5_prop,dados$Gini)
cor.test(dados$refritl5_prop,dados$Gini)

Tabela4.4 = rbind(TabelaGEENormal(uni_n_refritl51),TabelaGEENormal(uni_n_refritl52),
                  TabelaGEENormal(uni_n_refritl53),TabelaGEENormal(uni_n_refritl54),
                  TabelaGEENormal(uni_n_refritl55),#TabelaGEENormal(uni_n_refritl56),
                  TabelaGEENormal(uni_n_refritl57)#TabelaGEENormal(uni_n_refritl58)
)
#write.xlsx(Tabela4.4 %>% as.data.frame(), 'Tabela 4.4.xlsx', rowNames = F)

#Sem interação
multi_n_semint_refritl51 = geeglm(refritl5_prop ~ #sexo_M_prop + idade_60a79_prop + 
                                    anos_de_estudo + 
                                    plano_saude_nao_prop + #Nota + 
                                    Gini, 
                                  id = cidade, data = dados %>% 
                                    select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                                  family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_semint_refritl51)
#write.xlsx(TabelaGEENormal(multi_n_semint_refritl51) %>% as.data.frame(), 'Tabela 4.5.xlsx', rowNames = F)

hist(multi_n_semint_refritl51$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_n_semint_refritl51$residuals),max(multi_n_semint_refritl51$residuals), length.out = 100),
      dnorm(seq(min(multi_n_semint_refritl51$residuals),max(multi_n_semint_refritl51$residuals), length.out = 100),
            mean=mean(multi_n_semint_refritl51$residuals),sd=sd(multi_n_semint_refritl51$residuals)))

ks.test(multi_n_semint_refritl51$residuals, "pnorm", mean(multi_n_semint_refritl51$residuals, na.rm = T), 
        sd(multi_n_semint_refritl51$residuals, na.rm = T))
qqnorm(multi_n_semint_refritl51$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_n_semint_refritl51$residuals)

#Com interação
multi_n_refritl51 = geeglm(refritl5_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                             idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                             anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                             plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                             Nota*Gini, 
                           id = cidade, data = dados %>% 
                             select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                           family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_refritl51)

#sexo_M_prop*anos_de_estudo
#sexo_M_prop*Gini
#sexo_M_prop*plano_saude_nao_prop
#idade_60a79_prop*Nota
#idade_60a79_prop*plano_saude_nao_prop
#plano_saude_nao_prop*Gini
#anos_de_estudo*plano_saude_nao_prop
#idade_60a79_prop*Gini
#anos_de_estudo*Gini
#sexo_M_prop*idade_60a79_prop

multi_n_refritl52 = geeglm(refritl5_prop ~ sexo_M_prop*Nota + 
                             idade_60a79_prop*anos_de_estudo +
                             anos_de_estudo*Nota +
                             plano_saude_nao_prop*Nota +
                             Nota*Gini, 
                           id = cidade, data = dados %>% 
                             select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                           family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_refritl52)
#write.xlsx(TabelaGEENormal(multi_n_refritl52) %>% as.data.frame(), 'Tabela 4.6.xlsx', rowNames = F)

hist(multi_refritl52$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_refritl52$residuals),max(multi_refritl52$residuals), length.out = 100),
      dnorm(seq(min(multi_refritl52$residuals),max(multi_refritl52$residuals), length.out = 100),
            mean=mean(multi_refritl52$residuals),sd=sd(multi_refritl52$residuals)))

qqnorm(multi_refritl52$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_refritl52$residuals)

ks.test(multi_refritl52$residuals, "pnorm", mean(multi_refritl52$residuals, na.rm = T), sd(multi_refritl52$residuals, na.rm = T))

####=======
#### Gamma
####=======
unig_refritl51 = geeglm(refritl5_prop ~ sexo_M_prop, id = cidade, 
                        data = dados %>% select(refritl5_prop,sexo_M_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl52 = geeglm(refritl5_prop ~ idade_60a79_prop, id = cidade, 
                        data = dados %>% select(refritl5_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl53 = geeglm(refritl5_prop ~ anos_de_estudo, id = cidade, 
                        data = dados %>% select(refritl5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl54 = geeglm(refritl5_prop ~ plano_saude_nao_prop, id = cidade, 
                        data = dados %>% select(refritl5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl55 = geeglm(refritl5_prop ~ Nota, id = cidade, 
                        data = dados %>% select(refritl5_prop,Nota,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl56 = geeglm(refritl5_prop ~ IVS, id = cidade, 
                        data = dados %>% select(refritl5_prop,IVS,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl57 = geeglm(refritl5_prop ~ Gini, id = cidade, 
                        data = dados %>% select(refritl5_prop,Gini,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl58 = geeglm(refritl5_prop ~ Gini, id = cidade, 
                        data = dados %>% select(refritl5_prop,Gini,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

Tabela4.7 = rbind(TabelaGEEGama(unig_refritl51),TabelaGEEGama(unig_refritl52),
                  TabelaGEEGama(unig_refritl53),TabelaGEEGama(unig_refritl54),
                  TabelaGEEGama(unig_refritl55),#TabelaGEEGama(unig_refritl56),
                  TabelaGEEGama(unig_refritl57)#TabelaGEEGama(unig_refritl58)
)
#write.xlsx(Tabela4.7 %>% as.data.frame(), 'Tabela 4.7.xlsx', rowNames = F)

#Sem interação
multig_semint_refritl51 = geeglm(refritl5_prop ~ #sexo_M_prop + idade_60a79_prop + 
                                   anos_de_estudo + 
                                   plano_saude_nao_prop + #Nota + 
                                   Gini, 
                                 id = cidade, data = dados %>% 
                                   select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                                 family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_refritl51)
#write.xlsx(TabelaGEEGama(multig_semint_refritl51) %>% as.data.frame(), 'Tabela 4.8.xlsx', rowNames = F)

#Com interação
multig_refritl51 = geeglm(refritl5_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl51)

#sexo_M_prop*anos_de_estudo
#sexo_M_prop*plano_saude_nao_prop
#sexo_M_prop*Gini
#idade_60a79_prop*Nota
#idade_60a79_prop*Gini
#plano_saude_nao_prop*Gini
#anos_de_estudo*plano_saude_nao_prop
#anos_de_estudo*Gini
#idade_60a79_prop*plano_saude_nao_prop
#sexo_M_prop*idade_60a79_prop
#plano_saude_nao_prop*Nota
multig_refritl52 = geeglm(refritl5_prop ~ sexo_M_prop*Nota +
                            idade_60a79_prop*anos_de_estudo +
                            anos_de_estudo*Nota +
                            plano_saude_nao_prop +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(refritl5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl52)
#write.xlsx(TabelaGEEGama(multig_refritl52) %>% as.data.frame(), 'Tabela 4.9.xlsx', rowNames = F)

####==============
#### feijao5_prop
####==============
####======
#### Beta
####======
hist(dados$feijao5_prop)
ks.test(dados$feijao5_prop, "pnorm", mean(dados$feijao5_prop, na.rm = T), sd(dados$feijao5_prop, na.rm = T))

fit_feijao5 = lm(feijao5_prop ~ factor(ano), data=dados)
res_feijao50 = subset(fit_feijao5$residuals, dados$ano == 2010)
res_feijao51 = subset(fit_feijao5$residuals, dados$ano == 2011)
res_feijao52 = subset(fit_feijao5$residuals, dados$ano == 2012)
res_feijao53 = subset(fit_feijao5$residuals, dados$ano == 2013)
res_feijao54 = subset(fit_feijao5$residuals, dados$ano == 2014)
res_feijao55 = subset(fit_feijao5$residuals, dados$ano == 2015)
res_feijao56 = subset(fit_feijao5$residuals, dados$ano == 2016)
res_feijao57 = subset(fit_feijao5$residuals, dados$ano == 2017)
res_feijao58 = subset(fit_feijao5$residuals, dados$ano == 2018)
res_feijao59 = subset(fit_feijao5$residuals, dados$ano == 2019)

res_feijao5 = data.frame(res_feijao50, res_feijao51, res_feijao52, res_feijao53, res_feijao54, res_feijao55, res_feijao56, res_feijao57, res_feijao58, res_feijao59)
cor(res_feijao5)
cov(res_feijao5)

uni_feijao51 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + sexo_M_prop, 
                        data = dados %>% select(feijao5_prop,sexo_M_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao52 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + idade_60a79_prop, 
                        data = dados %>% select(feijao5_prop,idade_60a79_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao53 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + anos_de_estudo, 
                        data = dados %>% select(feijao5_prop,anos_de_estudo,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao54 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + plano_saude_nao_prop, 
                        data = dados %>% select(feijao5_prop,plano_saude_nao_prop,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao55 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + Nota, 
                        data = dados %>% select(feijao5_prop,Nota,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao56 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + IVS, 
                        data = dados %>% select(feijao5_prop,IVS,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao57 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + IDHM, 
                        data = dados %>% select(feijao5_prop,IDHM,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))
uni_feijao58 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + Gini, 
                        data = dados %>% select(feijao5_prop,Gini,cidade_nome) %>% na.omit(), 
                        family = beta_family(link = "logit"))

cor.test(dados$feijao5_prop,dados$IVS)
cor.test(dados$feijao5_prop,dados$IDHM)
cor.test(dados$feijao5_prop,dados$Gini)

Tabela5.1 = rbind(TabelaGLMMBeta(uni_feijao51),TabelaGLMMBeta(uni_feijao52),
                  TabelaGLMMBeta(uni_feijao53),TabelaGLMMBeta(uni_feijao54),
                  TabelaGLMMBeta(uni_feijao55),#TabelaGLMMBeta(uni_feijao56)
                  TabelaGLMMBeta(uni_feijao57)#TabelaGLMMBeta(uni_feijao58)
)
#write.xlsx(Tabela5.1 %>% as.data.frame(), 'Tabela 5.1.xlsx', rowNames = F)

#Sem interação
multi_semint_feijao51 = glmmTMB(feijao5_prop ~ (1 | cidade_nome) + #sexo_M_prop + 
                                  #idade_60a79_prop + 
                                  anos_de_estudo + 
                                  #plano_saude_nao_prop + 
                                  #Nota + 
                                  IDHM, 
                                 data = dados %>% 
                                   select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                                 family = beta_family(link = "logit"))
summary(multi_semint_feijao51)
write.xlsx(TabelaGLMMBeta(multi_semint_feijao51) %>% as.data.frame(), 'Tabela 5.2.xlsx', rowNames = F)

#Com interação
multi_feijao51 = glmmTMB(feijao5_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM + 
                            (1 | cidade_nome), data = dados %>% 
                            select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                          family = beta_family(link = "logit"))
summary(multi_feijao51)

#sexo_M_prop*IDHM
#anos_de_estudo*Nota
#sexo_M_prop*plano_saude_nao_prop
#sexo_M_prop*anos_de_estudo
#anos_de_estudo*plano_saude_nao_prop
#idade_60a79_prop*plano_saude_nao_prop
#plano_saude_nao_prop*Nota
#sexo_M_prop*idade_60a79_prop
#idade_60a79_prop*Nota
#anos_de_estudo*IDHM
#idade_60a79_prop*IDHM
multi_feijao52 = glmmTMB(feijao5_prop ~ sexo_M_prop*Nota +
                           idade_60a79_prop*anos_de_estudo +
                           plano_saude_nao_prop*IDHM +
                           Nota*IDHM + 
                           (1 | cidade_nome), data = dados %>% 
                           select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
                         family = beta_family(link = "logit"))
summary(multi_feijao52)
write.xlsx(TabelaGLMMBeta(multi_feijao52) %>% as.data.frame(), 'Tabela 5.3.xlsx', rowNames = F)

####========
#### Normal
####========
uni_n_feijao51 = geeglm(feijao5_prop ~ sexo_M_prop, id = cidade, 
                         data = dados %>% select(feijao5_prop,sexo_M_prop,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao52 = geeglm(feijao5_prop ~ idade_60a79_prop, id = cidade, 
                         data = dados %>% select(feijao5_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao53 = geeglm(feijao5_prop ~ anos_de_estudo, id = cidade, 
                         data = dados %>% select(feijao5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao54 = geeglm(feijao5_prop ~ plano_saude_nao_prop, id = cidade, 
                         data = dados %>% select(feijao5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao55 = geeglm(feijao5_prop ~ Nota, id = cidade, 
                         data = dados %>% select(feijao5_prop,Nota,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao56 = geeglm(feijao5_prop ~ IVS, id = cidade, 
                         data = dados %>% select(feijao5_prop,IVS,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao57 = geeglm(feijao5_prop ~ Gini, id = cidade, 
                         data = dados %>% select(feijao5_prop,Gini,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_n_feijao58 = geeglm(feijao5_prop ~ Gini, id = cidade, 
                         data = dados %>% select(feijao5_prop,Gini,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados$feijao5_prop,dados$IVS)
cor.test(dados$feijao5_prop,dados$IDHM)
cor.test(dados$feijao5_prop,dados$Gini)

Tabela5.4 = rbind(TabelaGEENormal(uni_n_feijao51),TabelaGEENormal(uni_n_feijao52),
                  TabelaGEENormal(uni_n_feijao53),TabelaGEENormal(uni_n_feijao54),
                  TabelaGEENormal(uni_n_feijao55),#TabelaGEENormal(uni_n_feijao56),
                  TabelaGEENormal(uni_n_feijao57)#TabelaGEENormal(uni_n_feijao58)
)
write.xlsx(Tabela5.4 %>% as.data.frame(), 'Tabela 5.4.xlsx', rowNames = F)

#Sem interação
multi_n_semint_feijao51 = geeglm(feijao5_prop ~ #sexo_M_prop + 
                                   idade_60a79_prop + 
                                    anos_de_estudo + 
                                    plano_saude_nao_prop + #Nota + 
                                    IDHM, 
                                  id = cidade, data = dados %>% 
                                    select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                  family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_semint_feijao51)
write.xlsx(TabelaGEENormal(multi_n_semint_feijao51) %>% as.data.frame(), 'Tabela 5.5.xlsx', rowNames = F)

hist(multi_n_semint_feijao51$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_n_semint_feijao51$residuals),max(multi_n_semint_feijao51$residuals), length.out = 100),
      dnorm(seq(min(multi_n_semint_feijao51$residuals),max(multi_n_semint_feijao51$residuals), length.out = 100),
            mean=mean(multi_n_semint_feijao51$residuals),sd=sd(multi_n_semint_feijao51$residuals)))

ks.test(multi_n_semint_feijao51$residuals, "pnorm", mean(multi_n_semint_feijao51$residuals, na.rm = T), 
        sd(multi_n_semint_feijao51$residuals, na.rm = T))
qqnorm(multi_n_semint_feijao51$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_n_semint_feijao51$residuals)

#Com interação
multi_n_feijao51 = geeglm(feijao5_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados %>% 
                            select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_feijao51)

#plano_saude_nao_prop*Nota
#anos_de_estudo*IDHM
#sexo_M_prop*anos_de_estudo
#sexo_M_prop*Nota
#idade_60a79_prop*plano_saude_nao_prop
#sexo_M_prop*idade_60a79_prop
#idade_60a79_prop*Nota
#plano_saude_nao_prop*IDHM
#idade_60a79_prop*anos_de_estudo
#anos_de_estudo*plano_saude_nao_prop
multi_n_feijao52 = geeglm(feijao5_prop ~ sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*IDHM +
                            idade_60a79_prop*IDHM +
                            anos_de_estudo*Nota +
                            Nota*IDHM, 
                          id = cidade, data = dados %>% 
                            select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_n_feijao52)
write.xlsx(TabelaGEENormal(multi_n_feijao52) %>% as.data.frame(), 'Tabela 5.6.xlsx', rowNames = F)

hist(multi_feijao52$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_feijao52$residuals),max(multi_feijao52$residuals), length.out = 100),
      dnorm(seq(min(multi_feijao52$residuals),max(multi_feijao52$residuals), length.out = 100),
            mean=mean(multi_feijao52$residuals),sd=sd(multi_feijao52$residuals)))

qqnorm(multi_feijao52$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_feijao52$residuals)

ks.test(multi_feijao52$residuals, "pnorm", mean(multi_feijao52$residuals, na.rm = T), sd(multi_feijao52$residuals, na.rm = T))

####=======
#### Gamma
####=======
unig_feijao51 = geeglm(feijao5_prop ~ sexo_M_prop, id = cidade, 
                        data = dados %>% select(feijao5_prop,sexo_M_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao52 = geeglm(feijao5_prop ~ idade_60a79_prop, id = cidade, 
                        data = dados %>% select(feijao5_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao53 = geeglm(feijao5_prop ~ anos_de_estudo, id = cidade, 
                        data = dados %>% select(feijao5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao54 = geeglm(feijao5_prop ~ plano_saude_nao_prop, id = cidade, 
                        data = dados %>% select(feijao5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao55 = geeglm(feijao5_prop ~ Nota, id = cidade, 
                        data = dados %>% select(feijao5_prop,Nota,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao56 = geeglm(feijao5_prop ~ IVS, id = cidade, 
                        data = dados %>% select(feijao5_prop,IVS,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao57 = geeglm(feijao5_prop ~ IDHM, id = cidade, 
                        data = dados %>% select(feijao5_prop,IDHM,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao58 = geeglm(feijao5_prop ~ Gini, id = cidade, 
                        data = dados %>% select(feijao5_prop,Gini,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

Tabela5.7 = rbind(TabelaGEEGama(unig_feijao51),TabelaGEEGama(unig_feijao52),
                  TabelaGEEGama(unig_feijao53),TabelaGEEGama(unig_feijao54),
                  TabelaGEEGama(unig_feijao55),#TabelaGEEGama(unig_feijao56),
                  TabelaGEEGama(unig_feijao57)#TabelaGEEGama(unig_feijao58)
)
write.xlsx(Tabela5.7 %>% as.data.frame(), 'Tabela 5.7.xlsx', rowNames = F)

#Sem interação
multig_semint_feijao51 = geeglm(feijao5_prop ~ #sexo_M_prop + idade_60a79_prop + 
                                   anos_de_estudo + 
                                   plano_saude_nao_prop + #Nota + 
                                   IDHM, 
                                 id = cidade, data = dados %>% 
                                   select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                 family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_feijao51)
write.xlsx(TabelaGEEGama(multig_semint_feijao51) %>% as.data.frame(), 'Tabela 5.8.xlsx', rowNames = F)

#Com interação
multig_feijao51 = geeglm(feijao5_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*IDHM +
                           idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados %>% 
                           select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao51)

#anos_de_estudo*IDHM
#plano_saude_nao_prop*Nota
#sexo_M_prop*anos_de_estudo
#sexo_M_prop*Nota
#sexo_M_prop*idade_60a79_prop
#idade_60a79_prop*Nota
#idade_60a79_prop*plano_saude_nao_prop
#plano_saude_nao_prop*IDHM
#idade_60a79_prop*anos_de_estudo
#
multig_feijao52 = geeglm(feijao5_prop ~ sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*IDHM +
                           idade_60a79_prop*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                           Nota*IDHM, 
                         id = cidade, data = dados %>% 
                           select(feijao5_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao52)
write.xlsx(TabelaGEEGama(multig_feijao52) %>% as.data.frame(), 'Tabela 5.9.xlsx', rowNames = F)

####======
#### hart
####======
hist(dados$hart_prop)
ks.test(dados$hart_prop, "pnorm", mean(dados$hart_prop, na.rm = T), sd(dados$hart_prop, na.rm = T))

fit_hart = lm(hart_prop ~ factor(sexo_M_prop)*factor(ano), data=dados)
res_hart0 = subset(fit_hart$residuals, dados$ano == 2010)
res_hart1 = subset(fit_hart$residuals, dados$ano == 2011)
res_hart2 = subset(fit_hart$residuals, dados$ano == 2012)
res_hart3 = subset(fit_hart$residuals, dados$ano == 2013)
res_hart4 = subset(fit_hart$residuals, dados$ano == 2014)
res_hart5 = subset(fit_hart$residuals, dados$ano == 2015)
res_hart6 = subset(fit_hart$residuals, dados$ano == 2016)
res_hart7 = subset(fit_hart$residuals, dados$ano == 2017)
res_hart8 = subset(fit_hart$residuals, dados$ano == 2018)
res_hart9 = subset(fit_hart$residuals, dados$ano == 2019)

res_hart = data.frame(res_hart0, res_hart1, res_hart2, res_hart3, res_hart4, res_hart5, res_hart6, res_hart7, res_hart8, res_hart9)
cor(res_hart)
cov(res_hart)

uni_hart1 = geeglm(hart_prop ~ sexo_M_prop, id = cidade, 
                   data = dados %>% select(hart_prop,sexo_M_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart2 = geeglm(hart_prop ~ idade_60a79_prop, id = cidade, 
                   data = dados %>% select(hart_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart3 = geeglm(hart_prop ~ anos_de_estudo, id = cidade, 
                   data = dados %>% select(hart_prop,anos_de_estudo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart4 = geeglm(hart_prop ~ plano_saude_nao_prop, id = cidade, 
                   data = dados %>% select(hart_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart5 = geeglm(hart_prop ~ Nota, id = cidade, 
                   data = dados %>% select(hart_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart6 = geeglm(hart_prop ~ IVS, id = cidade, 
                   data = dados %>% select(hart_prop,IVS,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart7 = geeglm(hart_prop ~ IDHM, id = cidade, 
                   data = dados %>% select(hart_prop,IDHM,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart8 = geeglm(hart_prop ~ Gini, id = cidade, 
                   data = dados %>% select(hart_prop,Gini,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados$hart_prop,dados$IVS)
cor.test(dados$hart_prop,dados$IDHM)
cor.test(dados$hart_prop,dados$Gini)

Tabela22.1 = rbind(TabelaGEENormal(uni_hart1),TabelaGEENormal(uni_hart2),
                   TabelaGEENormal(uni_hart3),TabelaGEENormal(uni_hart4),
                   TabelaGEENormal(uni_hart5),#TabelaGEENormal(uni_hart6),TabelaGEENormal(uni_hart7)
                   TabelaGEENormal(uni_hart8)
)
#write.xlsx(Tabela22.1 %>% as.data.frame(), 'Tabela 22.1.xlsx', rowNames = F)

#Sem interação: Nota
multi_semint_hart1 = geeglm(hart_prop ~ sexo_M_prop + idade_60a79_prop + anos_de_estudo + 
                              plano_saude_nao_prop + Nota + 
                              Gini, 
                            id = cidade, data = dados %>% 
                              select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                            family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_hart1)
#write.xlsx(TabelaGEENormal(multi_semint_hart1) %>% as.data.frame(), 'Tabela 22.2.xlsx', rowNames = F)

hist(multi_semint_hart1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_hart1$residuals),max(multi_semint_hart1$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_hart1$residuals),max(multi_semint_hart1$residuals), length.out = 100),
            mean=mean(multi_semint_hart1$residuals),sd=sd(multi_semint_hart1$residuals)))

qqnorm(multi_semint_hart1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_hart1$residuals)

ks.test(multi_semint_hart1$residuals, "pnorm", mean(multi_semint_hart1$residuals, na.rm = T), sd(multi_semint_hart1$residuals, na.rm = T))

#Com interação
multi_hart1 = geeglm(hart_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart1)

#sexo_M_prop*Nota
multi_hart2 = geeglm(hart_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart2)

#idade_60a79_prop*Gini
multi_hart3 = geeglm(hart_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart3)

#sexo_M_prop*idade_60a79_prop
multi_hart4 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart4)

#plano_saude_nao_prop*Nota
multi_hart5 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart5)

#idade_60a79_prop*anos_de_estudo
multi_hart6 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart6)

#anos_de_estudo*Nota
multi_hart7 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart7)

#idade_60a79_prop*Nota
multi_hart8 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*plano_saude_nao_prop +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart8)

#plano_saude_nao_prop*Gini
multi_hart9 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*plano_saude_nao_prop +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart9)

#Nota*Gini
multi_hart10 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini + Nota +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados %>% 
                        select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart10)

#Nota
multi_hart11 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados %>% 
                        select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart11)

#anos_de_estudo*Gini
multi_hart12 = geeglm(hart_prop ~ sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop,
                      id = cidade, data = dados %>% 
                        select(hart_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart12)
#write.xlsx(TabelaGEENormal(multi_hart12) %>% as.data.frame(), 'Tabela 22.3.xlsx', rowNames = F)

hist(multi_hart12$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_hart12$residuals),max(multi_hart12$residuals), length.out = 100),
      dnorm(seq(min(multi_hart12$residuals),max(multi_hart12$residuals), length.out = 100),
            mean=mean(multi_hart12$residuals),sd=sd(multi_hart12$residuals)))

qqnorm(multi_hart12$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_hart12$residuals)

ks.test(multi_hart12$residuals, "pnorm", mean(multi_hart12$residuals, na.rm = T), sd(multi_hart12$residuals, na.rm = T))

####======
#### diab
####======
####========
#### Normal
####========
hist(dados$diab_prop)
ks.test(dados$diab_prop, "pnorm", mean(dados$diab_prop, na.rm = T), sd(dados$diab_prop, na.rm = T))

fit_diab = lm(diab_prop ~ factor(sexo_M_prop)*factor(ano), data=dados)
res_diab0 = subset(fit_diab$residuals, dados$ano == 2010)
res_diab1 = subset(fit_diab$residuals, dados$ano == 2011)
res_diab2 = subset(fit_diab$residuals, dados$ano == 2012)
res_diab3 = subset(fit_diab$residuals, dados$ano == 2013)
res_diab4 = subset(fit_diab$residuals, dados$ano == 2014)
res_diab5 = subset(fit_diab$residuals, dados$ano == 2015)
res_diab6 = subset(fit_diab$residuals, dados$ano == 2016)
res_diab7 = subset(fit_diab$residuals, dados$ano == 2017)
res_diab8 = subset(fit_diab$residuals, dados$ano == 2018)
res_diab9 = subset(fit_diab$residuals, dados$ano == 2019)

res_diab = data.frame(res_diab0, res_diab1, res_diab2, res_diab3, res_diab4, res_diab5, res_diab6, res_diab7, res_diab8, res_diab9)
cor(res_diab)
cov(res_diab)

uni_diab1 = geeglm(diab_prop ~ sexo_M_prop, id = cidade, 
                   data = dados %>% select(diab_prop,sexo_M_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab2 = geeglm(diab_prop ~ idade_60a79_prop, id = cidade, 
                   data = dados %>% select(diab_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab3 = geeglm(diab_prop ~ anos_de_estudo, id = cidade, 
                   data = dados %>% select(diab_prop,anos_de_estudo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab4 = geeglm(diab_prop ~ plano_saude_nao_prop, id = cidade, 
                   data = dados %>% select(diab_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab5 = geeglm(diab_prop ~ Nota, id = cidade, 
                   data = dados %>% select(diab_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab6 = geeglm(diab_prop ~ IVS, id = cidade, 
                   data = dados %>% select(diab_prop,IVS,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab7 = geeglm(diab_prop ~ IDHM, id = cidade, 
                   data = dados %>% select(diab_prop,IDHM,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab8 = geeglm(diab_prop ~ Gini, id = cidade, 
                   data = dados %>% select(diab_prop,Gini,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados$diab_prop,dados$IVS)
cor.test(dados$diab_prop,dados$IDHM)
cor.test(dados$diab_prop,dados$Gini)

Tabela23.1 = rbind(TabelaGEENormal(uni_diab1),TabelaGEENormal(uni_diab2),
                   TabelaGEENormal(uni_diab3),TabelaGEENormal(uni_diab4),
                   TabelaGEENormal(uni_diab5),#TabelaGEENormal(uni_diab6),TabelaGEENormal(uni_diab7)
                   TabelaGEENormal(uni_diab8)
)
#write.xlsx(Tabela23.1 %>% as.data.frame(), 'Tabela 23.1.xlsx', rowNames = F)

#Sem interação
multi_semint_diab1 = geeglm(diab_prop ~ #sexo_M_prop + 
                              idade_60a79_prop + #anos_de_estudo + 
                              #plano_saude_nao_prop + #Nota + 
                              Gini, 
                            id = cidade, data = dados %>% 
                              select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                            family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_diab1)
#write.xlsx(TabelaGEENormal(multi_semint_diab1) %>% as.data.frame(), 'Tabela 23.2.xlsx', rowNames = F)

hist(multi_semint_diab1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_diab1$residuals),max(multi_semint_diab1$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_diab1$residuals),max(multi_semint_diab1$residuals), length.out = 100),
            mean=mean(multi_semint_diab1$residuals),sd=sd(multi_semint_diab1$residuals)))

qqnorm(multi_semint_diab1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_diab1$residuals)

ks.test(multi_semint_diab1$residuals, "pnorm", mean(multi_semint_diab1$residuals, na.rm = T), sd(multi_semint_diab1$residuals, na.rm = T))

#Com interação
multi_diab1 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab1)

#anos_de_estudo*Nota
multi_diab2 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab2)

#plano_saude_nao_prop*Nota
multi_diab3 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab3)

#sexo_M_prop*Nota
multi_diab4 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab4)

#idade_60a79_prop*anos_de_estudo
multi_diab5 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab5)

#Nota*Gini
multi_diab6 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                       idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab6)

#sexo_M_prop*Gini
multi_diab7 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                       idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab7)

#idade_60a79_prop*Gini
multi_diab8 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                       idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab8)

#idade_60a79_prop*Nota
multi_diab9 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                       idade_60a79_prop*plano_saude_nao_prop +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini, 
                     id = cidade, data = dados %>% 
                       select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab9)

#plano_saude_nao_prop*Gini
multi_diab10 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab10)

#sexo_M_prop*plano_saude_nao_prop
multi_diab11 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + Nota +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab11)

#sexo_M_prop*plano_saude_nao_prop
multi_diab12 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
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
unig_diab1 = geeglm(diab_prop ~ sexo_M_prop, id = cidade, 
                    data = dados %>% select(diab_prop,sexo_M_prop,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab2 = geeglm(diab_prop ~ idade_60a79_prop, id = cidade, 
                    data = dados %>% select(diab_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab3 = geeglm(diab_prop ~ anos_de_estudo, id = cidade, 
                    data = dados %>% select(diab_prop,anos_de_estudo,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab4 = geeglm(diab_prop ~ plano_saude_nao_prop, id = cidade, 
                    data = dados %>% select(diab_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab5 = geeglm(diab_prop ~ Nota, id = cidade, 
                    data = dados %>% select(diab_prop,Nota,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab6 = geeglm(diab_prop ~ IVS, id = cidade, 
                    data = dados %>% select(diab_prop,IVS,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab7 = geeglm(diab_prop ~ IDHM, id = cidade, 
                    data = dados %>% select(diab_prop,IDHM,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab8 = geeglm(diab_prop ~ Gini, id = cidade, 
                    data = dados %>% select(diab_prop,Gini,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

Tabela23.4 = rbind(TabelaGEEGama(unig_diab1),TabelaGEEGama(unig_diab2),
                   TabelaGEEGama(unig_diab3),TabelaGEEGama(unig_diab4),
                   TabelaGEEGama(unig_diab5),#TabelaGEEGama(unig_diab6),TabelaGEEGama(unig_diab7)
                   TabelaGEEGama(unig_diab8)
)
#write.xlsx(Tabela23.4 %>% as.data.frame(), 'Tabela 23.4.xlsx', rowNames = F)

#Sem interação
multig_semint_diab1 = geeglm(diab_prop ~ sexo_M_prop + 
                               idade_60a79_prop + #anos_de_estudo + 
                               #plano_saude_nao_prop + #Nota + 
                               Gini, 
                             id = cidade, data = dados %>% 
                               select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                             family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_diab1)
#write.xlsx(TabelaGEEGama(multig_semint_diab1) %>% as.data.frame(), 'Tabela 23.5.xlsx', rowNames = F)

#Com interação
multig_diab1 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                        idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab1)

#idade_60a79_prop*Nota
multig_diab2 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                        idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab2)

#plano_saude_nao_prop*Nota
multig_diab3 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                        idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini +
                        Nota*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab3)

#Nota*Gini
multig_diab4 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                        idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab4)

#sexo_M_prop*Nota
multig_diab5 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                        idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini + Nota, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab5)

#Nota

#idade_60a79_prop*anos_de_estudo
multig_diab6 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                        idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab6)

#anos_de_estudo*Nota
multig_diab7 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Gini +
                        idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab7)

#sexo_M_prop*Gini
multig_diab8 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                        idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab8)

#idade_60a79_prop*Gini
multig_diab9 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                        idade_60a79_prop*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados %>% 
                        select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab9)

#anos_de_estudo*Gini
multig_diab10 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                         idade_60a79_prop*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop +
                         plano_saude_nao_prop*Gini, 
                       id = cidade, data = dados %>% 
                         select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab10)

#plano_saude_nao_prop*Gini
multig_diab11 = geeglm(diab_prop ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop +
                         idade_60a79_prop*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop + Gini, 
                       id = cidade, data = dados %>% 
                         select(diab_prop,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab11)
#write.xlsx(TabelaGEEGama(multig_diab11) %>% as.data.frame(), 'Tabela 23.6.xlsx', rowNames = F)

####===========
#### TaxaICSAP
####===========
hist(dados$TaxaICSAP)
ks.test(dados$TaxaICSAP, "pnorm", mean(dados$TaxaICSAP, na.rm = T), sd(dados$TaxaICSAP, na.rm = T))

uni_TaxaICSAP1 = geeglm(TaxaICSAP ~ sexo_M_prop, id = cidade, 
                        data = dados %>% select(TaxaICSAP,sexo_M_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP2 = geeglm(TaxaICSAP ~ idade_60a79_prop, id = cidade, 
                        data = dados %>% select(TaxaICSAP,idade_60a79_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP3 = geeglm(TaxaICSAP ~ anos_de_estudo, id = cidade, 
                        data = dados %>% select(TaxaICSAP,anos_de_estudo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP4 = geeglm(TaxaICSAP ~ plano_saude_nao_prop, id = cidade, 
                        data = dados %>% select(TaxaICSAP,plano_saude_nao_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP5 = geeglm(TaxaICSAP ~ Nota, id = cidade, 
                        data = dados %>% select(TaxaICSAP,Nota,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP6 = geeglm(TaxaICSAP ~ IVS, id = cidade, 
                        data = dados %>% select(TaxaICSAP,IVS,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP7 = geeglm(TaxaICSAP ~ IDHM, id = cidade, 
                        data = dados %>% select(TaxaICSAP,IDHM,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP8 = geeglm(TaxaICSAP ~ Gini, id = cidade, 
                        data = dados %>% select(TaxaICSAP,Gini,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

cor.test(dados$TaxaICSAP,dados$IVS)
cor.test(dados$TaxaICSAP,dados$IDHM)
cor.test(dados$TaxaICSAP,dados$Gini)

Tabela24.1 = rbind(TabelaGEEGama(uni_TaxaICSAP1),TabelaGEEGama(uni_TaxaICSAP2),
                   TabelaGEEGama(uni_TaxaICSAP3),TabelaGEEGama(uni_TaxaICSAP4),
                   TabelaGEEGama(uni_TaxaICSAP5),#TabelaGEEGama(uni_TaxaICSAP6)
                   #TabelaGEEGama(uni_TaxaICSAP7)
                   TabelaGEEGama(uni_TaxaICSAP8)
)
#write.xlsx(Tabela24.1 %>% as.data.frame(), 'Tabela 24.1.xlsx', rowNames = F)

#Sem interação: Nota
multi_semint_TaxaICSAP1 = geeglm(TaxaICSAP ~ sexo_M_prop + 
                                   idade_60a79_prop + anos_de_estudo + 
                                   plano_saude_nao_prop + #Nota + 
                                   Gini, 
                                 id = cidade, data = dados %>% 
                                   select(TaxaICSAP,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                                 family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_semint_TaxaICSAP1)
#write.xlsx(TabelaGEEGama(multi_semint_TaxaICSAP1) %>% as.data.frame(), 'Tabela 24.2.xlsx', rowNames = F)

#Com interação
multi_TaxaICSAP1 = geeglm(TaxaICSAP ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(TaxaICSAP,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP1)

#plano_saude_nao_prop*Nota
multi_TaxaICSAP2 = geeglm(TaxaICSAP ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*plano_saude_nao_prop + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(TaxaICSAP,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP2)

#idade_60a79_prop*plano_saude_nao_prop
multi_TaxaICSAP3 = geeglm(TaxaICSAP ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(TaxaICSAP,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP3)

#anos_de_estudo*Nota
multi_TaxaICSAP4 = geeglm(TaxaICSAP ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*plano_saude_nao_prop + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(TaxaICSAP,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP4)

#sexo_M_prop*plano_saude_nao_prop
multi_TaxaICSAP5 = geeglm(TaxaICSAP ~ sexo_M_prop*idade_60a79_prop + sexo_M_prop*anos_de_estudo + sexo_M_prop*Nota + sexo_M_prop*Gini +
                            idade_60a79_prop*anos_de_estudo + idade_60a79_prop*Nota + idade_60a79_prop*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados %>% 
                            select(TaxaICSAP,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP5)
#write.xlsx(TabelaGEEGama(multi_TaxaICSAP5) %>% as.data.frame(), 'Tabela 24.3.xlsx', rowNames = F)

#hnp::hnp(multi_TaxaICSAP5, resid.type='pearson')

####====================
#### Análise descritiva
####====================
# Tabela1Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_CO[[variavel]], dados_CO$ano)}))
# Tabela2Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_ND[[variavel]], dados_ND$ano)}))
# Tabela3Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_NT[[variavel]], dados_NT$ano)}))
# Tabela4Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_Sud[[variavel]], dados_Sud$ano)}))
# Tabela5Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_Sul[[variavel]], dados_Sul$ano)}))
# 
# write.xlsx(Tabela1Desc %>% as.data.frame(), 'Tabela 1 Desc.xlsx', rowNames = T)
# write.xlsx(Tabela2Desc %>% as.data.frame(), 'Tabela 2 Desc.xlsx', rowNames = T)
# write.xlsx(Tabela3Desc %>% as.data.frame(), 'Tabela 3 Desc.xlsx', rowNames = T)
# write.xlsx(Tabela4Desc %>% as.data.frame(), 'Tabela 4 Desc.xlsx', rowNames = T)
# write.xlsx(Tabela5Desc %>% as.data.frame(), 'Tabela 5 Desc.xlsx', rowNames = T)

####====================================
#### Comparações por ano - 20 a 79 anos
####====================================
# Tabela2.0 = rbind(FriedmanTeste(dados$IMC_media, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$IMC_cat_baixo_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$IMC_cat_excesso_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$IMC_i_media, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$IMC_i_cat_baixo_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$IMC_i_cat_excesso_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$flvreg_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$flvdia_media, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$flvreco_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$refritl5_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$feijao5_prop, dados$ano, dados$cidade_nome)$tabela,#
#                   FriedmanTeste(dados$hart_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$diab_prop, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$ANEMIA, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$DEFICIENCIAS_NUTRICIONAIS, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$DIABETES_MELITUS, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$HIPERTENSAO, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$SomaICSAP, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados$TaxaICSAP, dados$ano, dados$cidade_nome)$tabela,
#                   FriedmanTeste(dados_comp$Nota, dados_comp$ano, dados_comp$cidade_nome)$tabela,
#                   FriedmanTeste(dados_inc$Nota, dados_inc$ano, dados_inc$cidade_nome)$tabela)

# Tabela2.0.1 = rbind(FriedmanTeste(dados$IMC_media, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$IMC_cat_baixo_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$IMC_cat_excesso_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$IMC_i_media, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$IMC_i_cat_baixo_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$IMC_i_cat_excesso_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$flvreg_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$flvdia_media, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$flvreco_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$refritl5_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$hart_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$diab_prop, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$ANEMIA, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$DEFICIENCIAS_NUTRICIONAIS, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$DIABETES_MELITUS, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$HIPERTENSAO, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$SomaICSAP, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados$TaxaICSAP, dados$ano, dados$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_comp$Nota, dados_comp$ano, dados_comp$cidade_nome)$C.Multiplas)
# Tabela2.0.2 = FriedmanTeste(dados_inc$Nota, dados_inc$ano, dados_inc$cidade_nome)$C.Multiplas
# Tabela2.0.3 = FriedmanTeste(dados$feijao5_prop, dados$ano, dados$cidade_nome)$C.Multiplas
# write.xlsx(Tabela2.0 %>% as.data.frame(), 'Tabela 2.0.xlsx', rowNames = T)
# write.xlsx(Tabela2.0.1 %>% as.data.frame(), 'Tabela 2.0.1.xlsx', rowNames = T)
# write.xlsx(Tabela2.0.2 %>% as.data.frame(), 'Tabela 2.0.2.xlsx', rowNames = T)
# write.xlsx(Tabela2.0.3 %>% as.data.frame(), 'Tabela 2.0.3.xlsx', rowNames = T)

# Tabela2 = rbind(FriedmanTeste(dados_CO$IMC_media, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$IMC_cat_baixo_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$IMC_cat_excesso_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$IMC_i_media, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$IMC_i_cat_baixo_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$IMC_i_cat_excesso_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$flvreg_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$flvdia_media, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$flvreco_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$refritl5_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$feijao5_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,#
#                 FriedmanTeste(dados_CO$hart_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$diab_prop, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$ANEMIA, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$DEFICIENCIAS_NUTRICIONAIS, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$DIABETES_MELITUS, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$HIPERTENSAO, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$SomaICSAP, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$TaxaICSAP, dados_CO$ano, dados_CO$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO$Nota, dados_CO$ano, dados_CO$cidade_nome)$tabela)

# Tabela2.1 = rbind(FriedmanTeste(dados_CO$IMC_media, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$IMC_cat_baixo_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$IMC_cat_excesso_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$IMC_i_media, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$IMC_i_cat_baixo_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$IMC_i_cat_excesso_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$flvreg_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$flvdia_media, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$flvreco_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$refritl5_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$hart_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$diab_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$ANEMIA, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$DEFICIENCIAS_NUTRICIONAIS, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$DIABETES_MELITUS, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$HIPERTENSAO, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$SomaICSAP, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$TaxaICSAP, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO$Nota, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas)


#Tabela2.2 = FriedmanTeste(dados_CO$feijao5_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas
# Tabela2 = do.call(rbind,lapply(variaveis, function(variavel) {FriedmanTeste(dados_CO[[variavel]], dados_CO$ano, dados_CO$cidade_nome)$tabela}))
# Tabela2.1 = do.call(rbind,lapply(variaveis, function(variavel) {FriedmanTeste(dados_CO[[variavel]], dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas}))
# write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)
# write.xlsx(Tabela2.1 %>% as.data.frame(), 'Tabela 2.1.xlsx', rowNames = T)
# write.xlsx(Tabela2.2 %>% as.data.frame(), 'Tabela 2.2.xlsx', rowNames = T)

# Tabela3 = rbind(FriedmanTeste(dados_ND$IMC_media, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$IMC_cat_baixo_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$IMC_cat_excesso_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$IMC_i_media, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$IMC_i_cat_baixo_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$IMC_i_cat_excesso_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$flvreg_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$flvdia_media, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$flvreco_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$refritl5_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$feijao5_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,#
#                 FriedmanTeste(dados_ND$hart_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$diab_prop, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$ANEMIA, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$DEFICIENCIAS_NUTRICIONAIS, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$DIABETES_MELITUS, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$HIPERTENSAO, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$SomaICSAP, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND$TaxaICSAP, dados_ND$ano, dados_ND$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_comp$Nota, dados_ND_comp$ano, dados_ND_comp$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_inc$Nota, dados_ND_inc$ano, dados_ND_inc$cidade_nome)$tabela)

# Tabela3.1 = rbind(FriedmanTeste(dados_ND$IMC_media, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$IMC_cat_baixo_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$IMC_cat_excesso_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$IMC_i_media, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$IMC_i_cat_baixo_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$IMC_i_cat_excesso_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$flvreg_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$flvdia_media, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$flvreco_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$refritl5_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   #FriedmanTeste(dados_ND$feijao5_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,#
#                   FriedmanTeste(dados_ND$hart_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$diab_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$ANEMIA, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$DEFICIENCIAS_NUTRICIONAIS, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$DIABETES_MELITUS, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$HIPERTENSAO, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$SomaICSAP, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND$TaxaICSAP, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_comp$Nota, dados_ND_comp$ano, dados_ND_comp$cidade_nome)$C.Multiplas)
# Tabela3.2 = FriedmanTeste(dados_ND_inc$Nota, dados_ND_inc$ano, dados_ND_inc$cidade_nome)$C.Multiplas
# Tabela3.3 = FriedmanTeste(dados_ND$feijao5_prop, dados_ND$ano, dados_ND$cidade_nome)$C.Multiplas

# write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)
# write.xlsx(Tabela3.1 %>% as.data.frame(), 'Tabela 3.1.xlsx', rowNames = T)
# write.xlsx(Tabela3.2 %>% as.data.frame(), 'Tabela 3.2.xlsx', rowNames = T)
# write.xlsx(Tabela3.3 %>% as.data.frame(), 'Tabela 3.3.xlsx', rowNames = T)

# Tabela4 = rbind(FriedmanTeste(dados_NT$IMC_media, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$IMC_cat_baixo_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$IMC_cat_excesso_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$IMC_i_media, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$IMC_i_cat_baixo_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$IMC_i_cat_excesso_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$flvreg_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$flvdia_media, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$flvreco_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$refritl5_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$feijao5_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,#
#                 FriedmanTeste(dados_NT$hart_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$diab_prop, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$ANEMIA, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$DEFICIENCIAS_NUTRICIONAIS, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$DIABETES_MELITUS, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$HIPERTENSAO, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$SomaICSAP, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT$TaxaICSAP, dados_NT$ano, dados_NT$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_comp$Nota, dados_NT_comp$ano, dados_NT_comp$cidade_nome)$tabela)
#                 #FriedmanTeste(dados_NT_inc$Nota, dados_NT_inc$ano, dados_NT_inc$cidade_nome)$tabela)

# Tabela4.1 = rbind(FriedmanTeste(dados_NT$IMC_media, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$IMC_cat_baixo_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$IMC_cat_excesso_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$IMC_i_media, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$IMC_i_cat_baixo_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$IMC_i_cat_excesso_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$flvreg_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$flvdia_media, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$flvreco_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$refritl5_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   #FriedmanTeste(dados_NT$feijao5_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,#
#                   FriedmanTeste(dados_NT$hart_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$diab_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$ANEMIA, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$DEFICIENCIAS_NUTRICIONAIS, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$DIABETES_MELITUS, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$HIPERTENSAO, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$SomaICSAP, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT$TaxaICSAP, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_comp$Nota, dados_NT_comp$ano, dados_NT_comp$cidade_nome)$C.Multiplas)
# Tabela4.2 = FriedmanTeste(dados_NT$feijao5_prop, dados_NT$ano, dados_NT$cidade_nome)$C.Multiplas
#Tabela4.3 = FriedmanTeste(dados_NT_inc$feijao5_prop, dados_NT_inc$ano, dados_NT_inc$cidade_nome)$C.Multiplas

# write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)
# write.xlsx(Tabela4.1 %>% as.data.frame(), 'Tabela 4.1.xlsx', rowNames = T)
# write.xlsx(Tabela4.2 %>% as.data.frame(), 'Tabela 4.2.xlsx', rowNames = T)

# Tabela5 = rbind(FriedmanTeste(dados_Sud$IMC_media, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$IMC_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$IMC_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$IMC_i_media, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$IMC_i_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$IMC_i_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$flvreg_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$flvdia_media, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$flvreco_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$refritl5_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$feijao5_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,#
#                 FriedmanTeste(dados_Sud$hart_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$diab_prop, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$ANEMIA, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$DEFICIENCIAS_NUTRICIONAIS, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$DIABETES_MELITUS, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$HIPERTENSAO, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$SomaICSAP, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$TaxaICSAP, dados_Sud$ano, dados_Sud$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sud$Nota, dados_Sud$ano, dados_Sud$cidade_nome)$tabela)

# Tabela5.1 = rbind(FriedmanTeste(dados_Sud$IMC_media, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$IMC_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$IMC_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$IMC_i_media, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$IMC_i_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$IMC_i_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$flvreg_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$flvdia_media, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$flvreco_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$refritl5_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   #FriedmanTeste(dados_Sud$feijao5_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,#
#                   FriedmanTeste(dados_Sud$hart_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$diab_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$ANEMIA, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$DEFICIENCIAS_NUTRICIONAIS, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$DIABETES_MELITUS, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$HIPERTENSAO, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$SomaICSAP, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$TaxaICSAP, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sud$Nota, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas)
# Tabela5.2 = FriedmanTeste(dados_Sud$feijao5_prop, dados_Sud$ano, dados_Sud$cidade_nome)$C.Multiplas

# write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)
# write.xlsx(Tabela5.1 %>% as.data.frame(), 'Tabela 5.1.xlsx', rowNames = T)
# write.xlsx(Tabela5.2 %>% as.data.frame(), 'Tabela 5.2.xlsx', rowNames = T)

# Tabela6 = rbind(FriedmanTeste(dados_Sul$IMC_media, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$IMC_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$IMC_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$IMC_i_media, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$IMC_i_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$IMC_i_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$flvreg_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$flvdia_media, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$flvreco_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$refritl5_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$feijao5_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,#
#                 FriedmanTeste(dados_Sul$hart_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$diab_prop, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$ANEMIA, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$DEFICIENCIAS_NUTRICIONAIS, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$DIABETES_MELITUS, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$HIPERTENSAO, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$SomaICSAP, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$TaxaICSAP, dados_Sul$ano, dados_Sul$cidade_nome)$tabela,
#                 FriedmanTeste(dados_Sul$Nota, dados_Sul$ano, dados_Sul$cidade_nome)$tabela)

# Tabela6.1 = rbind(FriedmanTeste(dados_Sul$IMC_media, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$IMC_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$IMC_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$IMC_i_media, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$IMC_i_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$IMC_i_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$flvreg_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$flvdia_media, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$flvreco_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$refritl5_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   #FriedmanTeste(dados_Sul$feijao5_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,#
#                   FriedmanTeste(dados_Sul$hart_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$diab_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$ANEMIA, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$DEFICIENCIAS_NUTRICIONAIS, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$DIABETES_MELITUS, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$HIPERTENSAO, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$SomaICSAP, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$TaxaICSAP, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_Sul$Nota, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas)
# Tabela6.2 = FriedmanTeste(dados_Sul$feijao5_prop, dados_Sul$ano, dados_Sul$cidade_nome)$C.Multiplas

# write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)
# write.xlsx(Tabela6.1 %>% as.data.frame(), 'Tabela 6.1.xlsx', rowNames = T)
# write.xlsx(Tabela6.2 %>% as.data.frame(), 'Tabela 6.2.xlsx', rowNames = T)
