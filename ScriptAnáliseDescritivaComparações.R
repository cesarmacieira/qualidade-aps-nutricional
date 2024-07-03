####===================================================================
#### Trabalho Catarina - Análises descritiva e comparações/associações
####===================================================================
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
dados = tryCatch({read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 23-05-2024.xlsx", sheet = 1)},
                 error = function(e) {read.xlsx("D:/NESCON/Trabalho - Catarina/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 23-05-2024.xlsx", sheet = 1)})

#dados1 = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 06-05-2024.xlsx", sheet = 1)
dados = dados %>% 
  mutate(IMC_i_cat_excesso_prop_inv = 1-IMC_i_cat_excesso_prop,
         refritl5_prop_inv = 1-refritl5_prop) %>% 
  mutate(Indicador = rowMeans(select(., IMC_i_cat_excesso_prop_inv, flvreg_prop, flvreco_prop, 
                                     refritl5_prop_inv, feijao5_prop, hart_prop, diab_prop), na.rm = TRUE))

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
              "População","Leitos.SUS","Taxa.Cob.Planos.Priv","Cobertura.ESF","IVS","IDHM","Gini")

#cruadia_cat_prop, cozidadia_cat_prop e sucodia_media são compostos por 0's ou 1's
Tabela1 = 
  do.call(rbind,dados %>% select(sexo_M_prop,idade_60a79_prop,civil_uniaoest_casado_prop,anos_de_estudo,plano_saude_nao_prop,IMC_media,
                                 IMC_cat_baixo_prop,IMC_cat_excesso_prop,IMC_i_media,IMC_i_cat_baixo_prop,IMC_i_cat_excesso_prop,
                                 hortareg_prop,frutareg_prop,flvreg_prop,hortadia_media,
                                 sofrutadia_media,frutadia_media,flvdia_media,flvreco_prop,refritl5_prop,feijao5_prop,
                                 hart_prop,diab_prop,POP,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO,
                                 SomaICSAP,TaxaICSAP,Nota,População,Leitos.SUS,Taxa.Cob.Planos.Priv,Cobertura.ESF,IVS,IDHM,Gini) %>% 
            map(TesteDeNormalidade))
#write.xlsx(Tabela1 %>% as.data.frame(), 'Tabela 1.xlsx')

#Tabela1 = lapply(variaveis, function(var) {TesteDeNormalidadeGrupos(dados[[var]], dados$ano)}) %>% bind_rows()
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

####====================
#### Análise descritiva
####====================
# Tabela1Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_CO[[variavel]], dados_CO$ano)}))
# Tabela2Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_ND[[variavel]], dados_ND$ano)}))
# Tabela3Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_NT[[variavel]], dados_NT$ano)}))
# Tabela4Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_Sud[[variavel]], dados_Sud$ano)}))
# Tabela5Desc = do.call(rbind,lapply(variaveis, function(variavel) {DescritivaNumMais2Grupos(dados_Sul[[variavel]], dados_Sul$ano)}))
 
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
# Tabela2.2 = FriedmanTeste(dados_CO$feijao5_prop, dados_CO$ano, dados_CO$cidade_nome)$C.Multiplas
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
# 
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
# 
# write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)
# write.xlsx(Tabela3.1 %>% as.data.frame(), 'Tabela 3.1.xlsx', rowNames = T)
# write.xlsx(Tabela3.2 %>% as.data.frame(), 'Tabela 3.2.xlsx', rowNames = T)
# write.xlsx(Tabela3.3 %>% as.data.frame(), 'Tabela 3.3.xlsx', rowNames = T)
# 
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
# 
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
# #Tabela4.3 = FriedmanTeste(dados_NT_inc$feijao5_prop, dados_NT_inc$ano, dados_NT_inc$cidade_nome)$C.Multiplas
# 
# write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)
# write.xlsx(Tabela4.1 %>% as.data.frame(), 'Tabela 4.1.xlsx', rowNames = T)
# write.xlsx(Tabela4.2 %>% as.data.frame(), 'Tabela 4.2.xlsx', rowNames = T)
# 
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
# 
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
# 
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
# 
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
# 
# write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)
# write.xlsx(Tabela6.1 %>% as.data.frame(), 'Tabela 6.1.xlsx', rowNames = T)
# write.xlsx(Tabela6.2 %>% as.data.frame(), 'Tabela 6.2.xlsx', rowNames = T)

####=============
#### Correlações
####=============
dados_corr = 
  dados %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                   feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                   anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix
Tabela7 = cbind(Hmisc::rcorr(dados_corr, type = 'spearman')$r, Hmisc::rcorr(dados_corr, type = 'spearman')$P)
#write.xlsx(Tabela7 %>% as.data.frame(),'Tabela 7.xlsx', rowNames = T)

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

dados_corr2010 = 
  dados2010 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2011 = 
  dados2011 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2012 = 
  dados2012 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

Tabela8 = rbind(cbind(Hmisc::rcorr(dados_corr2010, type = 'spearman')$r, Hmisc::rcorr(dados_corr2010, type = 'spearman')$P),
                cbind(Hmisc::rcorr(dados_corr2011, type = 'spearman')$r, Hmisc::rcorr(dados_corr2011, type = 'spearman')$P),
                cbind(Hmisc::rcorr(dados_corr2012, type = 'spearman')$r, Hmisc::rcorr(dados_corr2012, type = 'spearman')$P))
#write.xlsx(Tabela8 %>% as.data.frame(),'Tabela 8.xlsx', rowNames = T)

dados_corr2013 = 
  dados2013 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2014 = 
  dados2014 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2015 = 
  dados2015 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

Tabela9 = rbind(cbind(Hmisc::rcorr(dados_corr2013, type = 'spearman')$r, Hmisc::rcorr(dados_corr2013, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2014, type = 'spearman')$r, Hmisc::rcorr(dados_corr2014, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2015, type = 'spearman')$r, Hmisc::rcorr(dados_corr2015, type = 'spearman')$P))
#write.xlsx(Tabela9 %>% as.data.frame(),'Tabela 9.xlsx', rowNames = T)

dados_corr2016 = 
  dados2016 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2017 = 
  dados2017 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2018 = 
  dados2018 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

dados_corr2019 = 
  dados2019 %>% select(Indicador,IMC_i_cat_excesso_prop,flvreg_prop,flvreco_prop,refritl5_prop,
                       feijao5_prop,hart_prop,diab_prop,sexo_M_prop,idade_60a79_prop,
                       anos_de_estudo,plano_saude_nao_prop,TaxaICSAP,Nota,IVS,IDHM,Gini) %>% as.matrix

Tabela10 = rbind(cbind(Hmisc::rcorr(dados_corr2016, type = 'spearman')$r, Hmisc::rcorr(dados_corr2016, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2017, type = 'spearman')$r, Hmisc::rcorr(dados_corr2017, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2018, type = 'spearman')$r, Hmisc::rcorr(dados_corr2018, type = 'spearman')$P),
                 cbind(Hmisc::rcorr(dados_corr2019, type = 'spearman')$r, Hmisc::rcorr(dados_corr2019, type = 'spearman')$P))
#write.xlsx(Tabela10 %>% as.data.frame(),'Tabela 10.xlsx', rowNames = T)

####=========
#### Modelos
####=========
#Respostas: IMC_i_cat_excesso_prop, flvreg_prop, flvreco_prop, refritl5_prop, feijao5_prop, hart_prop, diab_prop e TaxaICSAP
#Explicativas: sexo, faixa etária, anos de estudo, plano de saúde, Nota (IVS, IDH, Gini)
# multi_imc1 = glmmTMB(Indicador ~ sexo_M_prop*Nota + idade_60a79_prop*Nota + anos_de_estudo*Nota + plano_saude_nao_prop*Nota + 
#                        Nota*IDHM + factor(ano) +
#                        (1 | cidade_nome), data = dados %>% 
#                        select(ano,Indicador,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade_nome) %>% na.omit(), 
#                      family = beta_family(link = "logit"))
# summary(multi_imc1)
#IMC_i_cat_excesso_prop ~ sexo_M_prop*Nota + idade_60a79_prop*Nota + anos_de_estudo*Nota + plano_saude_nao_prop*Nota + Nota*IDHM + factor(ano)
geeglm(IMC_i_cat_excesso_prop ~ sexo_M_prop*Nota + idade_60a79_prop*Nota + anos_de_estudo*Nota + plano_saude_nao_prop*Nota + factor(ano), 
       id = cidade, data = dados %>% 
         select(IMC_i_cat_excesso_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(), 
       family = gaussian(link = 'identity'), corstr = "exchangeable")

####=====
#### IMC
####=====
imc_bi1 = geeglm(IMC_i_cat_excesso_prop ~ sexo_M_prop, id = cidade, 
                    data = dados %>% select(IMC_i_cat_excesso_prop,sexo_M_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

imc_bi2 = geeglm(IMC_i_cat_excesso_prop ~ idade_60a79_prop, id = cidade, 
                 data = dados %>% select(IMC_i_cat_excesso_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                 family = gaussian(link = 'identity'), corstr = "exchangeable")

imc_bi3 = geeglm(IMC_i_cat_excesso_prop ~ anos_de_estudo, id = cidade, 
                 data = dados %>% select(IMC_i_cat_excesso_prop,anos_de_estudo,cidade) %>% na.omit(), 
                 family = gaussian(link = 'identity'), corstr = "exchangeable")

imc_bi4 = geeglm(IMC_i_cat_excesso_prop ~ plano_saude_nao_prop, id = cidade, 
                 data = dados %>% select(IMC_i_cat_excesso_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                 family = gaussian(link = 'identity'), corstr = "exchangeable")

imc_bi5 = geeglm(IMC_i_cat_excesso_prop ~ Nota, id = cidade, 
                 data = dados %>% select(IMC_i_cat_excesso_prop,Nota,cidade) %>% na.omit(), 
                 family = gaussian(link = 'identity'), corstr = "exchangeable")

imc_bi6 = geeglm(IMC_i_cat_excesso_prop ~ factor(ano), id = cidade, 
                 data = dados %>% select(IMC_i_cat_excesso_prop,ano,cidade) %>% na.omit(), 
                 family = gaussian(link = 'identity'), corstr = "exchangeable")

imc_multi = geeglm(IMC_i_cat_excesso_prop ~ sexo_M_prop + idade_60a79_prop + anos_de_estudo + plano_saude_nao_prop + 
                     factor(ano), 
                   id = cidade, data = dados %>% 
                     select(IMC_i_cat_excesso_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(imc_multi)

Tabela11.1 = rbind(TabelaGEENormal(imc_bi1),TabelaGEENormal(imc_bi2),TabelaGEENormal(imc_bi3),TabelaGEENormal(imc_bi4),
                   TabelaGEENormal(imc_bi5),TabelaGEENormal(imc_bi6))
Tabela11.2 = TabelaGEENormal(imc_multi)
#write.xlsx(Tabela11.1 %>% as.data.frame(), 'Tabela 11.1.xlsx', rowNames = F)
#write.xlsx(Tabela11.2 %>% as.data.frame(), 'Tabela 11.2.xlsx', rowNames = F)

####=============
#### flvreg_prop
####=============
flvreg_bi1 = geeglm(flvreg_prop ~ sexo_M_prop, id = cidade, 
                    data = dados %>% select(flvreg_prop,sexo_M_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreg_bi2 = geeglm(flvreg_prop ~ idade_60a79_prop, id = cidade, 
                    data = dados %>% select(flvreg_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreg_bi3 = geeglm(flvreg_prop ~ anos_de_estudo, id = cidade, 
                    data = dados %>% select(flvreg_prop,anos_de_estudo,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreg_bi4 = geeglm(flvreg_prop ~ plano_saude_nao_prop, id = cidade, 
                    data = dados %>% select(flvreg_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreg_bi5 = geeglm(flvreg_prop ~ Nota, id = cidade, 
                    data = dados %>% select(flvreg_prop,Nota,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreg_bi6 = geeglm(flvreg_prop ~ factor(ano), id = cidade, 
                    data = dados %>% select(flvreg_prop,ano,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreg_multi = geeglm(flvreg_prop ~ sexo_M_prop*Nota + idade_60a79_prop*Nota + anos_de_estudo + plano_saude_nao_prop + factor(ano), 
                      id = cidade, data = dados %>% 
                        select(flvreg_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(flvreg_multi)

Tabela12.1 = rbind(TabelaGEENormal(flvreg_bi1),TabelaGEENormal(flvreg_bi2),TabelaGEENormal(flvreg_bi3),TabelaGEENormal(flvreg_bi4),
                   TabelaGEENormal(flvreg_bi5),TabelaGEENormal(flvreg_bi6))
Tabela12.2 = TabelaGEENormal(flvreg_multi)
#write.xlsx(Tabela12.1 %>% as.data.frame(), 'Tabela 12.1.xlsx', rowNames = F)
#write.xlsx(Tabela12.2 %>% as.data.frame(), 'Tabela 12.2.xlsx', rowNames = F)

####==============
#### flvreco_prop
####==============
flvreco_bi1 = geeglm(flvreco_prop ~ sexo_M_prop, id = cidade, 
                    data = dados %>% select(flvreco_prop,sexo_M_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreco_bi2 = geeglm(flvreco_prop ~ idade_60a79_prop, id = cidade, 
                    data = dados %>% select(flvreco_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreco_bi3 = geeglm(flvreco_prop ~ anos_de_estudo, id = cidade, 
                    data = dados %>% select(flvreco_prop,anos_de_estudo,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreco_bi4 = geeglm(flvreco_prop ~ plano_saude_nao_prop, id = cidade, 
                    data = dados %>% select(flvreco_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreco_bi5 = geeglm(flvreco_prop ~ Nota, id = cidade, 
                    data = dados %>% select(flvreco_prop,Nota,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreco_bi6 = geeglm(flvreco_prop ~ factor(ano), id = cidade, 
                    data = dados %>% select(flvreco_prop,ano,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

flvreco_multi = geeglm(flvreco_prop ~ sexo_M_prop*Nota + anos_de_estudo + plano_saude_nao_prop + factor(ano), 
                       id = cidade, data = dados %>% 
                         select(flvreco_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(),
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(flvreco_multi)

Tabela13.1 = rbind(TabelaGEENormal(flvreco_bi1),TabelaGEENormal(flvreco_bi2),TabelaGEENormal(flvreco_bi3),TabelaGEENormal(flvreco_bi4),
                   TabelaGEENormal(flvreco_bi5),TabelaGEENormal(flvreco_bi6))
Tabela13.2 = TabelaGEENormal(flvreco_multi)
#write.xlsx(Tabela13.1 %>% as.data.frame(), 'Tabela 13.1.xlsx', rowNames = F)
#write.xlsx(Tabela13.2 %>% as.data.frame(), 'Tabela 13.2.xlsx', rowNames = F)

####===============
#### refritl5_prop
####===============
refri_bi1 = geeglm(refritl5_prop ~ sexo_M_prop, id = cidade, 
                   data = dados %>% select(refritl5_prop,sexo_M_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

refri_bi2 = geeglm(refritl5_prop ~ idade_60a79_prop, id = cidade, 
                   data = dados %>% select(refritl5_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

refri_bi3 = geeglm(refritl5_prop ~ anos_de_estudo, id = cidade, 
                   data = dados %>% select(refritl5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

refri_bi4 = geeglm(refritl5_prop ~ plano_saude_nao_prop, id = cidade, 
                   data = dados %>% select(refritl5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

refri_bi5 = geeglm(refritl5_prop ~ Nota, id = cidade, 
                   data = dados %>% select(refritl5_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

refri_bi6 = geeglm(refritl5_prop ~ factor(ano), id = cidade, 
                   data = dados %>% select(refritl5_prop,ano,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

refri_multi = geeglm(refritl5_prop ~ sexo_M_prop*Nota + idade_60a79_prop*Nota + anos_de_estudo*Nota + factor(ano), 
                     id = cidade, data = dados %>% 
                       select(refritl5_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(refri_multi)

Tabela14.1 = rbind(TabelaGEENormal(refri_bi1),TabelaGEENormal(refri_bi2),TabelaGEENormal(refri_bi3),TabelaGEENormal(refri_bi4),
                   TabelaGEENormal(refri_bi5),TabelaGEENormal(refri_bi6))
Tabela14.2 = TabelaGEENormal(refri_multi)
#write.xlsx(Tabela14.1 %>% as.data.frame(), 'Tabela 14.1.xlsx', rowNames = F)
#write.xlsx(Tabela14.2 %>% as.data.frame(), 'Tabela 14.2.xlsx', rowNames = F)

####==============
#### feijao5_prop
####==============
feijao_bi1 = geeglm(feijao5_prop ~ sexo_M_prop, id = cidade, 
                    data = dados %>% select(feijao5_prop,sexo_M_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

feijao_bi2 = geeglm(feijao5_prop ~ idade_60a79_prop, id = cidade, 
                    data = dados %>% select(feijao5_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

feijao_bi3 = geeglm(feijao5_prop ~ anos_de_estudo, id = cidade, 
                    data = dados %>% select(feijao5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

feijao_bi4 = geeglm(feijao5_prop ~ plano_saude_nao_prop, id = cidade, 
                    data = dados %>% select(feijao5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

feijao_bi5 = geeglm(feijao5_prop ~ Nota, id = cidade, 
                    data = dados %>% select(feijao5_prop,Nota,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

feijao_bi6 = geeglm(feijao5_prop ~ factor(ano), id = cidade, 
                    data = dados %>% select(feijao5_prop,ano,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")

feijao_multi = geeglm(feijao5_prop ~ sexo_M_prop*Nota + idade_60a79_prop*Nota + anos_de_estudo + plano_saude_nao_prop,
                      id = cidade, data = dados %>% 
                        select(feijao5_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(feijao_multi)

Tabela15.1 = rbind(TabelaGEENormal(feijao_bi1),TabelaGEENormal(feijao_bi2),TabelaGEENormal(feijao_bi3),TabelaGEENormal(feijao_bi4),
                   TabelaGEENormal(feijao_bi5),TabelaGEENormal(feijao_bi6))
Tabela15.2 = TabelaGEENormal(feijao_multi)
#write.xlsx(Tabela15.1 %>% as.data.frame(), 'Tabela 15.1.xlsx', rowNames = F)
#write.xlsx(Tabela15.2 %>% as.data.frame(), 'Tabela 15.2.xlsx', rowNames = F)

####===========
#### hart_prop
####===========
hart_bi1 = geeglm(hart_prop ~ sexo_M_prop, id = cidade, 
                  data = dados %>% select(hart_prop,sexo_M_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

hart_bi2 = geeglm(hart_prop ~ idade_60a79_prop, id = cidade, 
                  data = dados %>% select(hart_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

hart_bi3 = geeglm(hart_prop ~ anos_de_estudo, id = cidade, 
                  data = dados %>% select(hart_prop,anos_de_estudo,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

hart_bi4 = geeglm(hart_prop ~ plano_saude_nao_prop, id = cidade, 
                  data = dados %>% select(hart_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

hart_bi5 = geeglm(hart_prop ~ Nota, id = cidade, 
                  data = dados %>% select(hart_prop,Nota,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

hart_bi6 = geeglm(hart_prop ~ factor(ano), id = cidade, 
                  data = dados %>% select(hart_prop,ano,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

hart_multi = geeglm(hart_prop ~ sexo_M_prop*Nota + idade_60a79_prop + anos_de_estudo*Nota,
                    id = cidade, data = dados %>% 
                      select(hart_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(),  
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(hart_multi)

Tabela16.1 = rbind(TabelaGEENormal(hart_bi1),TabelaGEENormal(hart_bi2),TabelaGEENormal(hart_bi3),TabelaGEENormal(hart_bi4),
                   TabelaGEENormal(hart_bi5),TabelaGEENormal(hart_bi6))
Tabela16.2 = TabelaGEENormal(hart_multi)
#write.xlsx(Tabela16.1 %>% as.data.frame(), 'Tabela 16.1.xlsx', rowNames = F)
#write.xlsx(Tabela16.2 %>% as.data.frame(), 'Tabela 16.2.xlsx', rowNames = F)

####===========
#### hart_prop
####===========
diab_bi1 = geeglm(diab_prop ~ sexo_M_prop, id = cidade, 
                  data = dados %>% select(diab_prop,sexo_M_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

diab_bi2 = geeglm(diab_prop ~ idade_60a79_prop, id = cidade, 
                  data = dados %>% select(diab_prop,idade_60a79_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

diab_bi3 = geeglm(diab_prop ~ anos_de_estudo, id = cidade, 
                  data = dados %>% select(diab_prop,anos_de_estudo,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

diab_bi4 = geeglm(diab_prop ~ plano_saude_nao_prop, id = cidade, 
                  data = dados %>% select(diab_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

diab_bi5 = geeglm(diab_prop ~ Nota, id = cidade, 
                  data = dados %>% select(diab_prop,Nota,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

diab_bi6 = geeglm(diab_prop ~ factor(ano), id = cidade, 
                  data = dados %>% select(diab_prop,ano,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

diab_multi = geeglm(diab_prop ~ idade_60a79_prop + anos_de_estudo + factor(ano), 
                    id = cidade, data = dados %>% 
                      select(diab_prop,ano,sexo_M_prop,idade_60a79_prop,anos_de_estudo,plano_saude_nao_prop,Nota,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(diab_multi)

Tabela17.1 = rbind(TabelaGEENormal(diab_bi1),TabelaGEENormal(diab_bi2),TabelaGEENormal(diab_bi3),TabelaGEENormal(diab_bi4),
                   TabelaGEENormal(diab_bi5),TabelaGEENormal(diab_bi6))
Tabela17.2 = TabelaGEENormal(diab_multi)
write.xlsx(Tabela17.1 %>% as.data.frame(), 'Tabela 17.1.xlsx', rowNames = F)
write.xlsx(Tabela17.2 %>% as.data.frame(), 'Tabela 17.2.xlsx', rowNames = F)

