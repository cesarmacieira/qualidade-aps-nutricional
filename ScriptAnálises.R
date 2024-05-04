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

####=============================
#### Carregando o banco de dados 
####=============================
#dados = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Catarina/Dados Catarina Vigitel ICSAP PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM 28-03-2024.xlsx", sheet = 1)
dados = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/qualidade-aps-nutricional/Dados Catarina Vigitel ICSAP Notas PMAQ POP Leitos Planos Priv ESF Gini IVS IDHM Porte Est Eq 09-04-2024.xlsx", sheet = 1)

####============================
#### Distribuição das variáveis
####============================
variaveis = c("idade_media","civil_uniaoest_casado_prop","anos_de_estudo","plano_saude_nao_prop",
              "IMC_media","IMC_cat_baixo_prop","IMC_cat_excesso_prop","IMC_i_media",
              "IMC_i_cat_baixo_prop","IMC_i_cat_excesso_prop","hortareg_prop","frutareg_prop",
              "flvreg_prop","hortadia_media","sofrutadia_media","frutadia_media",
              "flvdia_media","flvreco_prop","refritl5_prop","feijao5_prop",
              "hart_media","diab_prop","POP","ANEMIA","DEFICIENCIAS_NUTRICIONAIS",
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
                                 hart_media,diab_prop,POP,ANEMIA,DEFICIENCIAS_NUTRICIONAIS,DIABETES_MELITUS,HIPERTENSAO,
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
dados_CO = dados_20a79 %>% filter(Região == 'Centro-Oeste')

dados_ND = dados_20a79 %>% filter(Região == 'Nordeste')
dados_ND_inc = dados_20a79 %>% filter(Região == 'Nordeste') %>% filter(cidade == 'Maceió' | cidade == 'São Luís' | cidade == 'Teresina')
dados_ND_comp = dados_20a79 %>% filter(Região == 'Nordeste') %>% filter(cidade != 'Maceió' & cidade != 'São Luís' & cidade != 'Teresina')

dados_NT = dados_20a79 %>% filter(Região == 'Norte')
dados_NT_inc = dados_20a79 %>% filter(Região == 'Norte') %>% filter(cidade == 'Macapá')
dados_NT_comp = dados_20a79 %>% filter(Região == 'Norte') %>% filter(cidade != 'Macapá')

dados_Sud = dados_20a79 %>% filter(Região == 'Sudeste')
dados_Sul = dados_20a79 %>% filter(Região == 'Sul')

#0 a 19 anos
dados_0a19 = dados %>% filter(idade_cat == '0 a 4 anos' | idade_cat == '5 a 19 anos')

dados_CO_0a19 = dados_0a19 %>% filter(Região == 'Centro-Oeste')
dados_ND_0a19 = dados_0a19 %>% filter(Região == 'Nordeste')
dados_ND_inc_0a19 = dados_0a19 %>% filter(Região == 'Nordeste') %>% filter(cidade == 'Maceió' | cidade == 'São Luís' | cidade == 'Teresina')
dados_ND_comp_0a19 = dados_0a19 %>% filter(Região == 'Nordeste') %>% filter(cidade != 'Maceió' & cidade != 'São Luís' & cidade != 'Teresina')

dados_NT_0a19 = dados_0a19 %>% filter(Região == 'Norte')
dados_NT_inc_0a19 = dados_0a19 %>% filter(Região == 'Norte') %>% filter(cidade == 'Macapá')
dados_NT_comp_0a19 = dados_0a19 %>% filter(Região == 'Norte') %>% filter(cidade != 'Macapá')

dados_Sud_0a19 = dados_0a19 %>% filter(Região == 'Sudeste')
dados_Sul_0a19 = dados_0a19 %>% filter(Região == 'Sul')

#80 anos ou mais
dados_80mais = dados %>% filter(idade_cat == '80 anos ou mais')
dados_CO_80mais = dados_80mais %>% filter(Região == 'Centro-Oeste')
dados_ND_80mais = dados_80mais %>% filter(Região == 'Nordeste')
dados_ND_inc_80mais = dados_80mais %>% filter(Região == 'Nordeste') %>% filter(cidade == 'Maceió' | cidade == 'São Luís' | cidade == 'Teresina')
dados_ND_comp_80mais = dados_80mais %>% filter(Região == 'Nordeste') %>% filter(cidade != 'Maceió' & cidade != 'São Luís' & cidade != 'Teresina')

dados_NT_80mais = dados_80mais %>% filter(Região == 'Norte')
dados_NT_inc_80mais = dados_80mais %>% filter(Região == 'Norte') %>% filter(cidade == 'Macapá')
dados_NT_comp_80mais = dados_80mais %>% filter(Região == 'Norte') %>% filter(cidade != 'Macapá')

dados_Sud_80mais = dados_80mais %>% filter(Região == 'Sudeste')
dados_Sul_80mais = dados_80mais %>% filter(Região == 'Sul')

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
Tabela2 = rbind(#FriedmanTeste(dados_CO$idade_media, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$civil_uniaoest_casado_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$anos_de_estudo, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$plano_saude_nao_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$IMC_media, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$IMC_cat_baixo_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$IMC_cat_excesso_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$IMC_i_media, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$IMC_i_cat_baixo_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$IMC_i_cat_excesso_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$hortareg_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$frutareg_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$flvreg_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$hortadia_media, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$sofrutadia_media, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$frutadia_media, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$flvdia_media, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$flvreco_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$refritl5_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$feijao5_prop, dados_CO$ano, dados_CO$cidade)$tabela,#
  FriedmanTeste(dados_CO$hart_media, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$diab_prop, dados_CO$ano, dados_CO$cidade)$tabela,
  #FriedmanTeste(dados_CO$POP, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$ANEMIA, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$DEFICIENCIAS_NUTRICIONAIS, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$DIABETES_MELITUS, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$HIPERTENSAO, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$SomaICSAP, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$TaxaICSAP, dados_CO$ano, dados_CO$cidade)$tabela,
  FriedmanTeste(dados_CO$Nota, dados_CO$ano, dados_CO$cidade)$tabela)
# FriedmanTeste(dados_CO$População, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Leitos.SUS, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Taxa.Cob.Planos.Priv, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Cobertura.ESF, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$IVS, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$IDHM, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Gini, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q1_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q2_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q3_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q4_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q5_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q6_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q7_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q8_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q9_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q10_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q11_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q12_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q13_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q14_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q15_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q16_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q17_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q18_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q19_media, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q20_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q21_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q22_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q23_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q24_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q25_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q26_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q27_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q28_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q29_prop, dados_CO$ano, dados_CO$cidade)$tabela,
# FriedmanTeste(dados_CO$Q30_prop, dados_CO$ano, dados_CO$cidade)$tabela)

Tabela2.1 = rbind(#FriedmanTeste(dados_CO$idade_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$civil_uniaoest_casado_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$anos_de_estudo, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$plano_saude_nao_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$IMC_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$IMC_cat_baixo_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$IMC_cat_excesso_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$IMC_i_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$IMC_i_cat_baixo_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$IMC_i_cat_excesso_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$hortareg_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$frutareg_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$flvreg_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$hortadia_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$sofrutadia_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$frutadia_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$flvdia_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$flvreco_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$refritl5_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$feijao5_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,#
                  FriedmanTeste(dados_CO$hart_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$diab_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_CO$POP, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$ANEMIA, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$DEFICIENCIAS_NUTRICIONAIS, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$DIABETES_MELITUS, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$HIPERTENSAO, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$SomaICSAP, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$TaxaICSAP, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO$Nota, dados_CO$ano, dados_CO$cidade)$C.Multiplas)
                  # FriedmanTeste(dados_CO$População, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Leitos.SUS, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Taxa.Cob.Planos.Priv, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Cobertura.ESF, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$IVS, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$IDHM, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Gini, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q1_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q2_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q3_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q4_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q5_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q6_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q7_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q8_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q9_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q10_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q11_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q12_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q13_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q14_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q15_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q16_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q17_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q18_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q19_media, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q20_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q21_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q22_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q23_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q24_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q25_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q26_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q27_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q28_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q29_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas,
                  # FriedmanTeste(dados_CO$Q30_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas)
Tabela2.2 = FriedmanTeste(dados_CO$feijao5_prop, dados_CO$ano, dados_CO$cidade)$C.Multiplas
# Tabela2 = do.call(rbind,lapply(variaveis, function(variavel) {FriedmanTeste(dados_CO[[variavel]], dados_CO$ano, dados_CO$cidade)$tabela}))
# Tabela2.1 = do.call(rbind,lapply(variaveis, function(variavel) {FriedmanTeste(dados_CO[[variavel]], dados_CO$ano, dados_CO$cidade)$C.Multiplas}))
# write.xlsx(Tabela2 %>% as.data.frame(), 'Tabela 2.xlsx', rowNames = T)
# write.xlsx(Tabela2.1 %>% as.data.frame(), 'Tabela 2.1.xlsx', rowNames = T)
# write.xlsx(Tabela2.2 %>% as.data.frame(), 'Tabela 2.2.xlsx', rowNames = T)

Tabela3 = rbind(FriedmanTeste(dados_ND$IMC_media, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$IMC_cat_baixo_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$IMC_cat_excesso_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$IMC_i_media, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$IMC_i_cat_baixo_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$IMC_i_cat_excesso_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$flvreg_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$flvdia_media, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$flvreco_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$refritl5_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$feijao5_prop, dados_ND$ano, dados_ND$cidade)$tabela,#
                FriedmanTeste(dados_ND$hart_media, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$diab_prop, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$ANEMIA, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$DEFICIENCIAS_NUTRICIONAIS, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$DIABETES_MELITUS, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$HIPERTENSAO, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$SomaICSAP, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND$TaxaICSAP, dados_ND$ano, dados_ND$cidade)$tabela,
                FriedmanTeste(dados_ND_comp$Nota, dados_ND_comp$ano, dados_ND_comp$cidade)$tabela,
                FriedmanTeste(dados_ND_inc$Nota, dados_ND_inc$ano, dados_ND_inc$cidade)$tabela)

Tabela3.1 = rbind(FriedmanTeste(dados_ND$IMC_media, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$IMC_cat_baixo_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$IMC_cat_excesso_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$IMC_i_media, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$IMC_i_cat_baixo_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$IMC_i_cat_excesso_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$flvreg_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$flvdia_media, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$flvreco_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$refritl5_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_ND$feijao5_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,#
                  FriedmanTeste(dados_ND$hart_media, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$diab_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$ANEMIA, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$DEFICIENCIAS_NUTRICIONAIS, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$DIABETES_MELITUS, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$HIPERTENSAO, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$SomaICSAP, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND$TaxaICSAP, dados_ND$ano, dados_ND$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_comp$Nota, dados_ND_comp$ano, dados_ND_comp$cidade)$C.Multiplas)
Tabela3.2 = FriedmanTeste(dados_ND_inc$Nota, dados_ND_inc$ano, dados_ND_inc$cidade)$C.Multiplas
Tabela3.3 = FriedmanTeste(dados_ND$feijao5_prop, dados_ND$ano, dados_ND$cidade)$C.Multiplas

# write.xlsx(Tabela3 %>% as.data.frame(), 'Tabela 3.xlsx', rowNames = T)
# write.xlsx(Tabela3.1 %>% as.data.frame(), 'Tabela 3.1.xlsx', rowNames = T)
# write.xlsx(Tabela3.2 %>% as.data.frame(), 'Tabela 3.2.xlsx', rowNames = T)
# write.xlsx(Tabela3.3 %>% as.data.frame(), 'Tabela 3.3.xlsx', rowNames = T)

Tabela4 = rbind(FriedmanTeste(dados_NT$IMC_media, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$IMC_cat_baixo_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$IMC_cat_excesso_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$IMC_i_media, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$IMC_i_cat_baixo_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$IMC_i_cat_excesso_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$flvreg_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$flvdia_media, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$flvreco_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$refritl5_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$feijao5_prop, dados_NT$ano, dados_NT$cidade)$tabela,#
                FriedmanTeste(dados_NT$hart_media, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$diab_prop, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$ANEMIA, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$DEFICIENCIAS_NUTRICIONAIS, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$DIABETES_MELITUS, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$HIPERTENSAO, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$SomaICSAP, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT$TaxaICSAP, dados_NT$ano, dados_NT$cidade)$tabela,
                FriedmanTeste(dados_NT_comp$Nota, dados_NT_comp$ano, dados_NT_comp$cidade)$tabela)
                #FriedmanTeste(dados_NT_inc$Nota, dados_NT_inc$ano, dados_NT_inc$cidade)$tabela)

Tabela4.1 = rbind(FriedmanTeste(dados_NT$IMC_media, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$IMC_cat_baixo_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$IMC_cat_excesso_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$IMC_i_media, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$IMC_i_cat_baixo_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$IMC_i_cat_excesso_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$flvreg_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$flvdia_media, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$flvreco_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$refritl5_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_NT$feijao5_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,#
                  FriedmanTeste(dados_NT$hart_media, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$diab_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$ANEMIA, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$DEFICIENCIAS_NUTRICIONAIS, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$DIABETES_MELITUS, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$HIPERTENSAO, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$SomaICSAP, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT$TaxaICSAP, dados_NT$ano, dados_NT$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_comp$Nota, dados_NT_comp$ano, dados_NT_comp$cidade)$C.Multiplas)
Tabela4.2 = FriedmanTeste(dados_NT$feijao5_prop, dados_NT$ano, dados_NT$cidade)$C.Multiplas
#Tabela4.3 = FriedmanTeste(dados_NT_inc$feijao5_prop, dados_NT_inc$ano, dados_NT_inc$cidade)$C.Multiplas

# write.xlsx(Tabela4 %>% as.data.frame(), 'Tabela 4.xlsx', rowNames = T)
# write.xlsx(Tabela4.1 %>% as.data.frame(), 'Tabela 4.1.xlsx', rowNames = T)
# write.xlsx(Tabela4.2 %>% as.data.frame(), 'Tabela 4.2.xlsx', rowNames = T)

Tabela5 = rbind(FriedmanTeste(dados_Sud$IMC_media, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$IMC_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$IMC_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$IMC_i_media, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$IMC_i_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$IMC_i_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$flvreg_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$flvdia_media, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$flvreco_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$refritl5_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$feijao5_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,#
                FriedmanTeste(dados_Sud$hart_media, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$diab_prop, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$ANEMIA, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$DEFICIENCIAS_NUTRICIONAIS, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$DIABETES_MELITUS, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$HIPERTENSAO, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$SomaICSAP, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$TaxaICSAP, dados_Sud$ano, dados_Sud$cidade)$tabela,
                FriedmanTeste(dados_Sud$Nota, dados_Sud$ano, dados_Sud$cidade)$tabela)

Tabela5.1 = rbind(FriedmanTeste(dados_Sud$IMC_media, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$IMC_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$IMC_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$IMC_i_media, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$IMC_i_cat_baixo_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$IMC_i_cat_excesso_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$flvreg_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$flvdia_media, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$flvreco_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$refritl5_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_Sud$feijao5_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,#
                  FriedmanTeste(dados_Sud$hart_media, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$diab_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$ANEMIA, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$DEFICIENCIAS_NUTRICIONAIS, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$DIABETES_MELITUS, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$HIPERTENSAO, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$SomaICSAP, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$TaxaICSAP, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sud$Nota, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas)
Tabela5.2 = FriedmanTeste(dados_Sud$feijao5_prop, dados_Sud$ano, dados_Sud$cidade)$C.Multiplas

# write.xlsx(Tabela5 %>% as.data.frame(), 'Tabela 5.xlsx', rowNames = T)
# write.xlsx(Tabela5.1 %>% as.data.frame(), 'Tabela 5.1.xlsx', rowNames = T)
# write.xlsx(Tabela5.2 %>% as.data.frame(), 'Tabela 5.2.xlsx', rowNames = T)

Tabela6 = rbind(FriedmanTeste(dados_Sul$IMC_media, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$IMC_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$IMC_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$IMC_i_media, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$IMC_i_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$IMC_i_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$flvreg_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$flvdia_media, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$flvreco_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$refritl5_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$feijao5_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,#
                FriedmanTeste(dados_Sul$hart_media, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$diab_prop, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$ANEMIA, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$DEFICIENCIAS_NUTRICIONAIS, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$DIABETES_MELITUS, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$HIPERTENSAO, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$SomaICSAP, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$TaxaICSAP, dados_Sul$ano, dados_Sul$cidade)$tabela,
                FriedmanTeste(dados_Sul$Nota, dados_Sul$ano, dados_Sul$cidade)$tabela)

Tabela6.1 = rbind(FriedmanTeste(dados_Sul$IMC_media, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$IMC_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$IMC_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$IMC_i_media, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$IMC_i_cat_baixo_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$IMC_i_cat_excesso_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$flvreg_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$flvdia_media, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$flvreco_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$refritl5_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  #FriedmanTeste(dados_Sul$feijao5_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,#
                  FriedmanTeste(dados_Sul$hart_media, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$diab_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$ANEMIA, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$DEFICIENCIAS_NUTRICIONAIS, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$DIABETES_MELITUS, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$HIPERTENSAO, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$SomaICSAP, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$TaxaICSAP, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas,
                  FriedmanTeste(dados_Sul$Nota, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas)
Tabela6.2 = FriedmanTeste(dados_Sul$feijao5_prop, dados_Sul$ano, dados_Sul$cidade)$C.Multiplas

# write.xlsx(Tabela6 %>% as.data.frame(), 'Tabela 6.xlsx', rowNames = T)
# write.xlsx(Tabela6.1 %>% as.data.frame(), 'Tabela 6.1.xlsx', rowNames = T)
# write.xlsx(Tabela6.2 %>% as.data.frame(), 'Tabela 6.2.xlsx', rowNames = T)

####===================================
#### Comparações por ano - 0 a 19 anos
####===================================
Tabela7 = rbind(FriedmanTeste(dados_CO_0a19$ANEMIA, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela,
                FriedmanTeste(dados_CO_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela,
                FriedmanTeste(dados_CO_0a19$DIABETES_MELITUS, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela,
                FriedmanTeste(dados_CO_0a19$HIPERTENSAO, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela,
                FriedmanTeste(dados_CO_0a19$SomaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela,
                FriedmanTeste(dados_CO_0a19$TaxaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela,
                FriedmanTeste(dados_CO_0a19$Nota, dados_CO_0a19$ano, dados_CO_0a19$cidade)$tabela)
Tabela7.1 = rbind(FriedmanTeste(dados_CO_0a19$ANEMIA, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO_0a19$DIABETES_MELITUS, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO_0a19$HIPERTENSAO, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO_0a19$SomaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO_0a19$TaxaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_CO_0a19$Nota, dados_CO_0a19$ano, dados_CO_0a19$cidade)$C.Multiplas)
# write.xlsx(Tabela7 %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)
# write.xlsx(Tabela7.1 %>% as.data.frame(), 'Tabela 7.1.xlsx', rowNames = T)

Tabela8 = rbind(FriedmanTeste(dados_ND_0a19$ANEMIA, dados_ND_0a19$ano, dados_ND_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_ND_0a19$ano, dados_ND_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_0a19$DIABETES_MELITUS, dados_ND_0a19$ano, dados_ND_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_0a19$HIPERTENSAO, dados_ND_0a19$ano, dados_ND_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_0a19$SomaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_0a19$TaxaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_comp_0a19$Nota, dados_ND_comp_0a19$ano, dados_ND_comp_0a19$cidade)$tabela,
                FriedmanTeste(dados_ND_inc_0a19$Nota, dados_ND_inc_0a19$ano, dados_ND_inc_0a19$cidade)$tabela)
Tabela8.1 = rbind(FriedmanTeste(dados_ND_0a19$ANEMIA, dados_ND_0a19$ano, dados_ND_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_ND_0a19$ano, dados_ND_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_0a19$DIABETES_MELITUS, dados_ND_0a19$ano, dados_ND_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_0a19$HIPERTENSAO, dados_ND_0a19$ano, dados_ND_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_0a19$SomaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_0a19$TaxaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_comp_0a19$Nota, dados_ND_comp_0a19$ano, dados_ND_comp_0a19$cidade)$C.Multiplas)
Tabela8.2 = FriedmanTeste(dados_ND_inc_0a19$Nota, dados_ND_inc_0a19$ano, dados_ND_inc_0a19$cidade)$C.Multiplas
# write.xlsx(Tabela8 %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)
# write.xlsx(Tabela8.1 %>% as.data.frame(), 'Tabela 8.1.xlsx', rowNames = T)
# write.xlsx(Tabela8.2 %>% as.data.frame(), 'Tabela 8.2.xlsx', rowNames = T)

Tabela9 = rbind(FriedmanTeste(dados_NT_0a19$ANEMIA, dados_NT_0a19$ano, dados_NT_0a19$cidade)$tabela,
                FriedmanTeste(dados_NT_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_NT_0a19$ano, dados_NT_0a19$cidade)$tabela,
                FriedmanTeste(dados_NT_0a19$DIABETES_MELITUS, dados_NT_0a19$ano, dados_NT_0a19$cidade)$tabela,
                FriedmanTeste(dados_NT_0a19$HIPERTENSAO, dados_NT_0a19$ano, dados_NT_0a19$cidade)$tabela,
                FriedmanTeste(dados_NT_0a19$SomaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade)$tabela,
                FriedmanTeste(dados_NT_0a19$TaxaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade)$tabela,
                FriedmanTeste(dados_NT_comp_0a19$Nota, dados_NT_comp_0a19$ano, dados_NT_comp_0a19$cidade)$tabela)
Tabela9.1 = rbind(FriedmanTeste(dados_NT_0a19$ANEMIA, dados_NT_0a19$ano, dados_NT_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_NT_0a19$ano, dados_NT_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_0a19$DIABETES_MELITUS, dados_NT_0a19$ano, dados_NT_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_0a19$HIPERTENSAO, dados_NT_0a19$ano, dados_NT_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_0a19$SomaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_0a19$TaxaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade)$C.Multiplas,
                  FriedmanTeste(dados_NT_comp_0a19$Nota, dados_NT_comp_0a19$ano, dados_NT_comp_0a19$cidade)$C.Multiplas)
# write.xlsx(Tabela9 %>% as.data.frame(), 'Tabela 9.xlsx', rowNames = T)
# write.xlsx(Tabela9.1 %>% as.data.frame(), 'Tabela 9.1.xlsx', rowNames = T)

Tabela10 = rbind(FriedmanTeste(dados_Sud_0a19$ANEMIA, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sud_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sud_0a19$DIABETES_MELITUS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sud_0a19$HIPERTENSAO, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sud_0a19$SomaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sud_0a19$TaxaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sud_0a19$Nota, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$tabela)
Tabela10.1 = rbind(FriedmanTeste(dados_Sud_0a19$ANEMIA, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_0a19$DIABETES_MELITUS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_0a19$HIPERTENSAO, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_0a19$SomaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_0a19$TaxaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_0a19$Nota, dados_Sud_0a19$ano, dados_Sud_0a19$cidade)$C.Multiplas)
# write.xlsx(Tabela10 %>% as.data.frame(), 'Tabela 10.xlsx', rowNames = T)
# write.xlsx(Tabela10.1 %>% as.data.frame(), 'Tabela 10.1.xlsx', rowNames = T)

Tabela11 = rbind(FriedmanTeste(dados_Sul_0a19$ANEMIA, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sul_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sul_0a19$DIABETES_MELITUS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sul_0a19$HIPERTENSAO, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sul_0a19$SomaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sul_0a19$TaxaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela,
                 FriedmanTeste(dados_Sul_0a19$Nota, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$tabela)
Tabela11.1 = rbind(FriedmanTeste(dados_Sul_0a19$ANEMIA, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_0a19$DIABETES_MELITUS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_0a19$HIPERTENSAO, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_0a19$SomaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_0a19$TaxaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_0a19$Nota, dados_Sul_0a19$ano, dados_Sul_0a19$cidade)$C.Multiplas)
# write.xlsx(Tabela11 %>% as.data.frame(), 'Tabela 11.xlsx', rowNames = T)
# write.xlsx(Tabela11.1 %>% as.data.frame(), 'Tabela 11.1.xlsx', rowNames = T)

####====================================
#### Comparações por ano - 20 a 79 anos
####====================================
Tabela12 = rbind(FriedmanTeste(dados_CO_80mais$IMC_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$IMC_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$IMC_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$IMC_i_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$IMC_i_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$IMC_i_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$flvreg_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$flvdia_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$flvreco_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$refritl5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$feijao5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,#
                 FriedmanTeste(dados_CO_80mais$hart_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$diab_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela,
                 FriedmanTeste(dados_CO_80mais$Nota, dados_CO_80mais$ano, dados_CO_80mais$cidade)$tabela)
Tabela12.1 = rbind(FriedmanTeste(dados_CO_80mais$IMC_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$IMC_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$IMC_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$IMC_i_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$IMC_i_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$IMC_i_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$flvreg_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$flvdia_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$flvreco_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$refritl5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$hart_media, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$diab_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_CO_80mais$Nota, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas)
Tabela12.2 = FriedmanTeste(dados_CO_80mais$feijao5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade)$C.Multiplas
# write.xlsx(Tabela12 %>% as.data.frame(), 'Tabela 12.xlsx', rowNames = T)
# write.xlsx(Tabela12.1 %>% as.data.frame(), 'Tabela 12.1.xlsx', rowNames = T)
# write.xlsx(Tabela12.2 %>% as.data.frame(), 'Tabela 12.2.xlsx', rowNames = T)

Tabela13 = rbind(FriedmanTeste(dados_ND_80mais$IMC_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$IMC_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$IMC_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$IMC_i_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$IMC_i_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$IMC_i_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$flvreg_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$flvdia_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$flvreco_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$refritl5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$feijao5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,#
                FriedmanTeste(dados_ND_80mais$hart_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_80mais$diab_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_comp_80mais$Nota, dados_ND_comp_80mais$ano, dados_ND_comp_80mais$cidade)$tabela,
                FriedmanTeste(dados_ND_inc_80mais$Nota, dados_ND_inc_80mais$ano, dados_ND_inc_80mais$cidade)$tabela)
Tabela13.1 = rbind(FriedmanTeste(dados_ND_80mais$IMC_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$IMC_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$IMC_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$IMC_i_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$IMC_i_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$IMC_i_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$flvreg_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$flvdia_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$flvreco_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$refritl5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$hart_media, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_80mais$diab_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas,
                  FriedmanTeste(dados_ND_comp_80mais$Nota, dados_ND_comp_80mais$ano, dados_ND_comp_80mais$cidade)$C.Multiplas)
Tabela13.2 = FriedmanTeste(dados_ND_inc_80mais$Nota, dados_ND_inc_80mais$ano, dados_ND_inc_80mais$cidade)$C.Multiplas
Tabela13.3 = FriedmanTeste(dados_ND_80mais$feijao5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade)$C.Multiplas
# write.xlsx(Tabela13 %>% as.data.frame(), 'Tabela 13.xlsx', rowNames = T)
# write.xlsx(Tabela13.1 %>% as.data.frame(), 'Tabela 13.1.xlsx', rowNames = T)
# write.xlsx(Tabela13.2 %>% as.data.frame(), 'Tabela 13.2.xlsx', rowNames = T)
# write.xlsx(Tabela13.3 %>% as.data.frame(), 'Tabela 13.3.xlsx', rowNames = T)

Tabela14 = rbind(FriedmanTeste(dados_NT_80mais$IMC_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$IMC_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$IMC_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$IMC_i_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$IMC_i_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$IMC_i_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$flvreg_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$flvdia_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$flvreco_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$refritl5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$feijao5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,#
                 FriedmanTeste(dados_NT_80mais$hart_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_80mais$diab_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$tabela,
                 FriedmanTeste(dados_NT_comp_80mais$Nota, dados_NT_comp_80mais$ano, dados_NT_comp_80mais$cidade)$tabela)
Tabela14.1 = rbind(FriedmanTeste(dados_NT_80mais$IMC_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$IMC_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$IMC_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$IMC_i_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$IMC_i_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$IMC_i_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$flvreg_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$flvdia_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$flvreco_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$refritl5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$hart_media, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_80mais$diab_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_NT_comp_80mais$Nota, dados_NT_comp_80mais$ano, dados_NT_comp_80mais$cidade)$C.Multiplas)
Tabela14.2 = FriedmanTeste(dados_NT_80mais$feijao5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade)$C.Multiplas
# write.xlsx(Tabela14 %>% as.data.frame(), 'Tabela 14.xlsx', rowNames = T)
# write.xlsx(Tabela14.1 %>% as.data.frame(), 'Tabela 14.1.xlsx', rowNames = T)
# write.xlsx(Tabela14.2 %>% as.data.frame(), 'Tabela 14.2.xlsx', rowNames = T)

Tabela15 = rbind(FriedmanTeste(dados_Sud_80mais$IMC_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$IMC_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$IMC_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$IMC_i_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$IMC_i_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$IMC_i_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$flvreg_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$flvdia_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$flvreco_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$refritl5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$feijao5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,#
                 FriedmanTeste(dados_Sud_80mais$hart_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$diab_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sud_80mais$Nota, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$tabela)
Tabela15.1 = rbind(FriedmanTeste(dados_Sud_80mais$IMC_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$IMC_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$IMC_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$IMC_i_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$IMC_i_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$IMC_i_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$flvreg_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$flvdia_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$flvreco_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$refritl5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$hart_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$diab_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sud_80mais$Nota, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas)
Tabela15.2 = FriedmanTeste(dados_Sud_80mais$feijao5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade)$C.Multiplas
# write.xlsx(Tabela15 %>% as.data.frame(), 'Tabela 15.xlsx', rowNames = T)
# write.xlsx(Tabela15.1 %>% as.data.frame(), 'Tabela 15.1.xlsx', rowNames = T)
# write.xlsx(Tabela15.2 %>% as.data.frame(), 'Tabela 15.2.xlsx', rowNames = T)

Tabela16 = rbind(FriedmanTeste(dados_Sul_80mais$IMC_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$IMC_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$IMC_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$IMC_i_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$IMC_i_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$IMC_i_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$flvreg_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$flvdia_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$flvreco_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$refritl5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$feijao5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,#
                 FriedmanTeste(dados_Sul_80mais$hart_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$diab_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela,
                 FriedmanTeste(dados_Sul_80mais$Nota, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$tabela)
Tabela16.1 = rbind(FriedmanTeste(dados_Sul_80mais$IMC_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$IMC_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$IMC_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$IMC_i_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$IMC_i_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$IMC_i_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$flvreg_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$flvdia_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$flvreco_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$refritl5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$hart_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$diab_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas,
                   FriedmanTeste(dados_Sul_80mais$Nota, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas)
Tabela16.2 = FriedmanTeste(dados_Sul_80mais$feijao5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade)$C.Multiplas
# write.xlsx(Tabela16 %>% as.data.frame(), 'Tabela 16.xlsx', rowNames = T)
# write.xlsx(Tabela16.1 %>% as.data.frame(), 'Tabela 16.1.xlsx', rowNames = T)
# write.xlsx(Tabela16.2 %>% as.data.frame(), 'Tabela 16.2.xlsx', rowNames = T)


#######################################

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