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

diag_gee_gama <- function(modelo, dados){
  X <- model.matrix(as.formula(paste("~ ", modelo$call$formula[3])), dados) 
  y <- modelo$y
  beta <- coef(modelo)
  R <- modelo$work
  mi <- fitted(modelo)
  individuo <- modelo$id
  
  repet <- dim(modelo$work)[1]
  ue <- modelo$nobs/repet
  
  N <- nrow(X)
  p <- ncol(X)
  
  #Calculo do Residuo de Pearson
  r<-(y-mi)*(1/sqrt(mi^2))
  
  #Calculo de phi
  invphi<-as.numeric(sum(r^2)/(N-p))
  phi<-1/invphi
  
  #Matriz C <- A * Delta  #Ligação canonica -> Delta=Identidade
  A <- diag(mi^2,N)
  Delta <- diag(1/mi,N)
  C <- Delta%*%A
  
  #Matriz Omega - variancia e covariancia de y
  Omega <- matrix(0,N,N)
  invOmega <- matrix(0,N,N)
  l <- 1
  while (l<N)
  {
    Omega[l:(l+repet-1),l:(l+repet-1)] <- 
      sqrt(A[l:(l+repet-1),l:(l+repet-1)])%*%R[1:repet,1:repet]%*%sqrt(A[l:(l+repet-1),l:(l+repet-1)])
    invOmega[l:(l+repet-1),l:(l+repet-1)] <-solve(Omega[l:(l+repet-1),l:(l+repet-1)])
    l <- l+repet
  }
  Omega <- invphi*Omega
  invOmega <- phi*invOmega
  
  #Matriz H e W
  W <- C%*%invOmega%*%C
  H <- solve(t(X)%*%W%*%X)
  raizW <- matrix(0,N,N)
  l <- 1
  while (l<N)
  {
    auto<-eigen(W[l:(l+repet-1),l:(l+repet-1)])
    raizW[l:(l+repet-1),l:(l+repet-1)] <- auto$vectors%*%sqrt(diag(auto$values,repet))%*%t(auto$vectors)
    l <- l+repet
  }
  H <- raizW%*%X%*%H%*%t(X)%*%raizW
  h <- diag(H)
  
  #Ponto Alavanca por UE
  hue<-as.vector(rep(0,ue))
  haux <- matrix(h,ue,repet,byrow=T)
  for (i in 1:ue)
    hue[i] <- sum(haux[i,])/repet
  
  #Residuo Padronizado
  rsd <- as.vector(rep(0,N))
  part.rsd <- raizW%*%solve(C)%*%(y-mi)
  for (l in 1:N)
  {
    e <- as.vector(rep(0,N))
    e[l] <- 1
    rsd[l] <- t(e)%*%part.rsd/sqrt(1-h[l])
  }	
  
  #Distancia de Cook
  cd <- as.vector(rep(0,N))
  for (l in 1:N)
  {
    cd[l] <- (rsd[l])^2*h[l]/((1-h[l])*p)
  }	
  return(cbind(h,cd,rsd))
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
# Tabela2.0 = rbind(FriedmanTeste(dados_20a79$IMC_media, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$IMC_cat_baixo_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$IMC_cat_excesso_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$IMC_i_media, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$IMC_i_cat_baixo_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$IMC_i_cat_excesso_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$flvreg_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$flvdia_media, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$flvreco_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$refritl5_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$feijao5_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,#
#                   FriedmanTeste(dados_20a79$hart_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$diab_prop, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$ANEMIA, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$DEFICIENCIAS_NUTRICIONAIS, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$DIABETES_MELITUS, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$HIPERTENSAO, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$SomaICSAP, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79$TaxaICSAP, dados_20a79$ano, dados_20a79$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79_comp$Nota, dados_20a79_comp$ano, dados_20a79_comp$cidade_nome)$tabela,
#                   FriedmanTeste(dados_20a79_inc$Nota, dados_20a79_inc$ano, dados_20a79_inc$cidade_nome)$tabela)

# Tabela2.0.1 = rbind(FriedmanTeste(dados_20a79$IMC_media, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$IMC_cat_baixo_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$IMC_cat_excesso_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$IMC_i_media, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$IMC_i_cat_baixo_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$IMC_i_cat_excesso_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$flvreg_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$flvdia_media, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$flvreco_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$refritl5_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$hart_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$diab_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$ANEMIA, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$DEFICIENCIAS_NUTRICIONAIS, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$DIABETES_MELITUS, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$HIPERTENSAO, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$SomaICSAP, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79$TaxaICSAP, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas,
#                     FriedmanTeste(dados_20a79_comp$Nota, dados_20a79_comp$ano, dados_20a79_comp$cidade_nome)$C.Multiplas)
# Tabela2.0.2 = FriedmanTeste(dados_20a79_inc$Nota, dados_20a79_inc$ano, dados_20a79_inc$cidade_nome)$C.Multiplas
# Tabela2.0.3 = FriedmanTeste(dados_20a79$feijao5_prop, dados_20a79$ano, dados_20a79$cidade_nome)$C.Multiplas
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

####===================================
#### Comparações por ano - 0 a 19 anos
####===================================
# Tabela7 = rbind(FriedmanTeste(dados_CO_0a19$ANEMIA, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO_0a19$DIABETES_MELITUS, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO_0a19$HIPERTENSAO, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO_0a19$SomaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO_0a19$TaxaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_CO_0a19$Nota, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$tabela)
# Tabela7.1 = rbind(FriedmanTeste(dados_CO_0a19$ANEMIA, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO_0a19$DIABETES_MELITUS, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO_0a19$HIPERTENSAO, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO_0a19$SomaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO_0a19$TaxaICSAP, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_CO_0a19$Nota, dados_CO_0a19$ano, dados_CO_0a19$cidade_nome)$C.Multiplas)
# write.xlsx(Tabela7 %>% as.data.frame(), 'Tabela 7.xlsx', rowNames = T)
# write.xlsx(Tabela7.1 %>% as.data.frame(), 'Tabela 7.1.xlsx', rowNames = T)

# Tabela8 = rbind(FriedmanTeste(dados_ND_0a19$ANEMIA, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_0a19$DIABETES_MELITUS, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_0a19$HIPERTENSAO, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_0a19$SomaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_0a19$TaxaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_comp_0a19$Nota, dados_ND_comp_0a19$ano, dados_ND_comp_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_inc_0a19$Nota, dados_ND_inc_0a19$ano, dados_ND_inc_0a19$cidade_nome)$tabela)
# Tabela8.1 = rbind(FriedmanTeste(dados_ND_0a19$ANEMIA, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_0a19$DIABETES_MELITUS, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_0a19$HIPERTENSAO, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_0a19$SomaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_0a19$TaxaICSAP, dados_ND_0a19$ano, dados_ND_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_comp_0a19$Nota, dados_ND_comp_0a19$ano, dados_ND_comp_0a19$cidade_nome)$C.Multiplas)
# Tabela8.2 = FriedmanTeste(dados_ND_inc_0a19$Nota, dados_ND_inc_0a19$ano, dados_ND_inc_0a19$cidade_nome)$C.Multiplas
# write.xlsx(Tabela8 %>% as.data.frame(), 'Tabela 8.xlsx', rowNames = T)
# write.xlsx(Tabela8.1 %>% as.data.frame(), 'Tabela 8.1.xlsx', rowNames = T)
# write.xlsx(Tabela8.2 %>% as.data.frame(), 'Tabela 8.2.xlsx', rowNames = T)

# Tabela9 = rbind(FriedmanTeste(dados_NT_0a19$ANEMIA, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_0a19$DIABETES_MELITUS, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_0a19$HIPERTENSAO, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_0a19$SomaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_0a19$TaxaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$tabela,
#                 FriedmanTeste(dados_NT_comp_0a19$Nota, dados_NT_comp_0a19$ano, dados_NT_comp_0a19$cidade_nome)$tabela)
# Tabela9.1 = rbind(FriedmanTeste(dados_NT_0a19$ANEMIA, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_0a19$DIABETES_MELITUS, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_0a19$HIPERTENSAO, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_0a19$SomaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_0a19$TaxaICSAP, dados_NT_0a19$ano, dados_NT_0a19$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_NT_comp_0a19$Nota, dados_NT_comp_0a19$ano, dados_NT_comp_0a19$cidade_nome)$C.Multiplas)
# write.xlsx(Tabela9 %>% as.data.frame(), 'Tabela 9.xlsx', rowNames = T)
# write.xlsx(Tabela9.1 %>% as.data.frame(), 'Tabela 9.1.xlsx', rowNames = T)

# Tabela10 = rbind(FriedmanTeste(dados_Sud_0a19$ANEMIA, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_0a19$DIABETES_MELITUS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_0a19$HIPERTENSAO, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_0a19$SomaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_0a19$TaxaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_0a19$Nota, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$tabela)
# Tabela10.1 = rbind(FriedmanTeste(dados_Sud_0a19$ANEMIA, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_0a19$DIABETES_MELITUS, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_0a19$HIPERTENSAO, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_0a19$SomaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_0a19$TaxaICSAP, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_0a19$Nota, dados_Sud_0a19$ano, dados_Sud_0a19$cidade_nome)$C.Multiplas)
# write.xlsx(Tabela10 %>% as.data.frame(), 'Tabela 10.xlsx', rowNames = T)
# write.xlsx(Tabela10.1 %>% as.data.frame(), 'Tabela 10.1.xlsx', rowNames = T)

# Tabela11 = rbind(FriedmanTeste(dados_Sul_0a19$ANEMIA, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_0a19$DIABETES_MELITUS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_0a19$HIPERTENSAO, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_0a19$SomaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_0a19$TaxaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_0a19$Nota, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$tabela)
# Tabela11.1 = rbind(FriedmanTeste(dados_Sul_0a19$ANEMIA, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_0a19$DEFICIENCIAS_NUTRICIONAIS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_0a19$DIABETES_MELITUS, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_0a19$HIPERTENSAO, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_0a19$SomaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_0a19$TaxaICSAP, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_0a19$Nota, dados_Sul_0a19$ano, dados_Sul_0a19$cidade_nome)$C.Multiplas)
# write.xlsx(Tabela11 %>% as.data.frame(), 'Tabela 11.xlsx', rowNames = T)
# write.xlsx(Tabela11.1 %>% as.data.frame(), 'Tabela 11.1.xlsx', rowNames = T)

####=======================================
#### Comparações por ano - 80 anos ou mais
####=======================================
# Tabela12 = rbind(FriedmanTeste(dados_CO_80mais$IMC_media, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$IMC_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$IMC_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$IMC_i_media, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$IMC_i_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$IMC_i_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$flvreg_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$flvdia_media, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$flvreco_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$refritl5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$feijao5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,#
#                  FriedmanTeste(dados_CO_80mais$hart_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$diab_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_CO_80mais$Nota, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$tabela)
# Tabela12.1 = rbind(FriedmanTeste(dados_CO_80mais$IMC_media, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$IMC_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$IMC_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$IMC_i_media, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$IMC_i_cat_baixo_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$IMC_i_cat_excesso_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$flvreg_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$flvdia_media, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$flvreco_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$refritl5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$hart_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$diab_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_CO_80mais$Nota, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas)
# Tabela12.2 = FriedmanTeste(dados_CO_80mais$feijao5_prop, dados_CO_80mais$ano, dados_CO_80mais$cidade_nome)$C.Multiplas
# write.xlsx(Tabela12 %>% as.data.frame(), 'Tabela 12.xlsx', rowNames = T)
# write.xlsx(Tabela12.1 %>% as.data.frame(), 'Tabela 12.1.xlsx', rowNames = T)
# write.xlsx(Tabela12.2 %>% as.data.frame(), 'Tabela 12.2.xlsx', rowNames = T)

# Tabela13 = rbind(FriedmanTeste(dados_ND_80mais$IMC_media, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$IMC_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$IMC_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$IMC_i_media, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$IMC_i_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$IMC_i_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$flvreg_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$flvdia_media, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$flvreco_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$refritl5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$feijao5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,#
#                 FriedmanTeste(dados_ND_80mais$hart_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_80mais$diab_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_comp_80mais$Nota, dados_ND_comp_80mais$ano, dados_ND_comp_80mais$cidade_nome)$tabela,
#                 FriedmanTeste(dados_ND_inc_80mais$Nota, dados_ND_inc_80mais$ano, dados_ND_inc_80mais$cidade_nome)$tabela)
# Tabela13.1 = rbind(FriedmanTeste(dados_ND_80mais$IMC_media, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$IMC_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$IMC_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$IMC_i_media, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$IMC_i_cat_baixo_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$IMC_i_cat_excesso_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$flvreg_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$flvdia_media, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$flvreco_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$refritl5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$hart_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_80mais$diab_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas,
#                   FriedmanTeste(dados_ND_comp_80mais$Nota, dados_ND_comp_80mais$ano, dados_ND_comp_80mais$cidade_nome)$C.Multiplas)
# Tabela13.2 = FriedmanTeste(dados_ND_inc_80mais$Nota, dados_ND_inc_80mais$ano, dados_ND_inc_80mais$cidade_nome)$C.Multiplas
# Tabela13.3 = FriedmanTeste(dados_ND_80mais$feijao5_prop, dados_ND_80mais$ano, dados_ND_80mais$cidade_nome)$C.Multiplas
# write.xlsx(Tabela13 %>% as.data.frame(), 'Tabela 13.xlsx', rowNames = T)
# write.xlsx(Tabela13.1 %>% as.data.frame(), 'Tabela 13.1.xlsx', rowNames = T)
# write.xlsx(Tabela13.2 %>% as.data.frame(), 'Tabela 13.2.xlsx', rowNames = T)
# write.xlsx(Tabela13.3 %>% as.data.frame(), 'Tabela 13.3.xlsx', rowNames = T)

# Tabela14 = rbind(FriedmanTeste(dados_NT_80mais$IMC_media, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$IMC_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$IMC_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$IMC_i_media, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$IMC_i_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$IMC_i_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$flvreg_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$flvdia_media, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$flvreco_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$refritl5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$feijao5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,#
#                  FriedmanTeste(dados_NT_80mais$hart_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_80mais$diab_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_NT_comp_80mais$Nota, dados_NT_comp_80mais$ano, dados_NT_comp_80mais$cidade_nome)$tabela)
# Tabela14.1 = rbind(FriedmanTeste(dados_NT_80mais$IMC_media, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$IMC_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$IMC_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$IMC_i_media, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$IMC_i_cat_baixo_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$IMC_i_cat_excesso_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$flvreg_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$flvdia_media, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$flvreco_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$refritl5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$hart_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_80mais$diab_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_NT_comp_80mais$Nota, dados_NT_comp_80mais$ano, dados_NT_comp_80mais$cidade_nome)$C.Multiplas)
# Tabela14.2 = FriedmanTeste(dados_NT_80mais$feijao5_prop, dados_NT_80mais$ano, dados_NT_80mais$cidade_nome)$C.Multiplas
# write.xlsx(Tabela14 %>% as.data.frame(), 'Tabela 14.xlsx', rowNames = T)
# write.xlsx(Tabela14.1 %>% as.data.frame(), 'Tabela 14.1.xlsx', rowNames = T)
# write.xlsx(Tabela14.2 %>% as.data.frame(), 'Tabela 14.2.xlsx', rowNames = T)

# Tabela15 = rbind(FriedmanTeste(dados_Sud_80mais$IMC_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$IMC_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$IMC_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$IMC_i_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$IMC_i_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$IMC_i_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$flvreg_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$flvdia_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$flvreco_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$refritl5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$feijao5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,#
#                  FriedmanTeste(dados_Sud_80mais$hart_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$diab_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sud_80mais$Nota, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$tabela)
# Tabela15.1 = rbind(FriedmanTeste(dados_Sud_80mais$IMC_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$IMC_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$IMC_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$IMC_i_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$IMC_i_cat_baixo_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$IMC_i_cat_excesso_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$flvreg_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$flvdia_media, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$flvreco_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$refritl5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$hart_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$diab_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sud_80mais$Nota, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas)
# Tabela15.2 = FriedmanTeste(dados_Sud_80mais$feijao5_prop, dados_Sud_80mais$ano, dados_Sud_80mais$cidade_nome)$C.Multiplas
# write.xlsx(Tabela15 %>% as.data.frame(), 'Tabela 15.xlsx', rowNames = T)
# write.xlsx(Tabela15.1 %>% as.data.frame(), 'Tabela 15.1.xlsx', rowNames = T)
# write.xlsx(Tabela15.2 %>% as.data.frame(), 'Tabela 15.2.xlsx', rowNames = T)

# Tabela16 = rbind(FriedmanTeste(dados_Sul_80mais$IMC_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$IMC_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$IMC_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$IMC_i_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$IMC_i_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$IMC_i_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$flvreg_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$flvdia_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$flvreco_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$refritl5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$feijao5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,#
#                  FriedmanTeste(dados_Sul_80mais$hart_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$diab_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela,
#                  FriedmanTeste(dados_Sul_80mais$Nota, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$tabela)
# Tabela16.1 = rbind(FriedmanTeste(dados_Sul_80mais$IMC_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$IMC_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$IMC_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$IMC_i_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$IMC_i_cat_baixo_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$IMC_i_cat_excesso_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$flvreg_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$flvdia_media, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$flvreco_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$refritl5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$hart_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$diab_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas,
#                    FriedmanTeste(dados_Sul_80mais$Nota, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas)
# Tabela16.2 = FriedmanTeste(dados_Sul_80mais$feijao5_prop, dados_Sul_80mais$ano, dados_Sul_80mais$cidade_nome)$C.Multiplas
# write.xlsx(Tabela16 %>% as.data.frame(), 'Tabela 16.xlsx', rowNames = T)
# write.xlsx(Tabela16.1 %>% as.data.frame(), 'Tabela 16.1.xlsx', rowNames = T)
# write.xlsx(Tabela16.2 %>% as.data.frame(), 'Tabela 16.2.xlsx', rowNames = T)

####=========
#### Modelos
####=========
#Respostas: IMC_i_cat_excesso_prop, flvreg_prop, flvreco_prop, refritl5_prop, feijao5_prop, hart_prop, diab_prop e TaxaICSAP
#Explicativas: sexo, faixa etária, anos de estudo, plano de saúde, Nota (IVS, IDH, Gini)

####========================
#### IMC_i_cat_excesso_prop
####========================
fit_IMC = lm(IMC_i_cat_excesso_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_IMC0 = subset(fit_IMC$residuals, dados_20a79$ano == 2010)
res_IMC1 = subset(fit_IMC$residuals, dados_20a79$ano == 2011)
res_IMC2 = subset(fit_IMC$residuals, dados_20a79$ano == 2012)
res_IMC3 = subset(fit_IMC$residuals, dados_20a79$ano == 2013)
res_IMC4 = subset(fit_IMC$residuals, dados_20a79$ano == 2014)
res_IMC5 = subset(fit_IMC$residuals, dados_20a79$ano == 2015)
res_IMC6 = subset(fit_IMC$residuals, dados_20a79$ano == 2016)
res_IMC7 = subset(fit_IMC$residuals, dados_20a79$ano == 2017)
res_IMC8 = subset(fit_IMC$residuals, dados_20a79$ano == 2018)
res_IMC9 = subset(fit_IMC$residuals, dados_20a79$ano == 2019)

res_IMC = data.frame(res_IMC0, res_IMC1, res_IMC2, res_IMC3, res_IMC4, res_IMC5, res_IMC6, res_IMC7, res_IMC8, res_IMC9)
cor(res_IMC)
cov(res_IMC)

uni_imc1 = geeglm(IMC_i_cat_excesso_prop ~ sexo, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,sexo,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc2 = geeglm(IMC_i_cat_excesso_prop ~ idade_cat, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,idade_cat,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc3 = geeglm(IMC_i_cat_excesso_prop ~ anos_de_estudo, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,anos_de_estudo,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc4 = geeglm(IMC_i_cat_excesso_prop ~ plano_saude_nao_prop, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc5 = geeglm(IMC_i_cat_excesso_prop ~ Nota, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,Nota,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc6 = geeglm(IMC_i_cat_excesso_prop ~ IVS, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,IVS,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc7 = geeglm(IMC_i_cat_excesso_prop ~ IDHM, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,IDHM,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_imc8 = geeglm(IMC_i_cat_excesso_prop ~ Gini, id = cidade, 
                  data = dados_20a79 %>% select(IMC_i_cat_excesso_prop,Gini,cidade) %>% na.omit(), 
                  family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$IMC_i_cat_excesso_prop,dados_20a79$IVS)
cor.test(dados_20a79$IMC_i_cat_excesso_prop,dados_20a79$IDHM)
cor.test(dados_20a79$IMC_i_cat_excesso_prop,dados_20a79$Gini)

Tabela17.1 = rbind(TabelaGEENormal(uni_imc1),TabelaGEENormal(uni_imc2),
                   TabelaGEENormal(uni_imc3),TabelaGEENormal(uni_imc4),
                   TabelaGEENormal(uni_imc5),#TabelaGEENormal(uni_imc6),
                   TabelaGEENormal(uni_imc7)#TabelaGEENormal(uni_imc8)
                   )
#write.xlsx(Tabela17.1 %>% as.data.frame(), 'Tabela 17.1.xlsx', rowNames = F)

#Sem interação: Nota e anos_de_estudo
multi_semint_imc1 = geeglm(IMC_i_cat_excesso_prop ~ sexo + idade_cat + #anos_de_estudo + 
                             plano_saude_nao_prop + #Nota + 
                             IDHM, 
                           id = cidade, data = dados_20a79 %>% 
                             select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                           family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_imc1)
#write.xlsx(TabelaGEENormal(multi_semint_imc1) %>% as.data.frame(), 'Tabela 17.2.xlsx', rowNames = F)

hist(multi_semint_imc1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_imc1$residuals),max(multi_semint_imc1$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_imc1$residuals),max(multi_semint_imc1$residuals), length.out = 100),
            mean=mean(multi_semint_imc1$residuals),sd=sd(multi_semint_imc1$residuals)))

ks.test(multi_semint_imc1$residuals, "pnorm", mean(multi_semint_imc1$residuals, na.rm = T), 
        sd(multi_semint_imc1$residuals, na.rm = T))
qqnorm(multi_semint_imc1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_imc1$residuals)

#Com interação
multi_imc1 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                      plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc1)

#idade_cat*plano_saude_nao_prop
multi_imc2 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                      plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc2)

#anos_de_estudo*IDHM
multi_imc3 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc3)

#sexo*Nota
multi_imc4 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc4)

#sexo*plano_saude_nao_prop
multi_imc5 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc5)

#plano_saude_nao_prop*IDHM
multi_imc6 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*IDHM +
                      idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc6)

#sexo*IDHM
multi_imc7 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo +
                      idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc7)

#idade_cat*Nota
multi_imc8 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo +
                      idade_cat*anos_de_estudo + idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc8)

#idade_cat*anos_de_estudo
multi_imc9 = geeglm(IMC_i_cat_excesso_prop ~ sexo*idade_cat + sexo*anos_de_estudo +
                      idade_cat*IDHM +
                      anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                      plano_saude_nao_prop*Nota +
                      Nota*IDHM, 
                    id = cidade, data = dados_20a79 %>% 
                      select(IMC_i_cat_excesso_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                    family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_imc9)
#write.xlsx(TabelaGEENormal(multi_imc9) %>% as.data.frame(), 'Tabela 17.3.xlsx', rowNames = F)

hist(multi_imc9$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_imc9$residuals),max(multi_imc9$residuals), length.out = 100),
      dnorm(seq(min(multi_imc9$residuals),max(multi_imc9$residuals), length.out = 100),
            mean=mean(multi_imc9$residuals),sd=sd(multi_imc9$residuals)))

qqnorm(multi_imc9$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_imc9$residuals)

ks.test(multi_imc9$residuals, "pnorm", mean(multi_imc9$residuals, na.rm = T), sd(multi_imc9$residuals, na.rm = T))

####=============
#### flvreg_prop
####=============
hist(dados_20a79$flvreg_prop)
ks.test(dados_20a79$flvreg_prop, "pnorm", mean(dados_20a79$flvreg_prop, na.rm = T), sd(dados_20a79$flvreg_prop, na.rm = T))

fit_flvreg = lm(flvreg_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_flvreg0 = subset(fit_flvreg$residuals, dados_20a79$ano == 2010)
res_flvreg1 = subset(fit_flvreg$residuals, dados_20a79$ano == 2011)
res_flvreg2 = subset(fit_flvreg$residuals, dados_20a79$ano == 2012)
res_flvreg3 = subset(fit_flvreg$residuals, dados_20a79$ano == 2013)
res_flvreg4 = subset(fit_flvreg$residuals, dados_20a79$ano == 2014)
res_flvreg5 = subset(fit_flvreg$residuals, dados_20a79$ano == 2015)
res_flvreg6 = subset(fit_flvreg$residuals, dados_20a79$ano == 2016)
res_flvreg7 = subset(fit_flvreg$residuals, dados_20a79$ano == 2017)
res_flvreg8 = subset(fit_flvreg$residuals, dados_20a79$ano == 2018)
res_flvreg9 = subset(fit_flvreg$residuals, dados_20a79$ano == 2019)

res_flvreg = data.frame(res_flvreg0, res_flvreg1, res_flvreg2, res_flvreg3, res_flvreg4, res_flvreg5, res_flvreg6, res_flvreg7, res_flvreg8, res_flvreg9)
cor(res_flvreg)
cov(res_flvreg)

uni_flvreg1 = geeglm(flvreg_prop ~ sexo, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,sexo,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg2 = geeglm(flvreg_prop ~ idade_cat, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,idade_cat,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg3 = geeglm(flvreg_prop ~ anos_de_estudo, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,anos_de_estudo,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg4 = geeglm(flvreg_prop ~ plano_saude_nao_prop, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg5 = geeglm(flvreg_prop ~ Nota, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,Nota,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg6 = geeglm(flvreg_prop ~ IVS, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,IVS,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg7 = geeglm(flvreg_prop ~ IDHM, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,IDHM,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreg8 = geeglm(flvreg_prop ~ Gini, id = cidade, 
                     data = dados_20a79 %>% select(flvreg_prop,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$flvreg_prop,dados_20a79$IVS)
cor.test(dados_20a79$flvreg_prop,dados_20a79$IDHM)
cor.test(dados_20a79$flvreg_prop,dados_20a79$Gini)

Tabela18.1 = rbind(TabelaGEENormal(uni_flvreg1),TabelaGEENormal(uni_flvreg2),
                   TabelaGEENormal(uni_flvreg3),TabelaGEENormal(uni_flvreg4),
                   TabelaGEENormal(uni_flvreg5),#TabelaGEENormal(uni_flvreg6),
                   TabelaGEENormal(uni_flvreg7)#TabelaGEENormal(uni_flvreg8)
)
#write.xlsx(Tabela18.1 %>% as.data.frame(), 'Tabela 18.1.xlsx', rowNames = F)

#Sem interação
multi_semint_flvreg1 = geeglm(flvreg_prop ~ sexo + idade_cat + anos_de_estudo + 
                             plano_saude_nao_prop + Nota + 
                             IDHM, 
                           id = cidade, data = dados_20a79 %>% 
                             select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                           family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_flvreg1)
#write.xlsx(TabelaGEENormal(multi_semint_flvreg1) %>% as.data.frame(), 'Tabela 18.2.xlsx', rowNames = F)

hist(multi_semint_flvreg1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_flvreg1$residuals),max(multi_semint_flvreg1$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_flvreg1$residuals),max(multi_semint_flvreg1$residuals), length.out = 100),
            mean=mean(multi_semint_flvreg1$residuals),sd=sd(multi_semint_flvreg1$residuals)))

ks.test(multi_semint_flvreg1$residuals, "pnorm", mean(multi_semint_flvreg1$residuals, na.rm = T), 
        sd(multi_semint_flvreg1$residuals, na.rm = T))
qqnorm(multi_semint_flvreg1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_flvreg1$residuals)

#Com interação
multi_flvreg1 = geeglm(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                         Nota*IDHM, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg1)

#Nota*IDHM
multi_flvreg2 = geeglm(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg2)

#plano_saude_nao_prop*IDHM
multi_flvreg3 = geeglm(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg3)

#sexo*Nota
multi_flvreg4 = geeglm(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg4)

#idade_cat*IDHM
multi_flvreg5 = geeglm(flvreg_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg5)

#sexo*idade_cat
multi_flvreg6 = geeglm(flvreg_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg6)

#sexo*plano_saude_nao_prop
multi_flvreg7 = geeglm(flvreg_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg7)

#anos_de_estudo*Nota
multi_flvreg8 = geeglm(flvreg_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                         plano_saude_nao_prop*Nota, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg8)

#plano_saude_nao_prop*Nota
multi_flvreg9 = geeglm(flvreg_prop ~ sexo*anos_de_estudo + sexo*IDHM +
                         idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                         anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM, 
                       id = cidade, data = dados_20a79 %>% 
                         select(flvreg_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreg9)
#write.xlsx(TabelaGEENormal(multi_flvreg9) %>% as.data.frame(), 'Tabela 18.3.xlsx', rowNames = F)

hist(multi_flvreg9$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_flvreg9$residuals),max(multi_flvreg9$residuals), length.out = 100),
      dnorm(seq(min(multi_flvreg9$residuals),max(multi_flvreg9$residuals), length.out = 100),
            mean=mean(multi_flvreg9$residuals),sd=sd(multi_flvreg9$residuals)))

qqnorm(multi_flvreg9$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_flvreg9$residuals)

ks.test(multi_flvreg9$residuals, "pnorm", mean(multi_flvreg9$residuals, na.rm = T), sd(multi_flvreg9$residuals, na.rm = T))

####==============
#### flvreco_prop
####==============
hist(dados_20a79$flvreco_prop)
ks.test(dados_20a79$flvreco_prop, "pnorm", mean(dados_20a79$flvreco_prop, na.rm = T), sd(dados_20a79$flvreco_prop, na.rm = T))

fit_flvreco = lm(flvreco_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_flvreco0 = subset(fit_flvreco$residuals, dados_20a79$ano == 2010)
res_flvreco1 = subset(fit_flvreco$residuals, dados_20a79$ano == 2011)
res_flvreco2 = subset(fit_flvreco$residuals, dados_20a79$ano == 2012)
res_flvreco3 = subset(fit_flvreco$residuals, dados_20a79$ano == 2013)
res_flvreco4 = subset(fit_flvreco$residuals, dados_20a79$ano == 2014)
res_flvreco5 = subset(fit_flvreco$residuals, dados_20a79$ano == 2015)
res_flvreco6 = subset(fit_flvreco$residuals, dados_20a79$ano == 2016)
res_flvreco7 = subset(fit_flvreco$residuals, dados_20a79$ano == 2017)
res_flvreco8 = subset(fit_flvreco$residuals, dados_20a79$ano == 2018)
res_flvreco9 = subset(fit_flvreco$residuals, dados_20a79$ano == 2019)

res_flvreco = data.frame(res_flvreco0, res_flvreco1, res_flvreco2, res_flvreco3, res_flvreco4, res_flvreco5, res_flvreco6, res_flvreco7, res_flvreco8, res_flvreco9)
cor(res_flvreco)
cov(res_flvreco)

uni_flvreco1 = geeglm(flvreco_prop ~ sexo, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,sexo,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco2 = geeglm(flvreco_prop ~ idade_cat, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,idade_cat,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco3 = geeglm(flvreco_prop ~ anos_de_estudo, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,anos_de_estudo,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco4 = geeglm(flvreco_prop ~ plano_saude_nao_prop, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco5 = geeglm(flvreco_prop ~ Nota, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,Nota,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco6 = geeglm(flvreco_prop ~ IVS, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,IVS,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco7 = geeglm(flvreco_prop ~ IDHM, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,IDHM,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_flvreco8 = geeglm(flvreco_prop ~ Gini, id = cidade, 
                      data = dados_20a79 %>% select(flvreco_prop,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$flvreco_prop,dados_20a79$IVS)
cor.test(dados_20a79$flvreco_prop,dados_20a79$IDHM)
cor.test(dados_20a79$flvreco_prop,dados_20a79$Gini)

Tabela19.1 = rbind(TabelaGEENormal(uni_flvreco1),TabelaGEENormal(uni_flvreco2),
                   TabelaGEENormal(uni_flvreco3),TabelaGEENormal(uni_flvreco4),
                   TabelaGEENormal(uni_flvreco5),#TabelaGEENormal(uni_flvreco6),
                   TabelaGEENormal(uni_flvreco7)#TabelaGEENormal(uni_flvreco8)
)
#write.xlsx(Tabela19.1 %>% as.data.frame(), 'Tabela 19.1.xlsx', rowNames = F)

#Sem interação: Nota
multi_semint_flvreco1 = geeglm(flvreco_prop ~ sexo + idade_cat + anos_de_estudo + 
                                plano_saude_nao_prop + #Nota + 
                                IDHM, 
                              id = cidade, data = dados_20a79 %>% 
                                select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                              family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_flvreco1)
#write.xlsx(TabelaGEENormal(multi_semint_flvreco1) %>% as.data.frame(), 'Tabela 19.2.xlsx', rowNames = F)

hist(multi_semint_flvreco1$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_flvreco1$residuals),max(multi_semint_flvreco1$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_flvreco1$residuals),max(multi_semint_flvreco1$residuals), length.out = 100),
            mean=mean(multi_semint_flvreco1$residuals),sd=sd(multi_semint_flvreco1$residuals)))

ks.test(multi_semint_flvreco1$residuals, "pnorm", mean(multi_semint_flvreco1$residuals, na.rm = T), 
        sd(multi_semint_flvreco1$residuals, na.rm = T))
qqnorm(multi_semint_flvreco1$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_flvreco1$residuals)

#Com interação
multi_flvreco1 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco1)

#sexo*anos_de_estudo
multi_flvreco2 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco2)

#anos_de_estudo*Nota
multi_flvreco3 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                          Nota*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco3)

#Nota*IDHM
multi_flvreco4 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco4)

#idade_cat*IDHM
multi_flvreco5 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco5)

#sexo*Nota
multi_flvreco6 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco6)

#plano_saude_nao_prop*IDHM
multi_flvreco7 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                          plano_saude_nao_prop*Nota, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco7)

#plano_saude_nao_prop*Nota
multi_flvreco8 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*IDHM +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco8)

#idade_cat*Nota
multi_flvreco9 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*IDHM + Nota +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM, 
                        id = cidade, data = dados_20a79 %>% 
                          select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco9)

#Nota
multi_flvreco10 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*plano_saude_nao_prop + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco10)

#sexo*plano_saude_nao_prop
multi_flvreco11 = geeglm(flvreco_prop ~ sexo*idade_cat + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(flvreco_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_flvreco11)
#write.xlsx(TabelaGEENormal(multi_flvreco11) %>% as.data.frame(), 'Tabela 19.3.xlsx', rowNames = F)

hist(multi_flvreco11$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_flvreco11$residuals),max(multi_flvreco11$residuals), length.out = 100),
      dnorm(seq(min(multi_flvreco11$residuals),max(multi_flvreco11$residuals), length.out = 100),
            mean=mean(multi_flvreco11$residuals),sd=sd(multi_flvreco11$residuals)))

qqnorm(multi_flvreco11$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_flvreco11$residuals)

ks.test(multi_flvreco11$residuals, "pnorm", mean(multi_flvreco11$residuals, na.rm = T), sd(multi_flvreco11$residuals, na.rm = T))

####===============
#### refritl5_prop
####===============
####========
#### Normal
####========
hist(dados_20a79$refritl5_prop)
ks.test(dados_20a79$refritl5_prop, "pnorm", mean(dados_20a79$refritl5_prop, na.rm = T), sd(dados_20a79$refritl5_prop, na.rm = T))

fit_refritl5 = lm(refritl5_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_refritl50 = subset(fit_refritl5$residuals, dados_20a79$ano == 2010)
res_refritl51 = subset(fit_refritl5$residuals, dados_20a79$ano == 2011)
res_refritl52 = subset(fit_refritl5$residuals, dados_20a79$ano == 2012)
res_refritl53 = subset(fit_refritl5$residuals, dados_20a79$ano == 2013)
res_refritl54 = subset(fit_refritl5$residuals, dados_20a79$ano == 2014)
res_refritl55 = subset(fit_refritl5$residuals, dados_20a79$ano == 2015)
res_refritl56 = subset(fit_refritl5$residuals, dados_20a79$ano == 2016)
res_refritl57 = subset(fit_refritl5$residuals, dados_20a79$ano == 2017)
res_refritl58 = subset(fit_refritl5$residuals, dados_20a79$ano == 2018)
res_refritl59 = subset(fit_refritl5$residuals, dados_20a79$ano == 2019)

res_refritl5 = data.frame(res_refritl50, res_refritl51, res_refritl52, res_refritl53, res_refritl54, res_refritl55, res_refritl56, res_refritl57, res_refritl58, res_refritl59)
cor(res_refritl5)
cov(res_refritl5)

uni_refritl51 = geeglm(refritl5_prop ~ sexo, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,sexo,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl52 = geeglm(refritl5_prop ~ idade_cat, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,idade_cat,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl53 = geeglm(refritl5_prop ~ anos_de_estudo, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl54 = geeglm(refritl5_prop ~ plano_saude_nao_prop, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl55 = geeglm(refritl5_prop ~ Nota, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,Nota,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl56 = geeglm(refritl5_prop ~ IVS, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,IVS,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl57 = geeglm(refritl5_prop ~ IDHM, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,IDHM,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_refritl58 = geeglm(refritl5_prop ~ Gini, id = cidade, 
                       data = dados_20a79 %>% select(refritl5_prop,Gini,cidade) %>% na.omit(), 
                       family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$refritl5_prop,dados_20a79$IVS)
cor.test(dados_20a79$refritl5_prop,dados_20a79$IDHM)
cor.test(dados_20a79$refritl5_prop,dados_20a79$Gini)

Tabela20.1 = rbind(TabelaGEENormal(uni_refritl51),TabelaGEENormal(uni_refritl52),
                   TabelaGEENormal(uni_refritl53),TabelaGEENormal(uni_refritl54),
                   TabelaGEENormal(uni_refritl55),#TabelaGEENormal(uni_refritl56),
                   TabelaGEENormal(uni_refritl57)#TabelaGEENormal(uni_refritl58)
)
#write.xlsx(Tabela20.1 %>% as.data.frame(), 'Tabela 20.1.xlsx', rowNames = F)

#Sem interação: plano_saude_nao_prop e anos_de_estudo
multi_semint_refritl51 = geeglm(refritl5_prop ~ sexo + idade_cat + #anos_de_estudo + 
                                  #plano_saude_nao_prop + 
                                  Nota + 
                                  IDHM, 
                                id = cidade, data = dados_20a79 %>% 
                                  select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_refritl51)
#write.xlsx(TabelaGEENormal(multi_semint_refritl51) %>% as.data.frame(), 'Tabela 20.2.xlsx', rowNames = F)

hist(multi_semint_refritl51$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_refritl51$residuals),max(multi_semint_refritl51$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_refritl51$residuals),max(multi_semint_refritl51$residuals), length.out = 100),
            mean=mean(multi_semint_refritl51$residuals),sd=sd(multi_semint_refritl51$residuals)))

ks.test(multi_semint_refritl51$residuals, "pnorm", mean(multi_semint_refritl51$residuals, na.rm = T), 
        sd(multi_semint_refritl51$residuals, na.rm = T))
qqnorm(multi_semint_refritl51$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_refritl51$residuals)

#Com interação
multi_refritl51 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl51)

#idade_cat*plano_saude_nao_prop
multi_refritl52 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl52)

#anos_de_estudo*plano_saude_nao_prop
multi_refritl53 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*Nota + anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl53)

#anos_de_estudo*Nota
multi_refritl54 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl54)

#sexo*IDHM
multi_refritl55 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                           Nota*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl55)

#Nota*IDHM
multi_refritl56 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl56)

#plano_saude_nao_prop*IDHM
multi_refritl57 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota +
                           idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                           anos_de_estudo*IDHM +
                           plano_saude_nao_prop*Nota, 
                         id = cidade, data = dados_20a79 %>% 
                           select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                         family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_refritl57)
#write.xlsx(TabelaGEENormal(multi_refritl57) %>% as.data.frame(), 'Tabela 20.3.xlsx', rowNames = F)

hist(multi_refritl57$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_refritl57$residuals),max(multi_refritl57$residuals), length.out = 100),
      dnorm(seq(min(multi_refritl57$residuals),max(multi_refritl57$residuals), length.out = 100),
            mean=mean(multi_refritl57$residuals),sd=sd(multi_refritl57$residuals)))

qqnorm(multi_refritl57$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_refritl57$residuals)

ks.test(multi_refritl57$residuals, "pnorm", mean(multi_refritl57$residuals, na.rm = T), sd(multi_refritl57$residuals, na.rm = T))

####=======
#### Gamma
####=======
unig_refritl51 = geeglm(refritl5_prop ~ sexo, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,sexo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl52 = geeglm(refritl5_prop ~ idade_cat, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,idade_cat,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl53 = geeglm(refritl5_prop ~ anos_de_estudo, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl54 = geeglm(refritl5_prop ~ plano_saude_nao_prop, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl55 = geeglm(refritl5_prop ~ Nota, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,Nota,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl56 = geeglm(refritl5_prop ~ IVS, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,IVS,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl57 = geeglm(refritl5_prop ~ IDHM, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,IDHM,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

unig_refritl58 = geeglm(refritl5_prop ~ Gini, id = cidade, 
                        data = dados_20a79 %>% select(refritl5_prop,Gini,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

Tabela20.4 = rbind(TabelaGEEGama(unig_refritl51),TabelaGEEGama(unig_refritl52),
                   TabelaGEEGama(unig_refritl53),TabelaGEEGama(unig_refritl54),
                   TabelaGEEGama(unig_refritl55),#TabelaGEEGama(unig_refritl56),
                   TabelaGEEGama(unig_refritl57)#TabelaGEEGama(unig_refritl58)
)
#write.xlsx(Tabela20.4 %>% as.data.frame(), 'Tabela 20.4.xlsx', rowNames = F)

#Sem interação: plano_saude_nao_prop e anos_de_estudo
multig_semint_refritl51 = geeglm(refritl5_prop ~ sexo + idade_cat + anos_de_estudo + 
                                   plano_saude_nao_prop + Nota + IDHM, 
                                 id = cidade, data = dados_20a79 %>% 
                                   select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                                 family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_refritl51)
#write.xlsx(TabelaGEEGama(multig_semint_refritl51) %>% as.data.frame(), 'Tabela 20.5.xlsx', rowNames = F)

#Com interação
multig_refritl51 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl51)

#sexo*Nota
multig_refritl52 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl52)

#anos_de_estudo*Nota
multig_refritl53 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*IDHM +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl53)

#anos_de_estudo*IDHM
multig_refritl54 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl54)

#plano_saude_nao_prop*Nota
multig_refritl55 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl55)

#idade_cat*plano_saude_nao_prop
multig_refritl56 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl56)

#plano_saude_nao_prop*IDHM
multig_refritl57 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                            anos_de_estudo*plano_saude_nao_prop +
                            plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl57)

#anos_de_estudo*plano_saude_nao_prop
multig_refritl58 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                            plano_saude_nao_prop*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl58)

#plano_saude_nao_prop*IDHM
multig_refritl59 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IDHM +
                            Nota*IDHM, 
                          id = cidade, data = dados_20a79 %>% 
                            select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl59)

#idade_cat*IDHM
multig_refritl510 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                             idade_cat*anos_de_estudo + idade_cat*Nota +
                             Nota*IDHM, 
                           id = cidade, data = dados_20a79 %>% 
                             select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                           family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl510)

#Nota*IDHM
multig_refritl511 = geeglm(refritl5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IDHM +
                             idade_cat*anos_de_estudo + idade_cat*Nota, 
                           id = cidade, data = dados_20a79 %>% 
                             select(refritl5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IDHM,cidade) %>% na.omit(), 
                           family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_refritl511)
#write.xlsx(TabelaGEEGama(multig_refritl511) %>% as.data.frame(), 'Tabela 20.6.xlsx', rowNames = F)

####==============
#### feijao5_prop
####==============
####========
#### Normal
####========
hist(dados_20a79$feijao5_prop)
ks.test(dados_20a79$feijao5_prop, "pnorm", mean(dados_20a79$feijao5_prop, na.rm = T), sd(dados_20a79$feijao5_prop, na.rm = T))

fit_feijao5 = lm(feijao5_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_feijao50 = subset(fit_feijao5$residuals, dados_20a79$ano == 2010)
res_feijao51 = subset(fit_feijao5$residuals, dados_20a79$ano == 2011)
res_feijao52 = subset(fit_feijao5$residuals, dados_20a79$ano == 2012)
res_feijao53 = subset(fit_feijao5$residuals, dados_20a79$ano == 2013)
res_feijao54 = subset(fit_feijao5$residuals, dados_20a79$ano == 2014)
res_feijao55 = subset(fit_feijao5$residuals, dados_20a79$ano == 2015)
res_feijao56 = subset(fit_feijao5$residuals, dados_20a79$ano == 2016)
res_feijao57 = subset(fit_feijao5$residuals, dados_20a79$ano == 2017)
res_feijao58 = subset(fit_feijao5$residuals, dados_20a79$ano == 2018)
#res_feijao59 = subset(fit_feijao5$residuals, dados_20a79$ano == 2019)

res_feijao5 = data.frame(res_feijao50, res_feijao51, res_feijao52, res_feijao53, res_feijao54, res_feijao55, res_feijao56, res_feijao57, res_feijao58)
cor(res_feijao5)
cov(res_feijao5)

uni_feijao51 = geeglm(feijao5_prop ~ sexo, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,sexo,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao52 = geeglm(feijao5_prop ~ idade_cat, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,idade_cat,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao53 = geeglm(feijao5_prop ~ anos_de_estudo, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao54 = geeglm(feijao5_prop ~ plano_saude_nao_prop, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao55 = geeglm(feijao5_prop ~ Nota, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,Nota,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao56 = geeglm(feijao5_prop ~ IVS, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,IVS,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao57 = geeglm(feijao5_prop ~ IDHM, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,IDHM,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_feijao58 = geeglm(feijao5_prop ~ Gini, id = cidade, 
                      data = dados_20a79 %>% select(feijao5_prop,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$feijao5_prop,dados_20a79$IVS)
cor.test(dados_20a79$feijao5_prop,dados_20a79$IDHM)
cor.test(dados_20a79$feijao5_prop,dados_20a79$Gini)

Tabela21.1 = rbind(TabelaGEENormal(uni_feijao51),TabelaGEENormal(uni_feijao52),
                   TabelaGEENormal(uni_feijao53),TabelaGEENormal(uni_feijao54),
                   TabelaGEENormal(uni_feijao55),TabelaGEENormal(uni_feijao56)
                   #TabelaGEENormal(uni_feijao57)#TabelaGEENormal(uni_feijao58)
)
#write.xlsx(Tabela21.1 %>% as.data.frame(), 'Tabela 21.1.xlsx', rowNames = F)

#Sem interação: plano_saude_nao_prop e anos_de_estudo
multi_semint_feijao51 = geeglm(feijao5_prop ~ sexo + idade_cat + #anos_de_estudo + 
                                 #plano_saude_nao_prop + 
                                 Nota + 
                                 IVS, 
                               id = cidade, data = dados_20a79 %>% 
                                 select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                               family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_semint_feijao51)
#write.xlsx(TabelaGEENormal(multi_semint_feijao51) %>% as.data.frame(), 'Tabela 21.2.xlsx', rowNames = F)

hist(multi_semint_feijao51$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_semint_feijao51$residuals),max(multi_semint_feijao51$residuals), length.out = 100),
      dnorm(seq(min(multi_semint_feijao51$residuals),max(multi_semint_feijao51$residuals), length.out = 100),
            mean=mean(multi_semint_feijao51$residuals),sd=sd(multi_semint_feijao51$residuals)))

qqnorm(multi_semint_feijao51$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_semint_feijao51$residuals)

ks.test(multi_semint_feijao51$residuals, "pnorm", mean(multi_semint_feijao51$residuals, na.rm = T), sd(multi_semint_feijao51$residuals, na.rm = T))

#Com interação
multi_feijao51 = geeglm(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao51)

#sexo*idade_cat
multi_feijao52 = geeglm(feijao5_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao52)

#sexo*anos_de_estudo
multi_feijao53 = geeglm(feijao5_prop ~ sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao53)

#anos_de_estudo*IVS
multi_feijao54 = geeglm(feijao5_prop ~ sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao54)

#sexo*Nota
multi_feijao55 = geeglm(feijao5_prop ~ sexo*plano_saude_nao_prop + sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao55)

#sexo*plano_saude_nao_prop
multi_feijao56 = geeglm(feijao5_prop ~ sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao56)

#idade_cat*plano_saude_nao_prop
multi_feijao57 = geeglm(feijao5_prop ~ sexo*IVS +
                          idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*IVS +
                          anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota +
                          plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                          Nota*IVS, 
                        id = cidade, data = dados_20a79 %>% 
                          select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                        family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_feijao57)
#write.xlsx(TabelaGEENormal(multi_feijao57) %>% as.data.frame(), 'Tabela 21.3.xlsx', rowNames = F)

hist(multi_feijao57$residuals,prob=T,xlab="Resíduos",ylab="Densidade",main="")
lines(seq(min(multi_feijao57$residuals),max(multi_feijao57$residuals), length.out = 100),
      dnorm(seq(min(multi_feijao57$residuals),max(multi_feijao57$residuals), length.out = 100),
            mean=mean(multi_feijao57$residuals),sd=sd(multi_feijao57$residuals)))

qqnorm(multi_feijao57$residuals,xlab="Quantis da Normal Padrão",ylab="Quantis dos Resíduos",main="")
qqline(multi_feijao57$residuals)

ks.test(multi_feijao57$residuals, "pnorm", mean(multi_feijao57$residuals, na.rm = T), sd(multi_feijao57$residuals, na.rm = T))

####=======
#### Gamma
####=======
unig_feijao51 = geeglm(feijao5_prop ~ sexo, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,sexo,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao52 = geeglm(feijao5_prop ~ idade_cat, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,idade_cat,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao53 = geeglm(feijao5_prop ~ anos_de_estudo, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,anos_de_estudo,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao54 = geeglm(feijao5_prop ~ plano_saude_nao_prop, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao55 = geeglm(feijao5_prop ~ Nota, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,Nota,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao56 = geeglm(feijao5_prop ~ IVS, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,IVS,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao57 = geeglm(feijao5_prop ~ IDHM, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,IDHM,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

unig_feijao58 = geeglm(feijao5_prop ~ Gini, id = cidade, 
                       data = dados_20a79 %>% select(feijao5_prop,Gini,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")

Tabela21.4 = rbind(TabelaGEEGama(unig_feijao51),TabelaGEEGama(unig_feijao52),
                   TabelaGEEGama(unig_feijao53),TabelaGEEGama(unig_feijao54),
                   TabelaGEEGama(unig_feijao55),TabelaGEEGama(unig_feijao56)
                   #TabelaGEEGama(unig_feijao57)#TabelaGEEGama(unig_feijao58)
)
#write.xlsx(Tabela21.4 %>% as.data.frame(), 'Tabela 21.4.xlsx', rowNames = F)

#Sem interação: anos_de_estudo e plano_saude_nao_prop
multig_semint_feijao51 = geeglm(feijao5_prop ~ sexo + idade_cat + #anos_de_estudo + 
                                  #plano_saude_nao_prop + 
                                  Nota + IVS, 
                                id = cidade, data = dados_20a79 %>% 
                                  select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                                family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_feijao51)
#write.xlsx(TabelaGEEGama(multig_semint_feijao51) %>% as.data.frame(), 'Tabela 21.5.xlsx', rowNames = F)

#Com interação
multig_feijao51 = geeglm(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*IVS +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                           Nota*IVS, 
                         id = cidade, data = dados_20a79 %>% 
                           select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao51)

#idade_cat*IVS
multig_feijao52 = geeglm(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                           idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                           Nota*IVS, 
                         id = cidade, data = dados_20a79 %>% 
                           select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao52)

#idade_cat*plano_saude_nao_prop
multig_feijao53 = geeglm(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*IVS +
                           idade_cat*anos_de_estudo + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                           Nota*IVS, 
                         id = cidade, data = dados_20a79 %>% 
                           select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao53)

#sexo*Nota
multig_feijao54 = geeglm(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IVS +
                           idade_cat*anos_de_estudo + idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                           Nota*IVS, 
                         id = cidade, data = dados_20a79 %>% 
                           select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao54)

#idade_cat*anos_de_estudo
multig_feijao55 = geeglm(feijao5_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*IVS +
                           idade_cat*Nota +
                           anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*IVS +
                           plano_saude_nao_prop*Nota + plano_saude_nao_prop*IVS +
                           Nota*IVS, 
                         id = cidade, data = dados_20a79 %>% 
                           select(feijao5_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,IVS,cidade) %>% na.omit(), 
                         family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_feijao55)
#write.xlsx(TabelaGEEGama(multig_feijao55) %>% as.data.frame(), 'Tabela 21.6.xlsx', rowNames = F)

####======
#### hart
####======
hist(dados_20a79$hart_prop)
ks.test(dados_20a79$hart_prop, "pnorm", mean(dados_20a79$hart_prop, na.rm = T), sd(dados_20a79$hart_prop, na.rm = T))

fit_hart = lm(hart_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_hart0 = subset(fit_hart$residuals, dados_20a79$ano == 2010)
res_hart1 = subset(fit_hart$residuals, dados_20a79$ano == 2011)
res_hart2 = subset(fit_hart$residuals, dados_20a79$ano == 2012)
res_hart3 = subset(fit_hart$residuals, dados_20a79$ano == 2013)
res_hart4 = subset(fit_hart$residuals, dados_20a79$ano == 2014)
res_hart5 = subset(fit_hart$residuals, dados_20a79$ano == 2015)
res_hart6 = subset(fit_hart$residuals, dados_20a79$ano == 2016)
res_hart7 = subset(fit_hart$residuals, dados_20a79$ano == 2017)
res_hart8 = subset(fit_hart$residuals, dados_20a79$ano == 2018)
res_hart9 = subset(fit_hart$residuals, dados_20a79$ano == 2019)

res_hart = data.frame(res_hart0, res_hart1, res_hart2, res_hart3, res_hart4, res_hart5, res_hart6, res_hart7, res_hart8, res_hart9)
cor(res_hart)
cov(res_hart)

uni_hart1 = geeglm(hart_prop ~ sexo, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,sexo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart2 = geeglm(hart_prop ~ idade_cat, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,idade_cat,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart3 = geeglm(hart_prop ~ anos_de_estudo, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,anos_de_estudo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart4 = geeglm(hart_prop ~ plano_saude_nao_prop, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart5 = geeglm(hart_prop ~ Nota, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart6 = geeglm(hart_prop ~ IVS, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,IVS,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart7 = geeglm(hart_prop ~ IDHM, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,IDHM,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_hart8 = geeglm(hart_prop ~ Gini, id = cidade, 
                   data = dados_20a79 %>% select(hart_prop,Gini,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$hart_prop,dados_20a79$IVS)
cor.test(dados_20a79$hart_prop,dados_20a79$IDHM)
cor.test(dados_20a79$hart_prop,dados_20a79$Gini)

Tabela22.1 = rbind(TabelaGEENormal(uni_hart1),TabelaGEENormal(uni_hart2),
                   TabelaGEENormal(uni_hart3),TabelaGEENormal(uni_hart4),
                   TabelaGEENormal(uni_hart5),#TabelaGEENormal(uni_hart6),TabelaGEENormal(uni_hart7)
                   TabelaGEENormal(uni_hart8)
)
#write.xlsx(Tabela22.1 %>% as.data.frame(), 'Tabela 22.1.xlsx', rowNames = F)

#Sem interação: Nota
multi_semint_hart1 = geeglm(hart_prop ~ sexo + idade_cat + anos_de_estudo + 
                              plano_saude_nao_prop + Nota + 
                              Gini, 
                            id = cidade, data = dados_20a79 %>% 
                              select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
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
multi_hart1 = geeglm(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart1)

#sexo*Nota
multi_hart2 = geeglm(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart2)

#idade_cat*Gini
multi_hart3 = geeglm(hart_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart3)

#sexo*idade_cat
multi_hart4 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart4)

#plano_saude_nao_prop*Nota
multi_hart5 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart5)

#idade_cat*anos_de_estudo
multi_hart6 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart6)

#anos_de_estudo*Nota
multi_hart7 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*plano_saude_nao_prop + idade_cat*Nota +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart7)

#idade_cat*Nota
multi_hart8 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*plano_saude_nao_prop +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart8)

#plano_saude_nao_prop*Gini
multi_hart9 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*plano_saude_nao_prop +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart9)

#Nota*Gini
multi_hart10 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini + Nota +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart10)

#Nota
multi_hart11 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini,
                      id = cidade, data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_hart11)

#anos_de_estudo*Gini
multi_hart12 = geeglm(hart_prop ~ sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop,
                      id = cidade, data = dados_20a79 %>% 
                        select(hart_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
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
hist(dados_20a79$diab_prop)
ks.test(dados_20a79$diab_prop, "pnorm", mean(dados_20a79$diab_prop, na.rm = T), sd(dados_20a79$diab_prop, na.rm = T))

fit_diab = lm(diab_prop ~ factor(sexo)*factor(ano), data=dados_20a79)
res_diab0 = subset(fit_diab$residuals, dados_20a79$ano == 2010)
res_diab1 = subset(fit_diab$residuals, dados_20a79$ano == 2011)
res_diab2 = subset(fit_diab$residuals, dados_20a79$ano == 2012)
res_diab3 = subset(fit_diab$residuals, dados_20a79$ano == 2013)
res_diab4 = subset(fit_diab$residuals, dados_20a79$ano == 2014)
res_diab5 = subset(fit_diab$residuals, dados_20a79$ano == 2015)
res_diab6 = subset(fit_diab$residuals, dados_20a79$ano == 2016)
res_diab7 = subset(fit_diab$residuals, dados_20a79$ano == 2017)
res_diab8 = subset(fit_diab$residuals, dados_20a79$ano == 2018)
res_diab9 = subset(fit_diab$residuals, dados_20a79$ano == 2019)

res_diab = data.frame(res_diab0, res_diab1, res_diab2, res_diab3, res_diab4, res_diab5, res_diab6, res_diab7, res_diab8, res_diab9)
cor(res_diab)
cov(res_diab)

uni_diab1 = geeglm(diab_prop ~ sexo, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,sexo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab2 = geeglm(diab_prop ~ idade_cat, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,idade_cat,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab3 = geeglm(diab_prop ~ anos_de_estudo, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,anos_de_estudo,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab4 = geeglm(diab_prop ~ plano_saude_nao_prop, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab5 = geeglm(diab_prop ~ Nota, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,Nota,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab6 = geeglm(diab_prop ~ IVS, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,IVS,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab7 = geeglm(diab_prop ~ IDHM, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,IDHM,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

uni_diab8 = geeglm(diab_prop ~ Gini, id = cidade, 
                   data = dados_20a79 %>% select(diab_prop,Gini,cidade) %>% na.omit(), 
                   family = gaussian(link = 'identity'), corstr = "exchangeable")

cor.test(dados_20a79$diab_prop,dados_20a79$IVS)
cor.test(dados_20a79$diab_prop,dados_20a79$IDHM)
cor.test(dados_20a79$diab_prop,dados_20a79$Gini)

Tabela23.1 = rbind(TabelaGEENormal(uni_diab1),TabelaGEENormal(uni_diab2),
                   TabelaGEENormal(uni_diab3),TabelaGEENormal(uni_diab4),
                   TabelaGEENormal(uni_diab5),#TabelaGEENormal(uni_diab6),TabelaGEENormal(uni_diab7)
                   TabelaGEENormal(uni_diab8)
)
#write.xlsx(Tabela23.1 %>% as.data.frame(), 'Tabela 23.1.xlsx', rowNames = F)

#Sem interação
multi_semint_diab1 = geeglm(diab_prop ~ #sexo + 
                              idade_cat + #anos_de_estudo + 
                              #plano_saude_nao_prop + #Nota + 
                              Gini, 
                            id = cidade, data = dados_20a79 %>% 
                              select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
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
multi_diab1 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab1)

#anos_de_estudo*Nota
multi_diab2 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab2)

#plano_saude_nao_prop*Nota
multi_diab3 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab3)

#sexo*Nota
multi_diab4 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab4)

#idade_cat*anos_de_estudo
multi_diab5 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                       idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                       anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                       plano_saude_nao_prop*Gini +
                       Nota*Gini, 
                     id = cidade, data = dados_20a79 %>% 
                       select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                     family = gaussian(link = 'identity'), corstr = "exchangeable")
summary(multi_diab5)

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
unig_diab1 = geeglm(diab_prop ~ sexo, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,sexo,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab2 = geeglm(diab_prop ~ idade_cat, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,idade_cat,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab3 = geeglm(diab_prop ~ anos_de_estudo, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,anos_de_estudo,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab4 = geeglm(diab_prop ~ plano_saude_nao_prop, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,plano_saude_nao_prop,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab5 = geeglm(diab_prop ~ Nota, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,Nota,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab6 = geeglm(diab_prop ~ IVS, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,IVS,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab7 = geeglm(diab_prop ~ IDHM, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,IDHM,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

unig_diab8 = geeglm(diab_prop ~ Gini, id = cidade, 
                    data = dados_20a79 %>% select(diab_prop,Gini,cidade) %>% na.omit(), 
                    family = Gamma(link = "log"), corstr = "exchangeable")

Tabela23.4 = rbind(TabelaGEEGama(unig_diab1),TabelaGEEGama(unig_diab2),
                   TabelaGEEGama(unig_diab3),TabelaGEEGama(unig_diab4),
                   TabelaGEEGama(unig_diab5),#TabelaGEEGama(unig_diab6),TabelaGEEGama(unig_diab7)
                   TabelaGEEGama(unig_diab8)
)
#write.xlsx(Tabela23.4 %>% as.data.frame(), 'Tabela 23.4.xlsx', rowNames = F)

#Sem interação
multig_semint_diab1 = geeglm(diab_prop ~ sexo + 
                               idade_cat + #anos_de_estudo + 
                               #plano_saude_nao_prop + #Nota + 
                               Gini, 
                             id = cidade, data = dados_20a79 %>% 
                               select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                             family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_semint_diab1)
#write.xlsx(TabelaGEEGama(multig_semint_diab1) %>% as.data.frame(), 'Tabela 23.5.xlsx', rowNames = F)

#Com interação
multig_diab1 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab1)

#idade_cat*Nota
multig_diab2 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                        Nota*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab2)

#plano_saude_nao_prop*Nota
multig_diab3 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini +
                        Nota*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab3)

#Nota*Gini
multig_diab4 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab4)

#sexo*Nota
multig_diab5 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini + Nota, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab5)

#Nota

#idade_cat*anos_de_estudo
multig_diab6 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab6)

#anos_de_estudo*Nota
multig_diab7 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Gini +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab7)

#sexo*Gini
multig_diab8 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                        idade_cat*plano_saude_nao_prop + idade_cat*Gini +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab8)

#idade_cat*Gini
multig_diab9 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                        idade_cat*plano_saude_nao_prop +
                        anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Gini +
                        plano_saude_nao_prop*Gini, 
                      id = cidade, data = dados_20a79 %>% 
                        select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                      family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab9)

#anos_de_estudo*Gini
multig_diab10 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                         idade_cat*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop +
                         plano_saude_nao_prop*Gini, 
                       id = cidade, data = dados_20a79 %>% 
                         select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab10)

#plano_saude_nao_prop*Gini
multig_diab11 = geeglm(diab_prop ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop +
                         idade_cat*plano_saude_nao_prop +
                         anos_de_estudo*plano_saude_nao_prop + Gini, 
                       id = cidade, data = dados_20a79 %>% 
                         select(diab_prop,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                       family = Gamma(link = "log"), corstr = "exchangeable")
summary(multig_diab11)
#write.xlsx(TabelaGEEGama(multig_diab11) %>% as.data.frame(), 'Tabela 23.6.xlsx', rowNames = F)

####===========
#### TaxaICSAP
####===========
hist(dados_20a79$TaxaICSAP)
ks.test(dados_20a79$TaxaICSAP, "pnorm", mean(dados_20a79$TaxaICSAP, na.rm = T), sd(dados_20a79$TaxaICSAP, na.rm = T))

uni_TaxaICSAP1 = geeglm(TaxaICSAP ~ sexo, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,sexo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP2 = geeglm(TaxaICSAP ~ idade_cat, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,idade_cat,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP3 = geeglm(TaxaICSAP ~ anos_de_estudo, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,anos_de_estudo,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP4 = geeglm(TaxaICSAP ~ plano_saude_nao_prop, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,plano_saude_nao_prop,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP5 = geeglm(TaxaICSAP ~ Nota, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,Nota,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP6 = geeglm(TaxaICSAP ~ IVS, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,IVS,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP7 = geeglm(TaxaICSAP ~ IDHM, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,IDHM,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

uni_TaxaICSAP8 = geeglm(TaxaICSAP ~ Gini, id = cidade, 
                        data = dados_20a79 %>% select(TaxaICSAP,Gini,cidade) %>% na.omit(), 
                        family = Gamma(link = "log"), corstr = "exchangeable")

cor.test(dados_20a79$TaxaICSAP,dados_20a79$IVS)
cor.test(dados_20a79$TaxaICSAP,dados_20a79$IDHM)
cor.test(dados_20a79$TaxaICSAP,dados_20a79$Gini)

Tabela24.1 = rbind(TabelaGEEGama(uni_TaxaICSAP1),TabelaGEEGama(uni_TaxaICSAP2),
                   TabelaGEEGama(uni_TaxaICSAP3),TabelaGEEGama(uni_TaxaICSAP4),
                   TabelaGEEGama(uni_TaxaICSAP5),#TabelaGEEGama(uni_TaxaICSAP6)
                   #TabelaGEEGama(uni_TaxaICSAP7)
                   TabelaGEEGama(uni_TaxaICSAP8)
)
#write.xlsx(Tabela24.1 %>% as.data.frame(), 'Tabela 24.1.xlsx', rowNames = F)

#Sem interação: Nota
multi_semint_TaxaICSAP1 = geeglm(TaxaICSAP ~ sexo + 
                                   idade_cat + anos_de_estudo + 
                                   plano_saude_nao_prop + #Nota + 
                                   Gini, 
                                 id = cidade, data = dados_20a79 %>% 
                                   select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                                 family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_semint_TaxaICSAP1)
#write.xlsx(TabelaGEEGama(multi_semint_TaxaICSAP1) %>% as.data.frame(), 'Tabela 24.2.xlsx', rowNames = F)

#Com interação
multi_TaxaICSAP1 = geeglm(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Nota + plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP1)

#plano_saude_nao_prop*Nota
multi_TaxaICSAP2 = geeglm(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*plano_saude_nao_prop + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP2)

#idade_cat*plano_saude_nao_prop
multi_TaxaICSAP3 = geeglm(TaxaICSAP ~ sexo*idade_cat + sexo*anos_de_estudo + sexo*plano_saude_nao_prop + sexo*Nota + sexo*Gini +
                            idade_cat*anos_de_estudo + idade_cat*Nota + idade_cat*Gini +
                            anos_de_estudo*plano_saude_nao_prop + anos_de_estudo*Nota + anos_de_estudo*Gini +
                            plano_saude_nao_prop*Gini +
                            Nota*Gini, 
                          id = cidade, data = dados_20a79 %>% 
                            select(TaxaICSAP,sexo,idade_cat,anos_de_estudo,plano_saude_nao_prop,Nota,Gini,cidade) %>% na.omit(), 
                          family = Gamma(link = "log"), corstr = "exchangeable")
summary(multi_TaxaICSAP3)

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