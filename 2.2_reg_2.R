# £ukasz Szymczyk
# 2019-12-06
# encoding: UTF-8
# 2.2 regresion 2 (eqXY) ==========================================================================


# libraries
library(tidyverse)
library(data.table)
library(magrittr)
library(openxlsx)
library(foreign)
library(gplots)
library(plm)
library(tseries)
library(lmtest)


# FUNCTION: FORMULA1 ==============================================================================

FORMULA1 <- function(I,J,CONFIG){
  znak <- 'nie'
  napis <- 'FEE ~' 
  if(CONFIG[paste0(I,'.',J),'PER']=='1'){
    napis <- paste0(napis,' PER')
    znak <- 'tak'
  }
  if(CONFIG[paste0(I,'.',J),'CF']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + CF')
    }else{
      napis <- paste0(napis,' CF')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'LNNAV']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + LNNAV')
    }else{
      napis <- paste0(napis,' LNNAV')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'LNAGEM']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + LNAGEM')
    }else{
      napis <- paste0(napis,' LNAGEM')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'OAUM']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + OAUM')
    }else{
      napis <- paste0(napis,' OAUM')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'ARAC']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + ARAC')
    }else{
      napis <- paste0(napis,' ARAC')
      znak <- 'tak'
    }
  }
  rm(znak)
  return(napis)
}


# FUNCTION: FORMULA2 ==============================================================================

FORMULA2 <- function(I,J,CONFIG){
  znak <- 'nie'
  napis <- 'FEE ~' 
  if(CONFIG[paste0(I,'.',J),'PER']=='1'){
    napis <- paste0(napis,' PER')
    znak <- 'tak'
  }
  if(CONFIG[paste0(I,'.',J),'CF']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + CF')
    }else{
      napis <- paste0(napis,' CF')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'LNNAV']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + LNNAV')
    }else{
      napis <- paste0(napis,' LNNAV')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'LNAGEM']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + LNAGEM')
    }else{
      napis <- paste0(napis,' LNAGEM')
      znak <- 'tak'
    }
  }
  if(CONFIG[paste0(I,'.',J),'OAUM']=='1'){
    if(znak=='tak'){
      napis <- paste0(napis,' + OAUM')
    }else{
      napis <- paste0(napis,' OAUM')
      znak <- 'tak'
    }
  }
  rm(znak)
  return(napis)
}


# DICTS ===========================================================================================

K <- c('OLS','LSDV','FE','RE','FEt')
G <- c('AKPca', 'MIPca','AKP','AKZ','MIP','MIZ','PDP','PDZ','RPP','ARN')


# TABLES1 ==========================================================================================

config1 <- tibble(
  i = 0, j = 0, group = 'kod_grupy',
  FEEm = 'miara_fee', PERm = 'miara_per',
  PER = 0, CF = 0, LNNAV = 0, LNAGEM = 0, OAUM = 0, ARAC = 0
)

results1 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0, CF = 0, LNNAV = 0, LNAGEM = 0, OAUM = 0, ARAC = 0,
  rsq = 0, adjrsq = 0, const = 0
)

pval1 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0, CF = 0, LNNAV = 0, LNAGEM = 0, OAUM = 0, ARAC = 0
)

tests1 <- tibble(  
  i = 0, j = 0, group = 'kod_grupy',
  Hausman = 0, Ftime = 0, LM = 0, BGW = 0
)

row.names(config1) <- '0.0'
row.names(results1) <- '0.0.0'
row.names(pval1) <- '0.0.0'
row.names(tests1) <- '0.0'


# TABLES2 ==========================================================================================

config2 <- tibble(
  i = 0, j = 0, group = 'kod_grupy',
  FEEm = 'miara_fee', PERm = 'miara_per',
  PER = 0, CF = 0, LNNAV = 0, LNAGEM = 0, OAUM = 0
)

results2 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0, CF = 0, LNNAV = 0, LNAGEM = 0, OAUM = 0,
  rsq = 0, adjrsq = 0, const = 0
)

pval2 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0, CF = 0, LNNAV = 0, LNAGEM = 0, OAUM = 0
)

tests2 <- tibble(  
  i = 0, j = 0, group = 'kod_grupy',
  Hausman = 0, Ftime = 0, LM = 0, BGW = 0
)

row.names(config2) <- '0.0'
row.names(results2) <- '0.0.0'
row.names(pval2) <- '0.0.0'
row.names(tests2) <- '0.0'


#start ============================================================================================
#i=1
for(i in 1:length(G)){
  j <- 0
  group <- G[i]
  
  # loading the data
  panel <- read.xlsx('data//raw//sample5_sample.xlsx',  sheet = group)
  panel <- as_tibble(panel)
  #str(panel)
  panel <- panel[,c(1:2,9:16)]
  panel$FI_k <- as.factor(panel$FI_k)
  panel$Data <- as.Date(panel$Data, origin = "1899-12-30")
  #panel$FI_w <- as.factor(panel$FI_w )
  #panel$TFI_j <- as.factor(panel$TFI_j)
  #panel$RETAIL <- as.factor(panel$RETAIL)
  #panel$SFIO <- as.factor(panel$SFIO)
  #panel$BANK <- as.factor(panel$BANK)
  #panel$BBANK <- as.factor(panel$BBANK)
  mini <- min(panel$Data)
  mini <- year(mini)*12+month(mini)-1
  panel <- panel %>% mutate (Data = year(Data)*12+month(Data)-mini)
  rm(mini)
  
  miary1 <- names(panel)[c(3,5)]
  panel1 <- panel[,-c(4)]
  names(panel1) <- c('fund','month','FEE','PER','CF','LNNAV','LNAGEM','OAUM','ARAC')
  
  miary2<- names(panel)[c(4,5)]
  panel2 <- panel[,-c(3,10)]
  names(panel2) <- c('fund','month','FEE','PER','CF','LNNAV','LNAGEM','OAUM') 
  
  #str(panel)
  #summary(panel)
  
  # ARMF -----
  j <- 0
  z <- 0
  while (z==0) {
    
    if(j==0){
      config1[paste0(i,'.',j),] <- c(i,j,group, miary1, rep(1,6))
    }else{
      config1[paste0(i,'.',j),] <- c(i,j,group, miary1, config1[paste0(i,'.',j-1),c(6:11)])
      config1[paste0(i,'.',j),del] <- 0
    }
    
    for(k in 1:5){
      results1[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
      pval1[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
    }
    
    tests1[paste0(i,'.',j),c(1:3)] <- c(i,j,group)
    
    formula0 <- formula(FORMULA1(i,j,config1))
    formula0d <- formula(paste0(FORMULA1(i,j,config1),' + factor(fund) -1'))
    formula0t <- formula(paste0(FORMULA1(i,j,config1),' + factor(month)'))
    formula0f <- formula(paste0(FORMULA1(i,j,config1),' + factor(fund)'))
    
    mod_OLS <- lm (formula0, data = panel1) # k=1
    mod_LSDV <- lm(formula0d, data= panel1) # k=2
    mod_FE <- plm (formula0, data = panel1, index = c('fund','month'), model = 'within') # k=3
    mod_RE <- plm (formula0, data = panel1, index = c('fund','month'), model = 'random') # k=4
    mod_FEt <-  plm (formula0t, data = panel1, index = c('fund','month'), model = 'within') #k=5
    
    tests1[paste0(i,'.',j), 'Hausman'] <- phtest(mod_FE, mod_RE)$p.value #If this number is < 0.05 then use FE
    tests1[paste0(i,'.',j), 'Ftime'] <- pFtest(mod_FE, mod_OLS)$p.value # Testing FE, null: OLS better than FE (p-value is < 0.05 then FE is a better choice)
    tests1[paste0(i,'.',j), 'LM'] <- pcdtest(mod_FE, test = c("lm"))$p.value #p-value is < 0.05 then we have cross-sectional dependence 
    tests1[paste0(i,'.',j), 'BGW'] <- pbgtest(mod_FE)$p.value #null is that there is not serial correlation (p > 0.05)
    
    for(l in 6:11){
      results1[paste0(i,'.',j,'.1'),names(results1)[l]] <- mod_OLS$coefficients[names(results1)[l]]
      results1[paste0(i,'.',j,'.2'),names(results1)[l]] <- mod_LSDV$coefficients[names(results1)[l]]
      results1[paste0(i,'.',j,'.3'),names(results1)[l]] <- mod_FE$coefficients[names(results1)[l]]
      results1[paste0(i,'.',j,'.4'),names(results1)[l]] <- mod_RE$coefficients[names(results1)[l]]
      results1[paste0(i,'.',j,'.5'),names(results1)[l]] <- mod_FEt$coefficients[names(results1)[l]]
      pval1[paste0(i,'.',j,'.1'),names(results1)[l]] <- summary(mod_OLS)$coefficients[,4][names(results1)[l]]
      pval1[paste0(i,'.',j,'.2'),names(results1)[l]] <- summary(mod_LSDV)$coefficients[,4][names(results1)[l]]
      pval1[paste0(i,'.',j,'.3'),names(results1)[l]] <- summary(mod_FE)$coefficients[,4][names(results1)[l]]
      pval1[paste0(i,'.',j,'.4'),names(results1)[l]] <- summary(mod_RE)$coefficients[,4][names(results1)[l]]
      pval1[paste0(i,'.',j,'.5'),names(results1)[l]] <- summary(mod_FEt)$coefficients[,4][names(results1)[l]]
    }
    
    results1[paste0(i,'.',j,'.1'),'rsq'] <- summary(mod_OLS)$r.squared
    results1[paste0(i,'.',j,'.1'),'adjrsq'] <- summary(mod_OLS)$adj.r.squared
    results1[paste0(i,'.',j,'.2'),'rsq'] <- summary(mod_LSDV)$r.squared
    results1[paste0(i,'.',j,'.2'),'adjrsq'] <- summary(mod_LSDV)$adj.r.squared 
    results1[paste0(i,'.',j,'.3'),'rsq'] <- summary(mod_FE)$r.squared[1]
    results1[paste0(i,'.',j,'.3'),'adjrsq'] <- summary(mod_FE)$r.squared[2]
    results1[paste0(i,'.',j,'.4'),'rsq'] <- summary(mod_RE)$r.squared[1]
    results1[paste0(i,'.',j,'.4'),'adjrsq'] <- summary(mod_RE)$r.squared[2]
    results1[paste0(i,'.',j,'.5'),'rsq'] <- summary(mod_FEt)$r.squared[1]
    results1[paste0(i,'.',j,'.5'),'adjrsq'] <- summary(mod_FEt)$r.squared[2]
    
    results1[paste0(i,'.',j,'.3'),'const'] <- mean(fixef(mod_FE))
    
    rm(PVALU,del)
    PVALU <- summary(mod_FE)$coefficients[,4]
    if(sum(PVALU > 0.05)==0){
      z <- 1
    }else{
      del <- names(PVALU[PVALU==max(PVALU)])
      j <- j+1
    }
    
  }
  
  # TOC -----
  j <- 0
  z <- 0
  while (z==0) {
    
    if(j==0){
      config2[paste0(i,'.',j),] <- c(i,j,group, miary2, rep(1,5))
    }else{
      config2[paste0(i,'.',j),] <- c(i,j,group, miary2, config2[paste0(i,'.',j-1),c(6:10)])
      config2[paste0(i,'.',j),del] <- 0
    }
    
    for(k in 1:5){
      results2[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
      pval2[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
    }
    
    tests2[paste0(i,'.',j),c(1:3)] <- c(i,j,group)
    
    formula0 <- formula(FORMULA2(i,j,config2))
    formula0d <- formula(paste0(FORMULA2(i,j,config2),' + factor(fund) -1'))
    formula0t <- formula(paste0(FORMULA2(i,j,config2),' + factor(month)'))
    formula0f <- formula(paste0(FORMULA2(i,j,config2),' + factor(fund)'))
    
    mod_OLS <- lm (formula0, data = panel2) # k=1
    mod_LSDV <- lm(formula0d, data = panel2) # k=2
    mod_FE <- plm (formula0, data = panel2, index = c('fund','month'), model = 'within') # k=3
    mod_RE <- plm (formula0, data = panel2, index = c('fund','month'), model = 'random') # k=4
    mod_FEt <-  plm (formula0t, data = panel2, index = c('fund','month'), model = 'within') #k=5
    
    tests2[paste0(i,'.',j), 'Hausman'] <- phtest(mod_FE, mod_RE)$p.value #If this number is < 0.05 then use FE
    tests2[paste0(i,'.',j), 'Ftime'] <- pFtest(mod_FE, mod_OLS)$p.value # Testing FE, null: OLS better than FE (p-value is < 0.05 then FE is a better choice)
    tests2[paste0(i,'.',j), 'LM'] <- pcdtest(mod_FE, test = c("lm"))$p.value #p-value is < 0.05 then we have cross-sectional dependence 
    tests2[paste0(i,'.',j), 'BGW'] <- pbgtest(mod_FE)$p.value #null is that there is not serial correlation (p > 0.05)
    
    for(l in 6:11){
      results2[paste0(i,'.',j,'.1'),names(results2)[l]] <- mod_OLS$coefficients[names(results2)[l]]
      results2[paste0(i,'.',j,'.2'),names(results2)[l]] <- mod_LSDV$coefficients[names(results2)[l]]
      results2[paste0(i,'.',j,'.3'),names(results2)[l]] <- mod_FE$coefficients[names(results2)[l]]
      results2[paste0(i,'.',j,'.4'),names(results2)[l]] <- mod_RE$coefficients[names(results2)[l]]
      results2[paste0(i,'.',j,'.5'),names(results2)[l]] <- mod_FEt$coefficients[names(results2)[l]]
      pval2[paste0(i,'.',j,'.1'),names(results2)[l]] <- summary(mod_OLS)$coefficients[,4][names(results2)[l]]
      pval2[paste0(i,'.',j,'.2'),names(results2)[l]] <- summary(mod_LSDV)$coefficients[,4][names(results2)[l]]
      pval2[paste0(i,'.',j,'.3'),names(results2)[l]] <- summary(mod_FE)$coefficients[,4][names(results2)[l]]
      pval2[paste0(i,'.',j,'.4'),names(results2)[l]] <- summary(mod_RE)$coefficients[,4][names(results2)[l]]
      pval2[paste0(i,'.',j,'.5'),names(results2)[l]] <- summary(mod_FEt)$coefficients[,4][names(results2)[l]]
    }
    
    results2[paste0(i,'.',j,'.1'),'rsq'] <- summary(mod_OLS)$r.squared
    results2[paste0(i,'.',j,'.1'),'adjrsq'] <- summary(mod_OLS)$adj.r.squared
    results2[paste0(i,'.',j,'.2'),'rsq'] <- summary(mod_LSDV)$r.squared
    results2[paste0(i,'.',j,'.2'),'adjrsq'] <- summary(mod_LSDV)$adj.r.squared 
    results2[paste0(i,'.',j,'.3'),'rsq'] <- summary(mod_FE)$r.squared[1]
    results2[paste0(i,'.',j,'.3'),'adjrsq'] <- summary(mod_FE)$r.squared[2]
    results2[paste0(i,'.',j,'.4'),'rsq'] <- summary(mod_RE)$r.squared[1]
    results2[paste0(i,'.',j,'.4'),'adjrsq'] <- summary(mod_RE)$r.squared[2]
    results2[paste0(i,'.',j,'.5'),'rsq'] <- summary(mod_FEt)$r.squared[1]
    results2[paste0(i,'.',j,'.5'),'adjrsq'] <- summary(mod_FEt)$r.squared[2]
    
    results2[paste0(i,'.',j,'.3'),'const'] <- mean(fixef(mod_FE))
    
    rm(PVALU,del)
    PVALU <- summary(mod_FE)$coefficients[,4]
    if(sum(PVALU > 0.05)==0){
      z <- 1
    }else{
      del <- names(PVALU[PVALU==max(PVALU)])
      j <- j+1
    }
    
  }  
  
  
  rm(miary1,miary2,PVALU,del,z,j)
  
}


###################################################################################################

results1 <- results1[results1$model == 'FE',]
pval1 <- pval1[pval1$model == 'FE',]
results2 <- results2[results2$model == 'FE',]
pval2 <- pval2[pval2$model == 'FE',]

I1 <- results1 %>% group_by(i) %>% summarise(j = max(j))
I1 <- paste0(I1$i,'.',I1$j)
results1 <- results1[paste0(results1$i,'.',results1$j) %in% I1,]
tests1 <- tests1[paste0(tests1$i,'.',tests1$j) %in% I1,]
pval1 <- pval1[paste0(pval1$i,'.',pval1$j) %in% I1,]
config1 <- config1[paste0(config1$i,'.',config1$j) %in% I1,]


I2 <- results2 %>% group_by(i) %>% summarise(j = max(j))
I2 <- paste0(I2$i,'.',I2$j)
results2 <- results2[paste0(results2$i,'.',results2$j) %in% I2,]
tests2 <- tests2[paste0(tests2$i,'.',tests2$j) %in% I2,]
pval2 <- pval2[paste0(pval2$i,'.',pval2$j) %in% I2,]
config2 <- config2[paste0(config2$i,'.',config2$j) %in% I2,]

rm(I1,I2)

rbind(tests1$group,round(tests1$Hausman,4))
rbind(tests2$group,round(tests2$Hausman,4))

cbind(group = results1$group,
      const = round(results1$const,4),
      PER = round(results1$PER,4),
      CF = round(results1$CF,4),
      LNNAV = round(results1$LNNAV,4),
      LNAGEM = round(results1$LNAGEM,4),
      OAUM = round(results1$OAUM,4),
      ARAC = round(results1$ARAC,4)
      )










