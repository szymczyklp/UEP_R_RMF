# £ukasz Szymczyk
# 2019-12-06
# encoding: UTF-8
# 2.1 regresion 1 (eq10) ==========================================================================


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


# DICTS ===========================================================================================

K <- c('OLS','LSDV','FE','RE','FEt')
G <- c('AKPca', 'MIPca','AKP','AKZ','MIP','MIZ','PDP','PDZ','RPP','ARN')


# TABLES1 ==========================================================================================

results1 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0, ARAC = 0,
  rsq = 0, adjrsq = 0, const = 0
)

pval1 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0, ARAC = 0
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

results2 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0,
  rsq = 0, adjrsq = 0, const = 0
)

pval2 <- tibble(  
  i = 0, j = 0, k =0, group = 'kod_grupy', model = 'kod_modelu',
  PER = 0
)

tests2 <- tibble(  
  i = 0, j = 0, group = 'kod_grupy',
  Hausman = 0, Ftime = 0, LM = 0, BGW = 0
)

row.names(config2) <- '0.0'
row.names(results2) <- '0.0.0'
row.names(pval2) <- '0.0.0'
row.names(tests2) <- '0.0'


# FORMULAS ========================================================================================

formula1 <- formula('FEE ~ PER + ARAC')
formula1d <- formula('FEE ~ PER + ARAC + factor(fund) -1')
formula1t <- formula('FEE ~ PER + ARAC + factor(month)')
formula1f <- formula('FEE ~ PER + ARAC + factor(fund)')

formula2 <- formula('FEE ~ PER')
formula2d <- formula('FEE ~ PER + factor(fund) -1')
formula2t <- formula('FEE ~ PER + factor(month)')
formula2f <- formula('FEE ~ PER + factor(fund)')


#start ============================================================================================
#i=1
for(i in 1:length(G)){
  j <- 0
  group <- G[i]
  
  # loading the data
  panel <- read.xlsx('data//raw//sample5_sample.xlsx',  sheet = group)
  panel <- as_tibble(panel)
  #str(panel)
  panel <- panel[,c(1:2,9:11,16)]
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
  names(panel1) <- c('fund','month','FEE','PER','ARAC')
  
  miary2<- names(panel)[c(4,5)]
  panel2 <- panel[,-c(3,6)]
  names(panel2) <- c('fund','month','FEE','PER') 
  
  #str(panel)
  #summary(panel)
  
  # ARMF -----
  
  j <- 0
    
    for(k in 1:5){
      results1[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
      pval1[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
    }
    
    tests1[paste0(i,'.',j),c(1:3)] <- c(i,j,group)
    
    mod_OLS <- lm (formula1, data = panel1) # k=1
    mod_LSDV <- lm(formula1d, data= panel1) # k=2
    mod_FE <- plm (formula1, data = panel1, index = c('fund','month'), model = 'within') # k=3
    mod_RE <- plm (formula1, data = panel1, index = c('fund','month'), model = 'random') # k=4
    mod_FEt <-  plm (formula1t, data = panel1, index = c('fund','month'), model = 'within') #k=5
    
    tests1[paste0(i,'.',j), 'Hausman'] <- phtest(mod_FE, mod_RE)$p.value #If this number is < 0.05 then use FE
    tests1[paste0(i,'.',j), 'Ftime'] <- pFtest(mod_FE, mod_OLS)$p.value # Testing FE, null: OLS better than FE (p-value is < 0.05 then FE is a better choice)
    tests1[paste0(i,'.',j), 'LM'] <- pcdtest(mod_FE, test = c("lm"))$p.value #p-value is < 0.05 then we have cross-sectional dependence 
    tests1[paste0(i,'.',j), 'BGW'] <- pbgtest(mod_FE)$p.value #null is that there is not serial correlation (p > 0.05)
    

    for(l in 6:7){
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
  
  # TOC -----

  j <- 0
    
    for(k in 1:5){
      results2[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
      pval2[paste0(i,'.',j,'.',k),c(1:5)] <-  c(i,j,k,group,K[k])
    }
    
    tests2[paste0(i,'.',j),c(1:3)] <- c(i,j,group)
    
    mod_OLS <- lm (formula2, data = panel2) # k=1
    mod_LSDV <- lm(formula2d, data = panel2) # k=2
    mod_FE <- plm (formula2, data = panel2, index = c('fund','month'), model = 'within') # k=3
    mod_RE <- plm (formula2, data = panel2, index = c('fund','month'), model = 'random') # k=4
    mod_FEt <-  plm (formula2t, data = panel2, index = c('fund','month'), model = 'within') #k=5
    
    tests2[paste0(i,'.',j), 'Hausman'] <- phtest(mod_FE, mod_RE)$p.value #If this number is < 0.05 then use FE
    tests2[paste0(i,'.',j), 'Ftime'] <- pFtest(mod_FE, mod_OLS)$p.value # Testing FE, null: OLS better than FE (p-value is < 0.05 then FE is a better choice)
    tests2[paste0(i,'.',j), 'LM'] <- pcdtest(mod_FE, test = c("lm"))$p.value #p-value is < 0.05 then we have cross-sectional dependence 
    tests2[paste0(i,'.',j), 'BGW'] <- pbgtest(mod_FE)$p.value #null is that there is not serial correlation (p > 0.05)
    
    results2[paste0(i,'.',j,'.1'),names(results2)[6]] <- mod_OLS$coefficients[names(results2)[6]]
    results2[paste0(i,'.',j,'.2'),names(results2)[6]] <- mod_LSDV$coefficients[names(results2)[6]]
    results2[paste0(i,'.',j,'.3'),names(results2)[6]] <- mod_FE$coefficients[names(results2)[6]]
    results2[paste0(i,'.',j,'.4'),names(results2)[6]] <- mod_RE$coefficients[names(results2)[6]]
    results2[paste0(i,'.',j,'.5'),names(results2)[6]] <- mod_FEt$coefficients[names(results2)[6]]
    pval2[paste0(i,'.',j,'.1'),names(results2)[6]] <- summary(mod_OLS)$coefficients[,4][names(results2)[6]]
    pval2[paste0(i,'.',j,'.2'),names(results2)[6]] <- summary(mod_LSDV)$coefficients[,4][names(results2)[6]]
    pval2[paste0(i,'.',j,'.3'),names(results2)[6]] <- summary(mod_FE)$coefficients[,4]
    pval2[paste0(i,'.',j,'.4'),names(results2)[6]] <- summary(mod_RE)$coefficients[,4][names(results2)[6]]
    pval2[paste0(i,'.',j,'.5'),names(results2)[6]] <- summary(mod_FEt)$coefficients[,4][names(results2)[6]]
      
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
  
  
  rm(miary1,miary2,PVALU,del,z,j)
  
}

results1 <- results1[results1$model == 'FE',]
pval1 <- pval1[pval1$model == 'FE',]

pval1$PER < 0.01
pval1$ARAC < 0.01

results2 <- results2[results2$model == 'FE',]
pval2 <- pval2[pval2$model == 'FE',]

round(results2$const *100,2)
round(results2$PER,4)
round(results2$rsq,4)

pval2$PER < 0.01


