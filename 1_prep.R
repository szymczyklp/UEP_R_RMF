# 2019-11-11
# 1 data preparation

# libraries
library(tidyverse)
library(data.table)

#library(PerformanceAnalytics)
#library(zoo)
#library(TTR)
library(quantmod)
library(xts)


# loading the data with prices (quotations)
wanju <- fread('data//WANJU2.csv', dec=".")


# checking data format

wanju <- as.tibble(wanju)

str(wanju)
wanju$ID_FUN <- as.factor(wanju$ID_FUN)
wanju$ID_IFC <- as.factor(wanju$ID_IFC)
wanju$date <- as.Date(wanju$date, '%Y-%m-%d')
str(wanju)
wanju

fwrite(wanju,'data//WANJU2.csv',sep = ';')

wanju$ID_IFC <- NULL

 wanju <- wanju %>%
   spread(ID_FUN,QT)


# prepare months vectors
 
returns_d <- wanju %>%
  group_by(date) %>%
  summarise() %>%
  arrange(date)
  returns_d <- returns_d[-1,]
 
dmin <- min(returns_d$date)
dmax <- max(returns_d$date)

returns_m <- as.Date(paste(year(dmin),month(dmin),'1',sep='-'))-1

omin <- year(dmin)*12+month(dmin)
omax <- year(dmax)*12+month(dmax)
range <-  omax - omin +1

for(i in 1:range){
  d <- i+omin
  returns_m <- c(returns_m,
                 as.Date(paste(if(d%%12==0){(d-(d%%12))/12-1}else{(d-(d%%12))/12},
                               if(d%%12==0){12}else{d%%12},'1',sep='-'))-1)
  rm(d)
}

rm(dmax,dmin,omax,omin,range,i)
returns_m <- as.tibble(returns_m)

names(returns_m) <- c('month')
returns_m <- returns_m[-1,]

returns_m <- returns_m %>%
  mutate(month = case_when(month(month) < 10 ~ paste0(year(month),'-0',month(month)),
                         TRUE ~ paste0(year(month),'-',month(month))))

mtest <- wanju$date %>% 
  as.tibble() %>% 
  cbind(n = 1)
mtest <- xts(mtest$n, mtest$value)
mtest <- monthlyReturn(mtest, leading = FALSE) %>%
  na.omit() 
mtest <- index (mtest)
  

# rate of returns (arythmetic)
 
d <- dim(wanju)[2]

for(i in 2:d){
  
  FUN <- wanju[,c(1,i)] %>%
    na.omit()
  names(FUN)[2] <- 'prices'
  ts <- xts(FUN$prices,FUN$date)
  
  dr <- dailyReturn(ts, leading = FALSE)
  dr <- tibble(date=index(dr), coredata(dr))
  names(dr)[2] <- names(wanju)[i]
  returns_d <- left_join(returns_d, dr, by = 'date')
  
  mr <- monthlyReturn(ts, leading = FALSE)
  mr <- tibble(date = index(mr), coredata(mr))
  names(mr)[2] <- names(wanju)[i]
  mr <- mr %>%
    mutate(month = case_when(month(date) < 10 ~ paste0(year(date),'-0',month(date)),
                           TRUE ~ paste0(year(date),'-',month(date))))
  mr <- mr[mr$date %in% mtest,] 
  #check case with end of operation at eom when last working day in given month is not the last calendar day
  mr$date <- NULL
  returns_m <- left_join(returns_m, mr, by = 'month')
  
  rm(FUN,ts,dr,mr)
  
}
rm(i,d,mtest)

returns_d <- gather(returns_d, key = "IF", value = 'DAROR', c(-1), na.rm = TRUE)
returns_m$month <- NULL
returns_m <- gather(returns_m, key = "IF", value = 'MAROR', c(-1), na.rm = TRUE)