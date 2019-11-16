# 2019-11-10
# 0 preliminary (not to publish)

# libraries
library(tidyverse)
library(data.table)


# loading the data with prices (quotations)
wanju <- fread('data//raw//WANJU.csv', dec=",")


# generating ID for mutual funds 
# (taking into account changing in investment policy)

FUN <- wanju %>%
  group_by(nazwa, `kod grupy`) %>%
  summarise() %>% 
  rowid_to_column('ID_FUN') %>%
  mutate(ID_FUN = ID_FUN+1000) %>%
  mutate(ID_FUN = substr(as.character(ID_FUN),2,4)) %>%
  mutate(ID_FUN = paste0('FUN',ID_FUN))

fwrite(FUN,'data//dict//ID_FUN.csv',sep = ';')

wanju <- left_join(wanju, FUN, by = c('nazwa', 'kod grupy') )

wanju$nazwa <- NULL
wanju$segment <- NULL
wanju$`kod segmentu` <- NULL
wanju$grupa <- NULL
wanju$`kod grupy` <- NULL
wanju <- wanju[,c(1,4,2,3)]

rm(FUN)


# generating ID for investment funds company

IFC <- wanju %>%
  group_by(firma) %>%
  summarise() %>% 
  rowid_to_column('ID_IFC') %>%
  mutate(ID_IFC = ID_IFC+100) %>%
  mutate(ID_IFC = substr(as.character(ID_IFC),2,3)) %>%
  mutate(ID_IFC = paste0('IFC',ID_IFC))

fwrite(IFC,'data//dict//ID_IFC.csv',sep = ';')

wanju <- left_join(wanju, IFC, by = 'firma')

wanju$firma <- NULL
wanju <- wanju[,c(4,1:3)]
names(wanju)[3:4] <- c('date','QT')
rm(IFC)



# save the data with prices (quotations) to next steps
fwrite(wanju, file = 'data//WANJU2.csv')
rm(wanju)