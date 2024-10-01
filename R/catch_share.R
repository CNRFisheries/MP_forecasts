rm(list=ls())
setwd("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione")
library(stringr)
library(readxl)
library(purrr)

# global settings
base.dir='C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione'

# get data
stocks_info <- read_excel("data/stocks_info.xlsx")
stocks_info=stocks_info[stocks_info$area!='Canale',]
stocks_info=stocks_info[stocks_info$stock!='NEP_17_18',]


fdi0=readr::read_csv("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Stock Assessment/Data/FDI/2023_FDI_Catches/Catches/FDI Catches by country2022.csv")
fdi1=readr::read_csv("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Stock Assessment/Data/FDI/2023_FDI_Catches/Catches/FDI Catches by country2021.csv")
fdi2=readr::read_csv("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Stock Assessment/Data/FDI/2023_FDI_Catches/Catches/FDI Catches by country2020.csv")

FDI=rbind(fdi0, fdi1,fdi2)

FDI$total_live_weight_landed=as.numeric(FDI$total_live_weight_landed)
FDI=FDI[!is.na(FDI$total_live_weight_landed),]
names(FDI)
names(catch.data)


report.catch=NULL
# apply functions
for(xx.stock in 1:nrow(stocks_info)){
  #for(xx.stock in 12:13){
  
  # get stock info ####
  x.stock=stocks_info[xx.stock,]
  x.model=x.stock$model
  x.area=x.stock$area
  x.ewg=x.stock$ewg
  x.name=substr(x.stock$stock,1,3)
  x.area=substr(x.stock$stock,5,nchar(x.stock$stock))
  x.area=paste('GSA',as.character(str_split(x.area,'_', simplify = T)))
 
  
  ## get proportion of italian OTB: you can replace this. What you need at the end is just a number
  i.catch=FDI[FDI$species==x.name,]
  i.catch=i.catch[i.catch$sub_region %in% str_remove(x.area, ' '),]
  i.catch=i.catch%>%
    dplyr::filter(!is.na(gear_type))%>%
    dplyr::filter(year%in%2020:2022, country!='JRC')%>%
    dplyr::mutate(gear=ifelse(gear_type=='TBB','OTB',gear_type))%>%
    dplyr::group_by(species, country, gear, year)%>%
    dplyr::summarise(landings=sum(total_live_weight_landed))%>%
    dplyr::group_by(species, country, gear)%>%
    dplyr::summarise(landings=mean(landings))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(prop=round(landings/sum(landings), digits=3))%>%
    dplyr::filter(prop>=0.01)
  i.catch$area=substr(x.stock$stock,5,nchar(x.stock$stock))
  catch.prop=i.catch[i.catch$country=='ITA' & i.catch$gear=='OTB',][1,]$prop # this is one single number < 1
  report.catch=rbind(report.catch, i.catch)
} 

write.csv(report.catch, 'data/catch_share_FDI.csv', row.names = F)





  