rm(list=ls())
setwd("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione")
library(FLCore)
library(FLAssess)
#library(FLash)
library(ggplotFL)
library(FLasher)
library(FLBRP)
library(FLSRTMB)
library(stringr)
library(readxl)
library(FLRef)
library(purrr)
library(icesAdvice)
source('R/supporting_functions.R')

# global settings
base.dir='C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione'
TgrYr <-2030 # last year of forecast
int.period=2 # intermediate period, aka f status quo for each scenario
mp.starts=2025 # first year of management rule application
f.reduction.last=mp.starts+int.period # last year of management rule application
no_fbar_years <- 1 # number of years to be used for estimating f status quo
nits <- 250 # iteration for stochastic recruitment in a4a


# get data
stocks_info <- read_excel("data/stocks_info.xlsx")
stocks_info=stocks_info[stocks_info$area!='Canale',]
stocks_info=stocks_info[stocks_info$stock!='NEP_17_18',]
catch.data=readr::read_csv("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Stock Assessment/STECF/2022/data/catch.csv")

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
  weak.reduction=x.stock$weak_reduction
  if(weak.reduction>0){
    scenarios=c('Status quo','Fmsy transition','F weak' ,'Annual 3%','Annual 5%')
    scen.vector=c('s.quo','fmsy.transition','fmsy.weak', 'r3','r5')
  }else{
    scenarios=c('Status quo','Fmsy transition','Annual 3%','Annual 5%')
    scen.vector=c('s.quo','fmsy.transition', 'r3','r5')
  }
  
  # get ref points
  i.ftarget=as.numeric(x.stock$Fref)
  i.flbrp=FLPar(Ftgt=i.ftarget, Btgt=NA, Bthr=x.stock$bpa, Blim=x.stock$bpa*0.5)
  
  ## get proportion of italian OTB: you can replace this. What you need at the end is just a number
  i.catch=catch.data[catch.data$species==x.name,]
  i.catch=i.catch[i.catch$area %in% x.area,]
  i.catch=i.catch%>%
    dplyr::filter(!is.na(gear))%>%
    dplyr::filter(year%in%2020:2022, country!='JRC')%>%
    dplyr::mutate(gear=ifelse(gear=='TBB','OTB',gear))%>%
    dplyr::group_by(country, gear)%>%
    dplyr::summarise(landings=sum(landings))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(prop=landings/sum(landings))
  catch.prop=i.catch[i.catch$country=='ITA' & i.catch$gear=='OTB',][1,]$prop # this is one single number < 1
  
  
  # get data
  if(x.model=='a4a'){
    if(x.ewg=='stecf2309'){
      x.folder='data/Stock_Objs_2309'
      }else{
        x.folder='data/Stock_Objs_2312'
      }
    x.files=list.files(x.folder)
    if(x.stock$stock=='DPS_8_9_10_11'){
      stk=readRDS(file.path(x.folder, "DPS_8_9_10_11_sty.rds"))
    }else{
      x.files=x.files[grep('fin', x.files)]
      x.files=x.files[grep(x.stock$stock, x.files)]
      stk=readRDS(file.path(x.folder, x.files))
    }
    stk2 = FLStockR(stk)
    stk2@refpts=i.flbrp
    stk2@desc='a4a'
    stk@desc='a4a'
  }
  
  if(x.model=='spict'){
    x.files=list.files('data/spict_objs')
    x.files=x.files[grep(x.stock$stock, x.files)]
    stk.spict <- readRDS(paste0("data/spict_objs/",x.files))
    stk=spict2FLStockR(stk.spict, rel = TRUE)
    
  }
  
  if(x.model=='ss3'){
    stocks=c('HKE_17_18_benchmark','MUT_17_18_benchmark','NEP_17_18','SOL_17')
    ## add here your list of ITA OTB fleets
    if(x.stock$stock == 'NEP_17_18'){
      fleet.selection=c('OTB_Pomo_ITA_17','OTB_NoPomo_ITA_17','OTB_ITA_18')
    }
    if(x.stock$stock == 'HKE_17_18'){
      fleet.selection=c('ITA_OTB_17','ITA_OTB_18')
    }
    if(x.stock$stock == 'MUT_17_18'){
      fleet.selection=c('OTB18', 'OTB17')
    }
    if(x.stock$stock == 'SOL_17'){
      fleet.selection=c('TBB_ITA', 'OTB_ITA')
      scenarios=c('Fmsy transition','Annual 3%','Annual 5%','Status quo')
    }
    
  
  }
  
  # apply functions to create forecast
  if(x.model=='a4a'){
    source('R/forecast_a4a.R')
  }
  if(x.model=='spict'){
    source('R/forecast_spict.R')
  }
  if(x.model=='ss3'){
    source('R/forecast_ss3.R')
  }
  if(x.stock$stock=='SOL_17'){
    next
  }
  
  # plotting
  p1=plotAdvice(stk2)+ggtitle(paste(x.stock$stock, 'stock trajectory'))
  ggsave(plot=p1, paste0('results/plots/', x.stock$stock,'_stk.png'), 
         width = 19, height = 15,units='cm',dpi=500)
  
  p2=plotAdvice(window(stf.store, start = 2010)) + 
    scale_x_continuous(breaks = seq(2010, 2030, 2))+
    ggtitle(paste(x.stock$stock, 'forecasts'))+
    theme(legend.position = 'bottom')+
    annotate("rect", xmin = 2022, xmax = 2024, ymin = 0, ymax = Inf,
             alpha = .4,fill = "grey")
  ggsave(plot=p2, paste0('results/plots/', x.stock$stock,'_fwd.png'), 
         width = 19, height = 15,units='cm',dpi=500)
  
  if(x.model %in% c('a4a', 'ss3')){
     p3=plotAdvice(window(stf.stoch.store, start = 2010)) + 
    scale_x_continuous(breaks = seq(2010, 2030, 2))+
    ggtitle(paste(x.stock$stock, 'stochastic forecasts'))+
    theme(legend.position = 'bottom')+
    annotate("rect", xmin = 2022, xmax = 2024, ymin = 0, ymax = Inf,
             alpha = .4,fill = "grey")
   ggsave(plot=p3, paste0('results/plots/', x.stock$stock,'_fwd_stc.png'), 
         width = 19, height = 15,units='cm',dpi=500) 
  }

  # saving
  write.csv(stf_results, paste0('results/forecasts/', x.stock$stock,'_summary.csv'), row.names = F)
  saveRDS(stf.store, paste0('results/forecasts/', x.stock$stock,'_fwd.rds'))
  rm(stf_results)
}

rmarkdown::render('R/markdowns/summary_word.Rmd')
