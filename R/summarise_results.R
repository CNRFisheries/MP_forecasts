rm(list=ls())
setwd("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione")
library(tidyverse)
base.dir='C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione/results/forecasts'
x.files=list.files(base.dir, pattern='.csv')
x.names=c('stock','scenario','year','rec','rec_var','ssb','fbar_tot','f_ita_otb','f_ita_tbb','catch_tot','catch_ita_otb','catch_ita_tbb')

store=NULL
for(i in 1:length(x.files)){
  xdat=read.csv(file.path(base.dir, x.files[i]))
  names(xdat)=x.names
  store=rbind(store, xdat)
}
store$catch_tot=round(store$catch_tot)
store$catch_ita_tbb=round(store$catch_ita_tbb)
store$catch_ita_otb=round(store$catch_ita_otb)
store$ssb=round(store$ssb)
store$rec=round(store$rec)
store$f_ita_otb=round(store$f_ita_otb, digits=3)
store$f_ita_tbb=round(store$f_ita_tbb, digits=3)
store$fbar_tot=round(store$fbar_tot, digits=3)

write.csv(xdat, 'results/forecast_results_summary_TIR_ADR_ION.csv', row.names = F)
