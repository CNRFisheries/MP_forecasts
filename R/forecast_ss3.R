library(r4ss)
library(ss3diags)
stock=stocks[grep(x.name, stocks)]
ref.dir=paste0("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione/data/raw_assessments/", stock)
forecast.dir=paste0("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione/data/raw_assessments/", stock, '/forecast')

## SOL ####
if(stock=='SOL_17'){
  weight_vector <- unname(unlist(readr::read_csv("C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione/data/raw_assessments/SOL_17/weight_vector.csv")[1,]))
  
  ## get stock trajectory plot
  ref.dir=file.path(ref.dir, 'ref')
  runs=list.files(ref.dir)
  
  kbproj.ref = NULL
  # Compile MVLN posteriors by scenario run
  for(i in 1:length(runs)){
    run = SSgetoutput(dirvec=file.path(ref.dir,runs[i]))
    mvn.temp = SSdeltaMVLN(run[[1]],addprj = T,mc=500,weight = weight_vector[i][[1]],Fref = "Btgt",plot=F)
    kbproj.ref = rbind(kbproj.ref,data.frame(mvn.temp$kb,model=runs[i]))
  } 
  
  kb = kbproj.ref[kbproj.ref$year==2022,]
  bmsy =median(kb$SSB)/median(kb$stock)
  fmsy =median(kb$F)/median(kb$harvest)
  
  labels <- expression("Recruitment", "SSB", 
                       "F", "Landings")
  sspar(mfrow=c(2,2),plot.cex = 0.9)
  quants = c("Recr","SSB", "F",  "Catch")
  for(i in c(1:4)){
    SSplotEnsemble(kbproj.ref,add=T,  quantiles = c(0.05, 0.95),shadealpha = 0.05,subplots = quants[i],ylabs=labels[i],
                   legendcex = 0.8,legendsp = 0.3, legendloc = "bottomleft",legend=ifelse(i==1,T,F), col='black')
    abline(h=fmsy, lty = 2)
    
  }
  dev.print(jpeg,paste0('results/plots/', x.stock$stock,'_stk.png'), width = 19, height = 15,units='cm',res=500) 
  
  ## forecast
  sol.scenario=c(list.files(file.path(forecast.dir,runs[1])))
  ts.store=list()
  kbproj.fr = NULL
  # Compile MVLN posteriors by scenario run
  for(i in 1:length(runs)){
  # load all scenarios as list  
  run = SSgetoutput(dirvec=file.path(forecast.dir, runs[i],sol.scenario))  
  # get MVLN mvn.temp for each scenario
  i.ts=get.timeseries(forecast.data = run, stock=x.stock$stock, scenario.vec = sol.scenario)
  i.ts$weigth=weight_vector[i]
  ts.store[[i]]=i.ts
  for(j in 1:length(sol.scenario)){  
    # this is a super long projection so I decrease mc to 1000 to speed up 
    # Make sure you set correct Fref depending on your choice
    # Make sure you name run = according to scenario
    mvn.temp = SSdeltaMVLN(run[[j]],run=sol.scenario[j],years = run[[j]]$startyr:run[[j]]$endyr+run[[j]]$nforecastyears,addprj = T,mc=500,weight = weight_vector[i],Fref = "Btgt",plot=F)
    # build kbproj.fr and add model name [j] for each TAC i 
    kbproj.fr = rbind(kbproj.fr,data.frame(mvn.temp$kb,model=runs[i]))
    # save labels once
    if(i==1 & j ==1) labels = mvn.temp$labels
    } # end j loop
  } # end k loop


  zoom <- c(2010,2030) # put the range you want to visualize in the forecast plot (zoom) 
  labels <- c("Recruitment", "SSB", 
                       "F", "Landings")
  ens.scenario=c('Fmsy transition','Annual 3%','Annual 5%','Status quo')
  sspar(mfrow=c(2,2),plot.cex = 0.9)
  quants = c("Recr","SSB", "F",  "Catch")
  for(i in c(1:4)){
    max.y=max(kbproj.fr[,quants[i]])
    SSplotEnsemble(kbproj.fr,add=T, xlim=zoom, quantiles = c(0.05, 0.95),shadealpha = 0.05,subplots = quants[i],ylabs=labels[i],
                   legendcex = 0.8,legendsp = 0.3,legendlabels=ens.scenario, legendloc = "bottomleft",legend=ifelse(i==1,T,F))
    polygon(x=c(2022,2024,2024,2022),y=c(0,0,max.y,max.y),col=rgb(0.4,0.4,0.4,0.25),border=NA)
    abline(h=fmsy, lty = 2)
  }
  dev.print(jpeg,paste0('results/plots/', x.stock$stock,'_fwd.png'), width = 19, height = 15,units='cm',res=500) 
  
  
  
  ## tables
  ts.tab=plyr::ldply(ts.store)
  #median(ts.tab$ratio_apic)
  #plot(ts.tab[ts.tab$year%in%2022:2024,]$fbar_tot)
  cat.tot=aggregate(catch_tot~year+scenario,ts.tab,median) 
  cat.ita=aggregate(catch_ita_otb~year+scenario,ts.tab,median) 
  cat.ita.tbb=aggregate(catch_ita_tbb~year+scenario,ts.tab,median) 
  
  f.tot=aggregate(fbar_tot~year+scenario,ts.tab,median)$fbar_tot
  f.ita.prop=aggregate(fbar_ita_otb~year+scenario,ts.tab,median)
  f.ita.tbb.prop=aggregate(fbar_ita_tbb~year+scenario,ts.tab,median)
  
  
  stf_tab=kbproj.fr[kbproj.fr$year%in%2022:2030, ]
  stf_tab$scenario=NA
  stf_tab[stf_tab$run=="fmsy.transition",]$scenario='Fmsy transition'
  stf_tab[stf_tab$run=="r3",]$scenario="Annual 3%" 
  stf_tab[stf_tab$run=="r5",]$scenario="Annual 5%" 
  stf_tab[stf_tab$run=="s.quo",]$scenario="Status quo"
  
  #prj_F_Ftrg = aggregate(harvest~year+scenario,stf_tab,median) 
  prj_SSB = aggregate(SSB~year+scenario,stf_tab,median) 
  prj_F = aggregate(F~year+scenario,stf_tab,median)
  prj_F$F=round(prj_F$F, digits=3)
  prj_Recr = aggregate(Recr~year+scenario,stf_tab,median)
  prj_Catch = aggregate(Catch~year+scenario,stf_tab,median) 
  
  i.res=merge(merge(merge(merge(merge(prj_Recr,cat.tot),prj_SSB),prj_F),cat.ita), cat.ita.tbb)
  names(i.res)=c('year','scenario','rec','catch_tot','ssb','fbar_tot','catch_ita_otb', 'catch_ita_tbb')
  i.res$stock=stock
  i.res$rec.var=NA
  i.res=merge(i.res,f.ita.prop)
  i.res=merge(i.res,f.ita.tbb.prop)
  i.res$fapic_ita_tbb=i.res$fbar_ita_tbb
  i.res$fapic_ita_otb=i.res$fbar_ita_otb
  
  i.res=i.res[,c('stock','scenario','year','rec','rec.var','ssb','fbar_tot','fapic_ita_otb','fapic_ita_tbb', 'catch_tot','catch_ita_otb','catch_ita_tbb')]
  i.res$rec=round(i.res$rec)
  i.res$ssb=round(i.res$ssb)
  i.res$catch_tot=round(i.res$catch_tot)
  i.res$catch_ita_otb=round(i.res$catch_ita_otb)
  i.res$catch_ita_tbb=round(i.res$catch_ita_tbb)
  i.res$fbar_tot=round(i.res$fbar_tot, digits=3)
  i.res$fapic_ita_otb=round(i.res$fapic_ita_otb, digits=3)
  i.res$fapic_ita_tbb=round(i.res$fapic_ita_tbb, digits=3)
  i.res=i.res%>%dplyr::arrange(scenario,year)
  
  ref.yr=i.res[i.res$year==2022,]
  ref.yr=ref.yr[1,]
  ref.yr$scenario='reference_year'
  i.res=rbind(ref.yr ,i.res[i.res$year>2022,])
  
  write.csv(i.res, paste0('results/forecasts/', x.stock$stock,'_summary.csv'), row.names = F)
  
}



if(stock!='SOL_17'){
  
  stk.name=stringr::str_remove(stock,'_benchmark')
  
  if(stock=='NEP_17_18'){
  run=file.path(ref.dir,'ref_2022')
  ss3rep=r4ss::SS_output(run)
  mvn=ssmvln(ss3rep = ss3rep,
           Fref='Btgt',
           addprj=T)
  
}else{
  run=file.path(ref.dir,'ref')
  ss3rep=r4ss::SS_output( run)
  mvn=ssmvln(ss3rep = ss3rep,
             Fref='Btgt',
             addprj=F)
}
    
    mvn$refpts
    mvn$refpts[1, ] # this corresponds to the stecf report. So cool!
    stk2 = ss2FLStockR(mvn)
    stk2@refpts = rbind(stk2@refpts, FLPar(Blim = stk2@refpts[[2]] * 0.25, Bthr = stk2@refpts[[2]] * 0.5))
    # remove Blim
    stk2@refpts = stk2@refpts[-4]
    stk2@refpts
    
    
  # forecasts
    
    fcs=SSgetoutput(dirvec =file.path(forecast.dir,scen.vector))
    fstks = FLStocks(Map(function(x, y) {
      run = ss2FLStockR(ssmvln(x, Fref = "Btgt", addprj = TRUE))
      run@refpts = stk2@refpts
      run@name = y
      return(run)
    }, x = fcs, y = scenarios))
    names(fstks) = scenarios
    stf.store=fstks
    
    stf.stoch.store = FLStocks(Map(function(x, y) {
      run = ss2FLStockR(ssmvln(x, Fref = "Btgt", addprj = TRUE), output='iters')
      run@refpts = stk2@refpts
      run@name = y
      return(run)
    }, x = fcs, y = scenarios))
    names(stf.stoch.store) = scenarios
    
    ## extract timeseries
    stf_results=get.timeseries(forecast.data = fcs, stock=stock, scenario.vec =scenarios )
    names(stf_results)
    ref.yr=stf_results[stf_results$year==2022,]
    ref.yr=ref.yr[1,]
    ref.yr$scenario='reference_year'
    stf_results=rbind(ref.yr ,stf_results[stf_results$year>2022,])
    stf_results=stf_results[,-which(colnames(stf_results)=='ratio_apic')]
    
}


  








