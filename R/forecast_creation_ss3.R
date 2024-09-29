rm(list=ls())
library(r4ss)
library(ss3diags)
library(FLCore)
library(FLBRP)
library(FLasher)
library(FLSRTMB)
library(ggplotFL)
library(FLRef)
library(ggplot2)
#library(ss3om)
library(foreach)
library(doParallel)
library(FLRef)
base.dir='C:/Users/e.armelloni/OneDrive/Lezioni/Lavoro/Altro/piano_gestione'
setwd(base.dir)


# global functions ####
scenario.creation=function(f.scenario){
  envir = globalenv()
  # relative proportions of $F_{apic}$ by fleet and season can be computed
  #Fap = aggregate(Fapic~Seas+Fleet,fexp,mean)
  #Fap$prop = Fap$Fapic/sum(Fap$Fapic)*nseas
  ##Get the $F_{tgt}$ reference point, here defined as $F_{MSY}$. Therefore, the `annF_MSY` is extracted.
  #Fratio = mean(Fapic$Fapic[Fapic$Yr%in%max(bmyrs)]/Fbar$annual_F[Fbar$Yr%in%max(bmyrs)])
  #Fref = c("annF_Btgt","annF_MSY","annF_SPR")[2] 
  #Ftgt = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label==Fref] # Fmsy
  #Ftgt.apic = Ftgt*Fratio
  #Fcy=Fapic$Fapic[Fapic$Yr%in%max(bmyrs)]
  
  # slope of transitions
  a=(f.reduction.last-((mp.starts:f.reduction.last)))/((f.reduction.last)-as.numeric(mp.starts-1))
  (b=1-a)
  (F_transition.msy <- a*Fcy+b*Ftgt.apic)
  F_weak=Fcy*weak.reduction
  (F_transition.w <- a*Fcy+b*F_weak)
  
  #
  fvec=do.call("rbind", replicate(length(stf_years), Fsq, simplify = FALSE))
  fvec$year=rep(stf_years, each=nfleets*nseas)
  fvec$prop=Fap$prop
  
  for(x in 1:length(fleet.name)){
    if(fleet.name[x]%in%fleet.selection){
    if(f.scenario=='s.quo'){
      cat('Activated Fsq scenario. Nothing to do')
      new.f=fvec[fvec$Fleet==fleet.name[x] & fvec$year %in%mp.starts:f.reduction.last, ]$Fapic
    }
    if(f.scenario=='r3'){
      cat('Activated r3 scenario')
      new.f=f.reduction(red.period = length(mp.starts:f.reduction.last), perc.red = 3, f.ini=Fsq[Fsq$Fleet==fleet.name[x],]$Fapic,nseas=nseas)
    }
    if(f.scenario=='r5'){
      new.f=f.reduction(red.period = length(mp.starts:f.reduction.last), perc.red = 5, f.ini=Fsq[Fsq$Fleet==fleet.name[x],]$Fapic,nseas=nseas)
    }
    if(f.scenario=='fmsy.transition'){
      new.f=fvec[fvec$Fleet==fleet.name[x] & fvec$year %in%mp.starts:f.reduction.last, ]$prop*rep(F_transition.msy, each=nseas)
    }
    if(f.scenario=='fmsy.weak'){
      new.f=fvec[fvec$Fleet==fleet.name[x] & fvec$year %in%mp.starts:f.reduction.last, ]$prop*rep(F_transition.w, each=nseas)
    }
    fvec[fvec$Fleet==fleet.name[x] & fvec$year %in%mp.starts:f.reduction.last, ]$Fapic=new.f
    fvec[fvec$Fleet==fleet.name[x] & fvec$year>f.reduction.last, ]$Fapic=new.f[(length(new.f)-(nseas-1)):length(new.f)]
    }
  }
  return(fvec)
}
f.reduction=function(red.period,perc.red,f.ini,nseas){
  #seasons=length(f.ini)
  f.vec=f.ini*(1-(perc.red/100))
  for(x in 2:red.period){
    f.i=f.vec[(length(f.vec)-(nseas-1)):length(f.vec)]*(1-(perc.red/100))
    f.vec=c(f.vec,f.i)
  }
  return(f.vec)
}

# F based forecasts ####
## HKE MUT and NEP ####
# define function parameters
scen.vector=c('r3','r5','fmsy.transition','fmsy.weak','s.quo')
stocks=c('HKE_17_18_benchmark','MUT_17_18_benchmark','NEP_17_18')
nfyrs=8
mp.starts=2025
f.reduction.last=2027
weak.reduction=0.74 # based on HAKE
x.scen=1
x.stock=3
for(x.stock in 1:length(stocks)){
  stock=stocks[x.stock]
  if(stock == 'NEP_17_18'){
    fleet.selection=c('OTB_Pomo_ITA_17','OTB_NoPomo_ITA_17','OTB_ITA_18')
  }
  if(stock == 'HKE_17_18_benchmark'){
    fleet.selection=c('ITA_OTB_17','ITA_OTB_18')
  }
  if(stock == 'MUT_17_18_benchmark'){
    fleet.selection=c('OTB18', 'OTB17')
  }
  
  
  for(x.scen in 1:length(scen.vector)){
  setwd(base.dir)
  f.scenario=scen.vector[x.scen]
  ref.model = ifelse(stock == 'NEP_17_18',"ref_2022" ,"ref")
  # get data from reference run and create new folder
  setwd(file.path('data','raw_assessments', stock))
  ss3rep = SS_output(ref.model)
  dir.create("forecast",showWarnings = F)
  x.files=list.files(ref.model)
  dat =x.files[grep('.dat',x.files)]
  ctl =  x.files[grep('.ctl',x.files)]
  ss.exe = x.files[grep('.exe',x.files)]
  ftgtdir = file.path("forecast",f.scenario)
  SSnewrun(model=ref.model,dat=dat,ctl=ctl,newdir=ftgtdir ,ss.exe=ss.exe)
  
  # creating the new forecast
  #if(stock == 'NEP_17_18'){
    fc <- SS_readforecast(file.path(ftgtdir,'forecast.ss'),verbose = F) ## read the base file for
  #}else{
    #fc <- SS_readforecast(file.path(base.dir,'data','raw_assessments','base_forecast_v3_30_22_1.ss'),verbose = F) ## read the base
    
  #}
  Fexp = ss3rep$exploitation
  
  #Importantly, the `annual_F` are scaled to the F-basis (here $F_{bar}$), whereas fleet specific $F$ values are always given as $F_{apic}$
  #aggregate across seasons, by taking the `mean` and not the `sum`.
  #Next compute the corresponding annual $F_{bar}$ values from the `annual_F` 
  #To work out exact ratio between $F_{apic}$ and $F_{bar}$ so that it is consistent with the benchmark calculations with ss3, it is necessary   to extract the reference years for selectivity from the `forecast.ss` file. 
  Fexp$Fapic = apply(as.matrix(ss3rep$exploitation[,-c(1:6)]),1,sum,na.rm=T)
  Fapic = aggregate(Fapic~Yr,Fexp,mean) # this is needed if you have seasonal F
  Fbar = aggregate(annual_F~Yr,Fexp,mean)
  endyr = ss3rep$endyr
  if(stock=='NEP_17_18'){
    endyr=2022
  }
  stf_years=(endyr+1):(endyr+nfyrs)
  
  # get F status q
  nfsq = 1 
  fsq = ss3rep$exploitation[ss3rep$exploitation$Yr%in%((endyr-nfsq+1):endyr),] 
  fsq = cbind(fsq[,1:2],fsq[,-c(1:5)])[,-3]  #><>  single fleet trick
  fsq = reshape2::melt(fsq, id.vars = c("Yr", "Seas"),
                       variable.name = "Fleet", 
                       value.name = "Fapic")
  Fsq = aggregate(Fapic~Seas+Fleet,fsq,mean) ## this only serves in case you get Fsq from a vector of years
  
  # start modifications
  
  
  fc$Bmark_years=rep(c(2019,2021), length(fc$Bmark_years)/2) # period inform reference points
  fc$Fcast_years=rep(c(2019,2021), length(fc$Fcast_years)/2) # period inform biology

  fc$FirstYear_for_caps_and_allocations=endyr+nfyrs+1
  nfc = fc$Bmark_years[4]-fc$Bmark_years[3]+1 
  nseas = length(unique(ss3rep$exploitation$Seas)) # number of seasons
  fleets = unique(ss3rep$fatage$Fleet) # fleets
  nfleets = length(fleets) # number of fleet
  fleet.name=unique(Fsq$Fleet)
  # Benchmark reference years
  bmyrs = (endyr-nfc+1):endyr
  #`Fratio` defines the ratio of $F_{apic}$ to $F_{bar}$ for the reference period 
  
  # mean Fapic by fleet and season is calculated 
  fexp = ss3rep$exploitation[ss3rep$exploitation$Yr%in%endyr,] 
  fexp = cbind(fexp[,1:2],fexp[,-c(1:5)])[,-3] #><>  single fleet trick
  # flip
  fexp = reshape2::melt(fexp, id.vars = c("Yr", "Seas"),
                        variable.name = "Fleet", 
                        value.name = "Fapic")
  fleet = data.frame(Fleet =ss3rep$FleetNames,ID=ss3rep$fleet_ID)
  fexp$Fleet= fleet[match(fexp$Fleet,fleet$Fleet),2]
  
  #Now, the forecast horizon can be defined in the loaded `starter.ss` object `fc`.
  if(stock=='NEP_17_18'){
    fc$Nforecastyrs = 9 # one year ahead + extra
   }else{
     fc$Nforecastyrs = nfyrs # one year ahead + extra
  }
  nint = 2
  fyrs= endyr+c(1:nfyrs) 
  ## check ref points
  # relative proportions of $F_{apic}$ by fleet and season can be computed
  Fap = aggregate(Fapic~Seas+Fleet,fexp,mean)
  Fap$prop = Fap$Fapic/sum(Fap$Fapic)*nseas
  #Get the $F_{tgt}$ reference point, here defined as $F_{MSY}$. Therefore, the `annF_MSY` is extracted.
  Fratio = mean(Fapic$Fapic[Fapic$Yr%in%endyr]/Fbar$annual_F[Fbar$Yr%in%endyr])
  Fref = c("annF_Btgt","annF_MSY","annF_SPR")[1] 
  #if(stock%in%c('HKE_17_18_benchmark','MUT_17_18_benchmark')){
  #   fc$Btarget=0.35
  #}
  Ftgt = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label==Fref] # Fmsy
  cat(Fref, Ftgt)
  Ftgt.apic = Ftgt*Fratio
  Fcy=Fapic$Fapic[Fapic$Yr%in%endyr]
  
  ### set up forecast scenarios
  fvec=scenario.creation(f.scenario=f.scenario)
  write.csv(fvec, file.path(ftgtdir, paste0('scenario_effort.csv')), row.names = F )
  fvec=fvec$Fapic
  new.forecast=data.frame("Year"=rep(fyrs,each=nseas*nfleets),"Seas"=1:nseas,
                          "Fleet"=rep(fleets,each=nseas),
                          "Catch or F"=fvec,
                          "Basis"=99) 
  if(stock=='NEP_17_18'){
    
     one.year.ahead=fc$ForeCatch
     names(one.year.ahead)=names(new.forecast)
     one.year.ahead$Catch.or.F=fvec[1:nfleets]
     one.year.ahead$Basis=99
     new.forecast=rbind(one.year.ahead,
                     new.forecast) 
  }

  fc$ForeCatch = new.forecast
  
  p.scen=ggplot(new.forecast[new.forecast$Basis==99,])+
    geom_line(aes(x=Year, y=Catch.or.F, color=factor(Fleet)))+
    facet_wrap(~Seas)+
    ggtitle(f.scenario)
  ggsave(plot=p.scen, file=file.path(ftgtdir, paste0(f.scenario,'.png')), width = 15, height = 10, units='cm')
  
  fc$eof=TRUE 
  fc$InputBasis = -1
  SS_writeforecast(fc, file =file.path(ftgtdir, "forecast.ss"),overwrite = T)
  
  # check that the starter is ok
  starter = SS_readstarter(file =file.path(ftgtdir, "starter.ss"))
  starter$maxyr_sdreport = max(fc$ForeCatch$Year)
  SS_writestarter(starter, file =file.path(ftgtdir, "starter.ss"),overwrite = T)
  
  # only for .exe 3.30.22.1 if base rec forecast is set = 4
  #if(stock%in%c('HKE_17_18_benchmark','MUT_17_18_benchmark')){
  #   control=SS_readctl(file =file.path(ftgtdir, ctl), datlist =file.path(ftgtdir, dat) )
  #   control[["recdev_phase"]]=-1
  #   control[["MainRdevYrLast"]]=endyr 
  #   SS_writectl(control, outfile =file.path(ftgtdir, ctl),overwrite = T)
  #}
  }
  
  # Done. Ready to run!

  
  # Define the vector
  scen.vector = c('r3','r5','fmsy.transition','fmsy.weak','s.quo')
  
  # Number of cores to use for parallel execution
  numCores <- 5#parallel::detectCores() - 1  # Use one less core than available
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Run the parallel loop
  foreach(x.scen = 1:length(scen.vector), .packages = c("r4ss")) %dopar% {
    setwd(base.dir)  # Make sure base.dir is defined in your environment
    f.scenario <- scen.vector[x.scen]
    
    # Get data from reference run and create new folder
    ftgtdir <- file.path('data','raw_assessments', stock, "forecast", f.scenario)
    x.files <- list.files(ftgtdir)
    ss.exe <- x.files[grep('.exe', x.files)]
    
    setwd(ftgtdir)
    system(ss.exe)  # or you can use the commented line below
    # r4ss::run(ftgtdir, skipfinished = F, show_in_console = T, exe = ss.exe)
  }
  
  # Stop the cluster when done
  stopCluster(cl)
  
}

 


## SOL ####
setwd(base.dir)
scen.vector=c('r3','r5','fmsy.transition','s.quo')
stock='SOL_17'
ref.model = "ref"
nfyrs=8
mp.starts=2025
f.reduction.last=2027
weak.reduction=1
run.list=list.files(file.path('data','raw_assessments', stock,ref.model))
fleet.selection=c('TBB_ITA', 'OTB_ITA')
x.run=x.scen=1

for(x.run in 1:length(run.list)){
  ref.run=run.list[x.run]
  
  for(x.scen in 1:length(scen.vector)){
    setwd(base.dir)
    f.scenario=scen.vector[x.scen]
    
    # get data from reference run and create new folder
    setwd(file.path('data','raw_assessments', stock))
    ss3rep = SS_output(file.path(ref.model, ref.run))
    dir.create("forecast",showWarnings = F)
    dir.create(file.path("forecast", ref.run),showWarnings = F)
    
    x.files=list.files(file.path(ref.model, ref.run))
    dat =x.files[grep('data.ss',x.files)][1]
    ctl =  x.files[grep('control.ss',x.files)][1]
    ss.exe = x.files[grep('.exe',x.files)]
    ftgtdir = file.path("forecast",ref.run,f.scenario)
    SSnewrun(model=file.path(ref.model, ref.run),dat=dat,ctl=ctl,newdir=ftgtdir ,ss.exe=ss.exe)
    
    # creating the new forecast
    fc <- SS_readforecast(file.path(ftgtdir,'forecast.ss'),verbose = F)
    #fc <- SS_readforecast(file.path(base.dir,'data','raw_assessments','base_forecast_v3_30_22_0.ss'),verbose = F)
    #fc <- SS_readforecast(file.path(base.dir,'data','raw_assessments','base_forecast.ss'),verbose = F) ## read the base file for forecast
    Fexp = ss3rep$exploitation
    
    #Importantly, the `annual_F` are scaled to the F-basis (here $F_{bar}$), whereas fleet specific $F$ values are always given as $F_{apic}$
    #aggregate across seasons, by taking the `mean` and not the `sum`.
    #Next compute the corresponding annual $F_{bar}$ values from the `annual_F` 
    #To work out exact ratio between $F_{apic}$ and $F_{bar}$ so that it is consistent with the benchmark calculations with ss3, it is necessary   to extract the reference years for selectivity from the `forecast.ss` file. 
    Fexp$Fapic = apply(as.matrix(ss3rep$exploitation[,-c(1:6)]),1,sum,na.rm=T)
    Fapic = aggregate(Fapic~Yr,Fexp,mean) # this is needed if you have seasonal F
    Fbar = aggregate(annual_F~Yr,Fexp,mean)
    endyr = ss3rep$endyr
    stf_years=(endyr+1):(endyr+nfyrs)
    
    # get F status q
    nfsq = 1 
    fsq = ss3rep$exploitation[ss3rep$exploitation$Yr%in%((endyr-nfsq+1):endyr),] 
    fsq = cbind(fsq[,1:2],fsq[,-c(1:5)])[,-3]  #><>  single fleet trick
    fsq = reshape2::melt(fsq, id.vars = c("Yr", "Seas"),
                         variable.name = "Fleet", 
                         value.name = "Fapic")
    Fsq = aggregate(Fapic~Seas+Fleet,fsq,mean) ## this only serves in case you get Fsq from a vector of years
    
    # start modifications
    fc$Nforecastyrs = nfyrs # one year ahead + extra
    fc$Bmark_years=rep(c(2019,2021), length(fc$Bmark_years)/2) # period to get info for biology
    fc$Fcast_years=rep(c(2019,2021), length(fc$Fcast_years)/2) # also period to get info for biology
    fc$FirstYear_for_caps_and_allocations=endyr+nfyrs+1
    nfc = fc$Bmark_years[4]-fc$Bmark_years[3]+1 
    nseas = length(unique(ss3rep$exploitation$Seas)) # number of seasons
    fleets = unique(ss3rep$fatage$Fleet) # fleets
    nfleets = length(fleets) # number of fleet
    fleet.name=unique(Fsq$Fleet)
    # Benchmark reference years
    bmyrs = (endyr-nfc+1):endyr
    #`Fratio` defines the ratio of $F_{apic}$ to $F_{bar}$ for the reference period 
    
    # mean Fapic by fleet and season is calculated 
    fexp = ss3rep$exploitation[ss3rep$exploitation$Yr%in%endyr,] 
    fexp = cbind(fexp[,1:2],fexp[,-c(1:5)])[,-3] #><>  single fleet trick
    # flip
    fexp = reshape2::melt(fexp, id.vars = c("Yr", "Seas"),
                          variable.name = "Fleet", 
                          value.name = "Fapic")
    fleet = data.frame(Fleet =ss3rep$FleetNames,ID=ss3rep$fleet_ID)
    fexp$Fleet= fleet[match(fexp$Fleet,fleet$Fleet),2]
    
    #Now, the forecast horizon can be defined in the loaded `starter.ss` object `fc`.
    nint = 2 
    nfyrs = fc$Nforecastyrs
    fyrs= endyr+c(1:nfyrs) 
    
    ## check ref points
    # relative proportions of $F_{apic}$ by fleet and season can be computed
    Fap = aggregate(Fapic~Seas+Fleet,fexp,mean)
    Fap$prop = Fap$Fapic/sum(Fap$Fapic)*nseas
    #Get the $F_{tgt}$ reference point, here defined as $F_{MSY}$. Therefore, the `annF_MSY` is extracted.
    Fratio = mean(Fapic$Fapic[Fapic$Yr%in%endyr]/Fbar$annual_F[Fbar$Yr%in%endyr])
    Fref = c("annF_Btgt","annF_MSY","annF_SPR")[1] 
    
    if(stock%in%c('HKE_17_18_benchmark','MUT_17_18_benchmark')){
      fc$Btarget=0.35
    }
    Ftgt = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label==Fref] # Fmsy
    cat(Fref, Ftgt)
    Ftgt.apic = Ftgt*Fratio
    Fcy=Fapic$Fapic[Fapic$Yr%in%endyr]
    
    ### set up forecast scenarios
    fvec=scenario.creation(f.scenario=f.scenario)
    write.csv(fvec, file.path(ftgtdir, paste0('scenario_effort.csv')), row.names = F )
    fvec=fvec$Fapic
    #sum(fvec[fvec$year==2030,]$Fapic)
    # set up your scenario here
    fc$ForeCatch = data.frame("Year"=rep(fyrs,each=nseas*nfleets),"Seas"=1:nseas,
                              "Fleet"=rep(fleets,each=nseas),
                              "Catch or F"=fvec,
                              "Basis"=99)
    
    p.scen=ggplot(fc$ForeCatch)+
      geom_line(aes(x=Year, y=Catch.or.F, color=factor(Fleet)))+
      facet_wrap(~Seas)+
      ggtitle(f.scenario)
    ggsave(plot=p.scen, file=file.path(ftgtdir, paste0(f.scenario,'.png')), width = 15, height = 10, units='cm')
    
    
    fc$eof=TRUE 
    fc$InputBasis = -1
    
    SS_writeforecast(fc, file =file.path(ftgtdir, "forecast.ss"),overwrite = T)
    
    # check that the starter is ok
    starter = SS_readstarter(file =file.path(ftgtdir, "starter.ss"))
    starter$maxyr_sdreport = max(fc$ForeCatch$Year)
    SS_writestarter(starter, file =file.path(ftgtdir, "starter.ss"),overwrite = T)
    
  }
  
}


setwd(base.dir)
# Define the vector
scen.vector = c('r3','r5','fmsy.transition','s.quo')
run.list=list.files(file.path('data','raw_assessments', stock,'forecast'))

x.run=x.scen=1

for(x.run in 1:length(run.list)){
  ref.run=run.list[x.run]
  
  # Number of cores to use for parallel execution
  numCores <- 5#parallel::detectCores() - 1  # Use one less core than available
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Run the parallel loop
  foreach(x.scen = 1:length(scen.vector), .packages = c("r4ss")) %dopar% {
    setwd(base.dir)  # Make sure base.dir is defined in your environment
    f.scenario <- scen.vector[x.scen]
    
    # Get data from reference run and create new folder
    ftgtdir <- file.path('data','raw_assessments', stock, "forecast",ref.run, f.scenario)
    x.files <- list.files(ftgtdir)
    ss.exe <- x.files[grep('.exe', x.files)]
    
    setwd(ftgtdir)
    system(ss.exe)  # or you can use the commented line below
    # r4ss::run(ftgtdir, skipfinished = F, show_in_console = T, exe = ss.exe)
  }
  
  # Stop the cluster when done
  stopCluster(cl)
  
}



