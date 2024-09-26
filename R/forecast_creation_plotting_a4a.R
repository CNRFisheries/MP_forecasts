rm(list=ls())
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

ref.points <- read_excel("data/ref_points.xlsx") # example file in the data folder
ref.points=ref.points[ref.points$MP=='Tirreno',]
target=ref.points$stock
folder.stockobj='your dir'
catch.data=readr::read_csv("your dir") # fdi like file

# define some functions
f.reduction=function(red.period,perc.red,f.ini){
  f.vec=f.ini*(1-(perc.red/100))
  for(x in 2:red.period){
    f.i=f.vec[x-1]*(1-(perc.red/100))
    f.vec=c(f.vec,f.i)
  }
  return(f.vec)
}

rec.devs=function(stk){
  # mean and sd from a lognormal distribution
  rec.vec=log(as.data.frame(rec(stk))$data)
  mu.rec=mean(rec.vec)
  sd.vec=sd(rec.vec)
  m=exp(mu.rec+(0.5*sd.vec^2))
  s=exp(mu.rec+(0.5*sd.vec^2))*sqrt(exp(sd.vec^2)-1)
  return(c(m,s))
}

# global settings for forecast
# define fprecast settings
TgrYr <-2030
int.period=2
mp.starts=2025
f.reduction.last=2027
no_fbar_years <- 1
nits <- 250

### get an overview of f reductions needed to msy to identify weak species
ref.points$target.reduction=NA
for(i in 1:length(target)){
  i.brp=ref.points[ref.points$stock==target[i],]
  # get data
  x.files=list.files(folder.stockobj)  
  
  x.files=x.files[grep('final', x.files)]
  i.files=x.files[grep(target[i], x.files)]
  
  stk=readRDS(file.path(folder.stockobj, i.files))
  
  # get ref points
  f.vec=as.data.frame(fbar(stk))
  f.last=f.vec[nrow(f.vec),]$data
  target.reduction=as.numeric(i.brp$f01)/f.last
  ref.points[i,]$target.reduction=target.reduction
}
weak.species=ref.points[!is.na(ref.points$target.reduction),]
weak.species=weak.species[weak.species$target.reduction==min(weak.species$target.reduction),]
i=1
### do the forecasts
for(i in 1:length(target)){
  # get ref points and details
  i.brp=ref.points[ref.points$stock==target[i],]
  i.f01=as.numeric(i.brp$f01)
  i.flbrp=FLPar(Ftgt=i.brp$f01, Btgt=NA, Bthr=i.brp$bpa, Blim=i.brp$bpa*0.5)
  
  # get data
  x.files=list.files(folder.stockobj)  
  
  x.files=x.files[grep('final', x.files)]
  i.files=x.files[grep(target[i], x.files)]
  
  stk=readRDS(file.path(folder.stockobj, i.files))
  stk@desc=""
  i.name=str_remove(i.files,'_final.rds')
  i.area=substr(i.name,5,nchar(i.name))
  i.area=paste('GSA',as.character(str_split(i.area,'_', simplify = T)))
  
  # check stk and define some basic quantities
  max_yr_stk <- range(stk)["maxyear"]
  min_yr_stk <- range(stk)["minyear"]
  Yrbase <- as.numeric(max_yr_stk)+1
  stf_years=seq(from=(Yrbase),to=TgrYr, by=1)
  Fcy <- as.numeric(fbar(stk)[,as.character(max_yr_stk)])
  stk.duration=length(min_yr_stk:max_yr_stk)
  
  ## get proportion of italian OTB
  i.catch=catch.data[catch.data$species==substr(i.files,1,3),]
  i.catch=i.catch[i.catch$area %in% i.area,]
  i.catch=i.catch%>%
    dplyr::group_by(year, country, gear)%>%
    dplyr::summarise(landings=sum(landings))%>%
    dplyr::filter(year==2021)%>%
    dplyr::ungroup()%>%
    dplyr::mutate(prop=landings/sum(landings))
  catch.prop=i.catch[i.catch$country=='ITA' & i.catch$gear=='OTB',][1,]
  
  # set up for scenarios: status quo, Fmsy transition, -3%, -6%
  no_stk_years <- dim(rec(stk))[2]
  fbars <- fbar(stk)[,(no_stk_years - no_fbar_years + 1):no_stk_years]
  (fbar_status_quo <- mean(c(fbars)))
  fbar_sq_ita=fbar_status_quo*catch.prop$prop
  fbar_sq_oth=fbar_status_quo*(1-catch.prop$prop)
  
  ## s quo
  f.s.quo=rep(fbar_status_quo, length(stf_years)) # status quo
  
  ## transition
  a=(f.reduction.last-((mp.starts:f.reduction.last)))/((f.reduction.last)-as.numeric(mp.starts-1))
  (b=1-a)
  (F_transition <- a*fbar_status_quo+b*i.f01)
  F_transition=F_transition*catch.prop$prop+fbar_sq_oth
  f.transition=c(rep(fbar_status_quo, int.period), 
                 (F_transition),
                 rep(F_transition[length(F_transition)], length((f.reduction.last+1):TgrYr))) 
  
  ## 3%
  red.vector.3=f.reduction(red.period = length(mp.starts:f.reduction.last), perc.red = 3, f.ini=fbar_sq_ita)+fbar_sq_oth
  
  f.3=c(rep(fbar_status_quo, int.period),
        red.vector.3,
        rep(red.vector.3[length(red.vector.3)], length((f.reduction.last+1):TgrYr)))
  
  ## 5%
  red.vector.5=f.reduction(red.period = length(mp.starts:f.reduction.last), perc.red = 5, f.ini=fbar_sq_ita)+fbar_sq_oth
  
  f.5=c(rep(fbar_status_quo, int.period),
        red.vector.5,
        rep(red.vector.5[length(red.vector.5)], length((f.reduction.last+1):TgrYr)))
  
  # weak species
  F_weak=fbar_status_quo*weak.species$target.reduction
  (F_weak <- a*Fcy+b*F_weak)
  F_weak=F_weak*catch.prop$prop+fbar_sq_oth
  f.weak=c(rep(fbar_status_quo, int.period), 
           F_weak,
                 rep(F_weak[length(F_weak)], length((f.reduction.last+1):TgrYr))) 
  
  
  
  fbar_scenarios <- rbind(f.s.quo, f.transition, f.weak, f.3, f.5)
  
  # Set up the future stock object. Some tricks to ensure biology is estimated on 2019-2021
  stf_stk.0 <- stf(trim(stk, year=2019:2021), nyears = length(stf_years)+1, wts.nyears = 3) # (weights are means of the last 3 years) 
  stf_stk <- stf(stk, nyears = length(stf_years), wts.nyears = 3) # (weights are means of the last 3 years)
  slots_to_replace <-slotNames(stf_stk.0)[1:17]
  for (slot_name in slots_to_replace) {
    slot(stf_stk, slot_name)[,(stk.duration+1):(stk.duration+length(stf_years))]= slot(stf_stk.0,slot_name)[,5:(4+length(stf_years))]
  }
  spr0 = spr0y(stk)
  srr=srrTMB(as.FLSR(trim(stk, year=2019:2021), model = geomean), spr0 = spr0, nyears = 3) # Here we set as geometric mean of the last 3 years
  
  
  ## stocastic recruitment
  stk.stoch <- propagate(stf_stk,nits)
  residuals.mult <- FLQuant(NA,dimnames=list(age=1,year=stf_years),iter=nits)
  rec.variability=rec.devs(stk)
  rec.cv=rec.variability[2]/rec.variability[1]
  sdlog <- rec.cv
  residuals.mult[] <- rlnorm(prod(dim(residuals.mult)),meanlog=0,sdlog=sdlog)
  
  # Loop over the scenarios
  stf.store <- FLStocks()
  stf.stoch.store<- FLStocks()
  stf_results=NULL
  for (scenario in 1:nrow(fbar_scenarios)) {
    cat("Scenario: ", scenario, "\n")
    # Make a target object withe F values for that scenario
    ctrl_target <- data.frame(year = stf_years,
                              quant = "f",
                              value = fbar_scenarios[scenario,])
    # Set the control object - year, quantity and value for the moment
    ctrl_f <- fwdControl(ctrl_target)
    
    # deterministic forward projection
    stk_stf_fwd <- FLStockR(fwd(object=stf_stk, 
                       control = ctrl_f, 
                       sr = srr, 
                       maxF = 10.0))
    stk_stf_fwd@refpts=i.flbrp
    stf.store[[scenario]] <- stk_stf_fwd
    
    # stochastic forward projection
    res <- FLStockR(fwd(stk.stoch,
                        control =ctrl_f,
                        sr = srr, 
                        deviances=residuals.mult))
    res@refpts=i.flbrp
    stf.stoch.store[[scenario]] <- res
    
    # assemble results
    scen.res=data.frame(year=stf_years,
                        rec=as.numeric(rec(stk_stf_fwd)[,as.character(stf_years)]),
                        catch=as.numeric(catch(stk_stf_fwd)[,as.character(stf_years)]),
                        ssb=as.numeric(ssb(stk_stf_fwd)[,as.character(stf_years)]),
                        fbar=as.numeric(fbar(stk_stf_fwd)[,as.character(stf_years)]),
                        scenario=rownames(fbar_scenarios)[scenario],
                        stock=i.name,
                        rec.var=rec.cv)
    stf_results=rbind(stf_results, scen.res)
  
  }
  ### readjust for ITA catches
  fc.catch.ita=stf_results[stf_results$scenario=='f.s.quo',]$catch*catch.prop$prop
  fc.catch.tot=stf_results[stf_results$scenario=='f.s.quo',]$catch
  stf_results$catch_ita=NA
  stf_results[stf_results$scenario=='f.s.quo',]$catch_ita=fc.catch.ita
  for(j in 2:length(rownames(fbar_scenarios))){
     fc.catch.alt=stf_results[stf_results$scenario==rownames(fbar_scenarios)[j],]$catch
     ita.reduction=fc.catch.tot-fc.catch.alt
     stf_results[stf_results$scenario==rownames(fbar_scenarios)[j],]$catch_ita=fc.catch.ita-ita.reduction 
  }

  names(stf.store)=c('Status quo','Fmsy transition', 'F weak' ,'Annual 3%','Annual 5%')
  names(stf.stoch.store)=c('Status quo','Fmsy transition','F weak' ,'Annual 3%','Annual 5%')
  write.csv(stf_results, paste0('results/forecasts/', i.name,'_summary.csv'))
  saveRDS(stf.store, paste0('results/forecasts/', i.name,'_fwd.rds'))
  
  # plotting
  stk2 = FLStockR(stk)
  stk2@refpts=i.flbrp
  stk2@desc='a4a'
  p1=plotAdvice(stk2)+ggtitle(paste(i.name, 'stock trajectory'))
  ggsave(plot=p1, paste0('results/plots/', i.name,'_stk.png'), 
         width = 19, height = 15,units='cm',dpi=500)
  
  p2=plotAdvice(window(stf.store, start = 2010)) + 
    #geom_vline(xintercept = 2025, linetype = 2) +
    scale_x_continuous(breaks = seq(2010, 2030, 2))+
    ggtitle(paste(i.name, 'forecasts'))+
    theme(legend.position = 'bottom')+
    annotate("rect", xmin = 2022, xmax = 2024, ymin = 0, ymax = Inf,
             alpha = .4,fill = "grey")
  ggsave(plot=p2, paste0('results/plots/', i.name,'_fwd.png'), 
         width = 19, height = 15,units='cm',dpi=500)
  
  p3=plotAdvice(window(stf.stoch.store, start = 2010)) + 
    #geom_vline(xintercept = 2025, linetype = 2) +
    scale_x_continuous(breaks = seq(2010, 2030, 2))+
    ggtitle(paste(i.name, 'stochastic forecasts'))+
    theme(legend.position = 'bottom')+
    annotate("rect", xmin = 2022, xmax = 2024, ymin = 0, ymax = Inf,
             alpha = .4,fill = "grey")
  ggsave(plot=p3, paste0('results/plots/', i.name,'_fwd_stc.png'), 
         width = 19, height = 15,units='cm',dpi=500)
 
}



