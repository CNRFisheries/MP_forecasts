## this set of functions creates and applies forecasts scenarios on stock objects. Please be aware that some uantities and values needed to use this script are loaded within the main workflow script.
## This script is based on the FLR functions used in STECF ewg and publicly available through the ewg Annexes.
  


  
  # check stk and define some basic quantities
  max_yr_stk <- range(stk)["maxyear"]
  min_yr_stk <- range(stk)["minyear"]
  Yrbase <- as.numeric(max_yr_stk)+1
  stf_years=seq(from=(Yrbase),to=TgrYr, by=1)
  Fcy <- as.numeric(fbar(stk)[,as.character(max_yr_stk)])
  stk.duration=length(min_yr_stk:max_yr_stk)
  
  # set up for scenarios: status quo, Fmsy transition, -3%, -6%
  no_stk_years <- dim(rec(stk))[2]
  fbars <- fbar(stk)[,(no_stk_years - no_fbar_years + 1):no_stk_years]
  (fbar_status_quo <- mean(c(fbars)))
  fbar_sq_ita=fbar_status_quo*catch.prop
  fbar_sq_oth=fbar_status_quo*(1-catch.prop)
  
  ## s quo
  f.s.quo=rep(fbar_status_quo, length(stf_years)) # status quo
  
  ## transition
  a=(f.reduction.last-((mp.starts:f.reduction.last)))/((f.reduction.last)-as.numeric(mp.starts-1))
  (b=1-a)
  (F_transition <- a*fbar_status_quo+b*i.ftarget)
  F_transition=F_transition*catch.prop+fbar_sq_oth
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
  F_weak=fbar_status_quo*weak.reduction
  (F_weak <- a*Fcy+b*F_weak)
  F_weak=F_weak*catch.prop+fbar_sq_oth
  f.weak=c(rep(fbar_status_quo, int.period), 
           F_weak,
                 rep(F_weak[length(F_weak)], length((f.reduction.last+1):TgrYr))) 
  
  
  if(weak.reduction>0){
   fbar_scenarios <- rbind(f.s.quo, f.transition, f.weak, f.3, f.5)
  }else{
   fbar_scenarios <- rbind(f.s.quo, f.transition, f.3, f.5) 
  }
  
  
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
    scen.res=data.frame(stock=x.stock$stock,
                        scenario=scenarios[scenario],
                        year=stf_years,
                        rec=round(as.numeric(rec(stk_stf_fwd)[,as.character(stf_years)])),
                        rec_var=round(rec.cv, digits=2),
                        ssb=round(as.numeric(ssb(stk_stf_fwd)[,as.character(stf_years)])),
                        fbar_tot=round(as.numeric(fbar(stk_stf_fwd)[,as.character(stf_years)]), digits=3),
                        fbar_ita_otb=NA,
                        fbar_ita_tbb=NA,
                        catch_tot=round(as.numeric(catch(stk_stf_fwd)[,as.character(stf_years)])))
    stf_results=rbind(stf_results, scen.res)
  
  }
  ### check both what happen with s quo and when f<fmsy
  fc.catch.ita=stf_results[stf_results$scenario=='Status quo',]$catch_tot*catch.prop
  fc.catch.tot=stf_results[stf_results$scenario=='Status quo',]$catch_tot
  stf_results$catch_ita_otb=NA
  stf_results[stf_results$scenario=='Status quo',]$catch_ita_otb=fc.catch.ita
  for(j in 2:length(scenarios)){
     fc.catch.alt=stf_results[stf_results$scenario==scenarios[j],]$catch_tot
     if(fc.catch.alt[8]>fc.catch.tot[8]){
      ita.increase=fc.catch.alt-fc.catch.tot
      stf_results[stf_results$scenario==scenarios[j],]$catch_ita_otb=fc.catch.ita+ita.increase 
     }else{
     ita.reduction=fc.catch.tot-fc.catch.alt
     stf_results[stf_results$scenario==scenarios[j],]$catch_ita_otb=fc.catch.ita-ita.reduction 
     }

  }
  stf_results$fbar_ita_otb=round(stf_results$fbar_tot-fbar_sq_oth, digits=3)
  stf_results$catch_ita_otb=round(stf_results$catch_ita_otb)
  names(stf.store)=scenarios
  names(stf.stoch.store)=scenarios
  ref.yr.tab=data.frame(stock=x.stock$stock,
                        scenario='reference_year', year=2022, 
                        rec=as.numeric(rec(stk))[length(rec(stk))],
                        rec_var=NA,
                        ssb=as.numeric(ssb(stk))[length(ssb(stk))],
                        fbar_tot=as.numeric(fbar(stk))[length(fbar(stk))],
                        fbar_ita_otb=fbar_sq_ita,
                        fbar_ita_tbb=NA,
                        catch_tot=as.numeric(catch(stk))[length(catch(stk))],
                        catch_ita_otb=as.numeric(catch(stk))[length(catch(stk))]*catch.prop)
  stf_results=rbind(ref.yr.tab, stf_results)
  stf_results$catch_ita_tbb=NA
