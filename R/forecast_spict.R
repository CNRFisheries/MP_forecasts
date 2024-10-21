## this set of functions creates and applies forecasts scenarios on spict objects. Please be aware that some quantities and values needed to use this script are loaded within the main workflow script.
## This script is based on the standard spict functions
  library(spict)
  # set up for scenarios: status quo, Fmsy transition, -3%, -6%
  if(x.name=='DPS'){stk=trim(stk, year=2000:2022)}
  no_stk_years <- dim(catch(stk))[2]
  #fbars <- fbar(stk)[,(no_stk_years - no_fbar_years + 1):no_stk_years]
  fbars <-as.data.frame(exp(get.par('logF', stk.spict)))
  fbars$year=as.numeric(rownames(fbars))
  if(x.name=='DPS'){fbars=fbars[fbars$year==2022.75,]$est}
  if(x.name=='MUT'){fbars=fbars[fbars$year==2021.75,]$est}
  
  
  ## s quo
  f.s.quo=(fbar_status_quo <- mean(c(fbars)))
  fbar_sq_ita=fbar_status_quo*catch.prop
  fbar_sq_oth=fbar_status_quo*(1-catch.prop)
  
  ## fmsy
  Fmsy = exp(get.par("logFmsy", stk.spict)[2])
  if(x.name=='DPS'){Fmsy==0.937}
  f.transition=Fmsy*catch.prop+fbar_sq_oth
  
  ## 3%
  red.vector.3=f.reduction(red.period = length(mp.starts:f.reduction.last), perc.red = 3, f.ini=fbar_sq_ita)[3]+fbar_sq_oth
  
  ## 5%
  red.vector.5=f.reduction(red.period = length(mp.starts:f.reduction.last), perc.red = 5, f.ini=fbar_sq_ita)[3]+fbar_sq_oth
  
  # weak species
  F_weak=fbar_status_quo*weak.reduction
  f.weak=F_weak*catch.prop+fbar_sq_oth
  
  ## do scenarios
  man = manage(stk.spict, maninterval = c(2025, 2030), maneval = 2030, scenarios = c(2))
  sman = add.man.scenario(man, "Fmsy", ffac =  f.transition/f.s.quo, maninterval = c(2025, 2030), maneval = 2030)
  if(weak.reduction>0){
      sman = add.man.scenario(sman, "F weak", ffac =  f.weak/f.s.quo, maninterval = c(2025, 2030), maneval = 2030)
  }
  sman = add.man.scenario(sman, "Annual 3%", ffac =  red.vector.3/f.s.quo, maninterval = c(2025, 2030), maneval = 2030)
  sman = add.man.scenario(sman, "Annual 5%", ffac =  red.vector.5/f.s.quo, maninterval = c(2025, 2030), maneval = 2030)

  
  fstks = spict2FLStockR(sman, rel = F)
  names(fstks)=scenarios
  fstks[[1]]@refpts
  # make table
  fc.catch.ita=as.data.frame(fstks[[1]]@catch)
  fc.catch.ita=fc.catch.ita[fc.catch.ita$year>=2023,]
  fc.catch.ita$data=fc.catch.ita$data*catch.prop
  fc.catch.ita=fc.catch.ita$data
  fc.catch.tot=as.data.frame(fstks[[1]]@catch)
  fc.catch.tot=round(fc.catch.tot[fc.catch.tot$year>=2023,]$data)
  
  get.B.ts=lapply(sman$man, function(res) {
    biom.ts=as.data.frame(exp(get.par('logB',res)))
    biom.ts$year=floor(as.numeric(rownames(biom.ts)))
    biom.ts=biom.ts%>%
      dplyr::group_by(year)%>%
      dplyr::summarise(tot_b=round(mean(est)))
    
    return(biom.ts)
  })
  B.ts=plyr::ldply(get.B.ts, .id='scenario')
  B.ts$scenario=as.character(B.ts$scenario)
  B.ts[B.ts$scenario=='currentF',]$scenario='Status quo'
  B.ts[B.ts$scenario=='Fmsy',]$scenario="Fmsy transition"
  
  
  stf_results=NULL
  for(j in 1:length(fstks)){
    j.stk=fstks[[j]]
    fstks[[j]]@refpts[1]=Fmsy
    fc.catch=as.data.frame(j.stk@catch)
    fc.catch=fc.catch[fc.catch$year>=2023,]
    tot_b=B.ts[B.ts$scenario==scenarios[j]&B.ts$year%in% 2023:2030,]$tot_b
    fc.har=as.data.frame(j.stk@harvest)
    fc.har=fc.har[fc.har$year>=2023,]
  
    scen.res=data.frame(stock=x.stock$stock,
                        scenario=scenarios[j],
                        year=2023:2030,
                        rec=NA,
                        rec_var=NA,
                        tot_b=tot_b,
                        fbar_tot=round(as.numeric(fc.har$data), digits=3),
                        fbar_ita_otb=NA,
                        fbar_ita_tbb=NA,
                        catch_tot=round(as.numeric(fc.catch$data)),
                        catch_ita_otb=round(as.numeric(fc.catch$data))*catch.prop,
                        catch_ita_tbb=NA)
    
    if(j>1){
      if(scen.res$catch_tot[8]>fc.catch.tot[8]){
      ita.increase=scen.res$catch_tot-fc.catch.tot
      scen.res$catch_ita_otb=fc.catch.ita+ita.increase
      }else{
      ita.reduction=fc.catch.tot-scen.res$catch_tot
      scen.res$catch_ita_otb=fc.catch.ita-ita.reduction 
      }
    }
    scen.res$catch_ita_otb=round(scen.res$catch_ita_otb)
    stf_results=rbind(stf_results, scen.res)
  }
  stf_results$fbar_ita_otb=round(stf_results$fbar_tot-fbar_sq_oth, digits=3)
  #
  ref.yr.tab=data.frame(stock=x.stock$stock,
                        scenario='reference_year', year=2022, 
                        rec=NA,
                        rec_var=NA,
                        tot_b=B.ts[B.ts$scenario==scenarios[j]&B.ts$year%in% 2022,]$tot_b,
                        fbar_tot=round(f.s.quo, digits=3),
                        fbar_ita_otb=round(fbar_sq_ita,digits=3),
                        fbar_ita_tbb=NA,
                        catch_tot=as.numeric(catch(stk))[length(catch(stk))],
                        catch_ita_otb=as.numeric(catch(stk))[length(catch(stk))]*catch.prop,
                        catch_ita_tbb=NA)
  stf_results=rbind(ref.yr.tab, stf_results)
  #stf_results$catch_ita_tbb=NA
  #
  stk2=stk
  stf.store=fstks
  
