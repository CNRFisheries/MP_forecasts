


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

get.timeseries=function(forecast.data, stock,scenario.vec){
  target.years=2022:2030
  
  forecast.df=NULL
  
  for(ii in 1:length(scenario.vec)){
    x.forecast=forecast.data[[ii]]
    fleet.def=data.frame(id=x.forecast$fleet_ID,
                         fleet_name=x.forecast$FleetNames)
    fleet.sel=fleet.def[fleet.def$fleet_name %in% fleet.selection,]
    
    ts.df=x.forecast[["timeseries"]]
    ts.df=ts.df[ts.df$Yr%in%target.years,]%>%
      replace(is.na(.),0)
    ts.df=ts.df%>%
      dplyr::group_by(Yr,Era)%>%
      dplyr::summarise_all(.,sum)
    
    catch.df=ts.df[,grep( 'dead',names(ts.df))]
    catch.df=catch.df[,grep( 'B',names(catch.df))]
    c.ts.all=as.numeric(apply(catch.df,1,sum))
    
    if(length(grep('TBB', fleet.sel$fleet_name))>0){
    catch.df.ita=catch.df[,fleet.sel[fleet.sel$fleet_name=='OTB_ITA',]$id]
    c.ts.ita=as.numeric(apply(catch.df.ita,1,sum))  
    c.ts.ita.tbb=catch.df[,fleet.sel[fleet.sel$fleet_name=='TBB_ITA',]$id]
    c.ts.ita.tbb=round(as.numeric(apply(c.ts.ita.tbb,1,sum))  )
    }else{
    catch.df.ita=catch.df[,fleet.sel$id]
    c.ts.ita=as.numeric(apply(catch.df.ita,1,sum))  
    c.ts.ita.tbb=NA
    }
    

    
    ssb.ts=ts.df[,'SpawnBio']
    rec.ts=ts.df[,'Recruit_0']
    
    f.df=x.forecast$exploitation
    f.df=na.omit(f.df)
    f.df=f.df[f.df$Yr%in%target.years,]
    f.ts=f.df$annual_F
    f.apic.tot=as.numeric(apply(f.df[,7:ncol(f.df)],1,sum))
    ratio.apic=f.ts/f.apic.tot
    
    if(length(grep('TBB', fleet.sel$fleet_name))>0){
      f.ita=f.df[,'OTB_ITA']  
      f.ita=f.ita
      f.ita.tbb=f.df[,'TBB_ITA']  
      f.ita.tbb=round(f.ita.tbb, digits=3)
      i.res=data.frame(stock=stock,
                       scenario=scenarios[ii],
                       year=2022:2030,
                       rec=round(rec.ts$Recruit_0),
                       rec_var=NA,
                       ssb=round(ssb.ts$SpawnBio),
                       fbar_tot=round(f.ts, digits=3),
                       fbar_ita_otb=round(f.ita, digits=3),
                       fbar_ita_tbb=f.ita.tbb,
                       ratio_apic=ratio.apic,
                       catch_tot=round(c.ts.all),
                       catch_ita_otb=round(c.ts.ita),
                       catch_ita_tbb=c.ts.ita.tbb)
      
      forecast.df=rbind(forecast.df, i.res)
    }else{
      f.ita=f.df[,fleet.sel$fleet_name]
      f.ita=as.numeric(apply(f.ita, 1, sum))
      f.ita.tbb=NA
      i.res=data.frame(stock=stock,
                       scenario=scenarios[ii],
                       year=2022:2030,
                       rec=round(rec.ts$Recruit_0),
                       rec_var=NA,
                       ssb=round(ssb.ts$SpawnBio),
                       fbar_tot=round(f.ts, digits=3),
                       fapic_ita_otb=round(f.ita, digits=3),
                       fapic_ita_tbb=f.ita.tbb,
                       ratio_apic=ratio.apic,
                       catch_tot=round(c.ts.all),
                       catch_ita_otb=round(c.ts.ita),
                       catch_ita_tbb=c.ts.ita.tbb)
      
      forecast.df=rbind(forecast.df, i.res)
    }

    

  }
  return(forecast.df)
}


stk.unitSums=function(stk){
  i.stk <- stk
  i.stk@catch.n <- unitSums(stk@catch.n)
  i.stk@stock <- unitSums(stk@stock)
  i.stk@catch.wt <- unitSums(stk@catch.wt)
  i.stk@discards.n <- unitSums(stk@discards.n)
  i.stk@discards.wt <- unitSums(stk@discards.wt)
  i.stk@landings <- unitSums(stk@landings)
  i.stk@discards <- unitSums(stk@discards)
  i.stk@landings.n <- unitSums(stk@landings.n)
  i.stk@landings.wt <- unitSums(stk@landings.wt)
  i.stk@stock.n <- unitSums(stk@stock.n)
  i.stk@stock.wt <- unitSums(stk@stock.wt)
  i.stk@m <- unitSums(stk@m)
  i.stk@mat <- unitSums(stk@mat)
  i.stk@harvest <- unitMeans(stk@harvest)
  i.stk@harvest.spwn <- unitSums(stk@harvest.spwn)
  i.stk@m.spwn <- unitSums(stk@m.spwn)
  i.stk@catch<- unitSums(stk@catch)
  return(i.stk)
}

base.plot=function(i.brp, stk){
  
  if(length(stk)>1){
    stf.tidy=list()
    for (ii.stk in 1:length(stf.store)) {
      stk.i=stf.store[[ii.stk]]
      plot.df=data.frame(Year=range(stk.i)[4]:range(stk.i)[5],
                         Rec=as.numeric(rec(stk.i)),
                         Catch=as.numeric(catch(stk.i)),
                         SSB=as.numeric(ssb(stk.i)),
                         fbar=as.numeric(fbar(stk.i)))
      plot.df=plot.df%>%
        tidyr::pivot_longer(-Year,names_to = 'var')
      plot.df$scenario=names(stf.store[ii.stk])
      stf.tidy[[ii.stk]]=plot.df
    }
    stf.tidy=plyr::ldply(stf.tidy)  
    f.sq=unique(stf.tidy[stf.tidy$Year==2023&stf.tidy$var=='fbar',]$value)
    ann_bpa <- data.frame(Year = 2010,wt = 5,lab = "Bpa",
                          var = 'SSB', value=50)
    ann_f <- data.frame(Year = 2010,wt = 5,lab = "Bpa",
                        var = 'fbar', value=0.2)
    p=ggplot(data=stf.tidy, aes(x=Year, y=value))+
      geom_line(data=stf.tidy[stf.tidy$scenario=='status quo',],aes(color=scenario))+
      geom_line(data=stf.tidy[stf.tidy$scenario!='status quo'&stf.tidy$Year>=2025,],aes(color=scenario))+
      annotate("rect", xmin = 2023, xmax = 2025, ymin = 0, ymax = Inf,
               alpha = .1,fill = "grey")
    
  }else{
    stf.tidy=data.frame(Year=range(stk)[4]:range(stk)[5],
                        Rec=as.numeric(rec(stk)),
                        Catch=as.numeric(catch(stk)),
                        SSB=as.numeric(ssb(stk)),
                        fbar=as.numeric(fbar(stk)))%>%
      tidyr::pivot_longer(-Year,
                          names_to = 'var')
    stf.tidy
    p=ggplot(data=stf.tidy, aes(x=Year, y=value))+
      geom_line()
    
  }
  
  p=p+
    facet_grid(rows=vars(var), scales = 'free')+
    ylim(c(0,NA))+
    geom_hline(data = data.frame(var='fbar',value=9),
               aes(yintercept = i.brp$f01), linetype = 2)+
    geom_hline(data = data.frame(var='SSB',value=9),
               aes(yintercept = i.brp$bpa), linetype = 2)+
    geom_text(data = ann_bpa,label = expression('B'[pa]))+
    geom_text(data = ann_f,label = expression("F"['01']))+
    ylab('');p
  
  
  return(p)
}