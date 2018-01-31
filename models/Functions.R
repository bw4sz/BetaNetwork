#extract and create a dataframe of posteriors

prepData<-function(indatraw){
  #Only non-detections are real 0's, the rest are NA's and are removed.
  #Plants not surveyed in that time period
  #Hummingbirds not present at that elevation
  
  #For each ID
  Time<-unique(indatraw$Time)
  
  #absences data frame
  absences<-list()
  
  for(t in Time){
    IDlist<-unlist(unique(indatraw[indatraw$Time ==t,"ID"]))
    
    for (j in IDlist){
      #Which plants were sampled
      a<-indatraw %>% filter(Time==t,ID==j)
      
      #For each sampled transect
      trans<-unique(a$Transect_R)
      
      if(!length(trans)==0){
        for(transect in trans){
          
          #for each date 
          datec<-a %>% filter(Transect_R %in% transect)
          datecam<-unique(datec$DateP)
        }} else{
          datecam<-a %>% distinct(DateP) %>% .$DateP
        }
      for(Date in datecam){
        
        #for each plant along that transect at that date
        pres<-a %>% filter(DateP %in% Date) %>% distinct(Iplant_Double) %>% .$Iplant_Double
        
        #Which day in sampling
        dday<-a %>% filter(Transect_R %in% transect,DateP %in% Date) %>% distinct(Day) %>% .$Day
        
        for (plant in pres){
          #Get mean elevation of that plant record
          camelev<- a %>% filter(Transect_R %in% transect,DateP %in% Date,Iplant_Double %in% plant) %>% .$ele %>% mean()
          
          #Which birds are present at that observation
          predh<-elevH[((elevH$Low < camelev) & (camelev < elevH$High)),"Hummingbird"]
          
          #remove the ones seen on that plant
          hum_present<-a %>% filter(Transect_R %in% transect,DateP %in% Date,Iplant_Double %in% plant) %>% .$Hummingbird
          abbh<-predh[!predh %in% hum_present]
          if(length(abbh)==0){next}
          
          #Make absences from those )(cat not the best)
          add_absences<-data.frame(Hummingbird=abbh,Iplant_Double=plant,Time=t,ID=j,DateP=Date,Month=min(a$Month),Year=unique(a$Year),Transect_R=transect,ele=camelev,Day=unique(dday),Survey_Type=unique(a$Survey_Type),Yobs=0)
          absences<-append(absences,list(add_absences))
        }
      }
    }
  }
  
  indatab<-rbind_all(absences)
  
  #merge with original data
  indat<-rbind_all(list(indatraw,indatab))
  
  #match to traits
  indat<-merge(indat,traitmelt,by=c("Hummingbird","Iplant_Double"))
  return(indat)
}

#run a jags model

getChains<-function(mod){
  pars_detect<-getPar(mod,Bird="jBird",Plant="jPlant")
}

getPredictions<-function(pars,Ynew_dat){
  Ynew_pred<-pars %>% filter(parameter=="Ynew_pred") %>% mutate(Index=as.numeric(as.character(Index)))
  Ynew_pred<-Ynew_dat %>% dplyr::select(-jinterval,-interval) %>% mutate(Index=1:nrow(Ynew_dat)) %>% inner_join(Ynew_pred,by="Index")
  predictions<-split(Ynew_pred,Ynew_pred$Draw,Ynew_pred$chain)
  predictions<-lapply(predictions,function(x){
    m<-acast(x,Hummingbird~Iplant_Double,value.var="value",fun.aggregate = sum)
    m<-(m > 0) * 1
    return(m)
  })
  return(predictions)
}

getPar<-function(x,Bird="Bird",Plant="Plant"){
  #extract desired info from the models
  #bind chains
  pc_dive<-reshape2::melt(x$BUGSoutput$sims.array)
  colnames(pc_dive)<-c("Draw","chain","par","value")
  
  #extract parameter name
  pc_dive$parameter<-data.frame(str_match(pc_dive$par,"(\\w+)"))[,-1]
  
  #Extract index
  splitpc<-split(pc_dive,pc_dive$parameter)
  
  #single index
  splitpc[c("alpha","beta1","detect","Ynew_pred")]<-lapply(splitpc[c("alpha","beta1","detect","Ynew_pred")],function(x){
    sv<-data.frame(str_match(x$par,"(\\w+)\\[(\\d+)]"))[,3]
    pc<-data.frame(x,Index=sv)
    return(pc)
  }) 
  
  #bind all matrices back together
  pc_dive<-bind_rows(splitpc)
}
