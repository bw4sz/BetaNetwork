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

runModel<-function(Yobs_dat,Ynew_dat,interval,jTraitmatch){
  
  #Inits
  InitStage <- function(){
    #A blank Y matrix - all present
    initY<-array(dim=c(Dat$Birds,Dat$Plants),max(Dat$Yobs)+1)
    initB<-rep(0.5,Dat$Birds)
    Ynew_pred<-rep(max(Dat$Ynew),Dat$Nnewdata)
    N<-rep(max(Dat$Yobs)+1,Dat$Nobs)
    list(dcam=initB,N=N,Nnew=Ynew_pred,Ynew_pred=Ynew_pred)}
  
  #Parameters to track
  ParsStage <- c("alpha","beta1","alpha_mu","alpha_sigma","beta1_sigma","beta1_mu","detect","Ynew_pred","fit","fitnew")
  
  #Jags Data
  Yobs<-Yobs_dat$Yobs
  Ynew<-Ynew_dat$Yobs
  
  Dat<-list(
    Yobs=Yobs,
    Birds=max(indat$jBird),
    Bird=Yobs_dat$jBird,
    Plant=Yobs_dat$jPlant,
    Nobs=length(Yobs),
    NewBird=Ynew_dat$jBird,
    NewPlant=Ynew_dat$jPlant,
    Ynew=Ynew,
    Nnewdata=length(Ynew),
    Traitmatch=jTraitmatch)
  
  #MCMC options
    system.time(
      m2<-jags(data=Dat,parameters.to.save=ParsStage,inits=InitStage,model.file="models/TraitMatchPoisson.jags",n.thin=1,n.iter=40000,n.burnin=39000,n.chains=1,DIC=F)
    )
    return(m2)
}

getChains<-function(mod){
  pars_detect<-getPar(mod,Bird="jBird",Plant="jPlant")
}

getPredictions<-function(mod,Ynew_dat){
  pars_detect<-getPar(mod,Bird="jBird",Plant="jPlant")
  Ynew_pred<-pars_detect %>% filter(parameter=="Ynew_pred")
  Ynew_dat$Index<-1:nrow(Ynew_dat)
  Ynew_pred<-merge(Ynew_pred,Ynew_dat,by="Index")
  return(Ynew_pred)
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

genNetwork<-function(x){
  #input matrix
  aggm<-matrix(nrow=nrow(jagsIndexBird),ncol=nrow(jagsIndexPlants),data=0)
  for (j in 1:nrow(x)){
    aggm[x[j,"jBird"],x[j,"jPlant"]]<-rpois(1,lambda=x[j,"phi"])
  }
}

#fits a chisquared residual for a given poisson function
modelFit<-function(mod){
  #Geneate Network
  genNetwork(mod)
  
  #Compare Network to remaining data
  #For each link
  lapply(generated_networks,function(x){
    genNetwork
  })
  
}

#sample trajectory for a given posterior
trajF<-function(alpha,beta1,trait){
  g<-data.frame(alpha,beta1)
  
  #label rows
  g$id<-1:nrow(g)
  
  sampletraj<-g %>% group_by(id) %>% do(traj(.$alpha,.$beta1,trait=trait)) %>% group_by(trait) %>% summarize(mean=mean(y),lower=quantile(y,0.05),upper=quantile(y,0.95))
  return(sampletraj)
}

#sample trajectory for a given posterior using quantile or hdi interval
traj<-function(alpha,beta1,trait){
  
  #fit regression for each input estimate
  v=exp(alpha + beta1 * trait)
  
  sampletraj<-data.frame(trait=trait,y=as.numeric(v))
  
  #Compute CI intervals
  return(sampletraj)
}


#plots
#converge of chains
chainplot<-function(pars,param,title){
  ggplot(pars[pars$par %in% param,],aes(x=Draw,y=estimate,col=as.factor(Chain))) + geom_line() + facet_wrap(~species,scale="free") + theme_bw() + labs(col="Chain") + ggtitle(title)  
}

#posteriors
tracegplot<-function(pars,param,title){
  ggplot(pars[pars$par %in% param,],aes(x=estimate)) + geom_histogram() + ggtitle("Estimate of Intercept") + theme_bw() + ggtitle(title)
}

#Generate network
networkStat<-function(statname){
  lapply(nstat,function(x){
    nstat<-networklevel(aggm,index=statname,level="lower")
  })
  netstat<-melt(t(sapply(1:500,function(k) genNetwork(x)))) 
  colnames(netstat)<-c("Iteration","Metric","value")
  return(netstat)
}

genNetwork<-function(x){
  #input matrix
  aggm<-matrix(nrow=nrow(jagsIndexBird),ncol=nrow(jagsIndexPlants),data=0)
  for (j in 1:nrow(x)){
    aggm[x[j,"jBird"],x[j,"jPlant"]]<-rbinom(1,lambda=x[j,"phi"])
  }
}