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

runModel<-function(indat,interval,traitmatchT){
  
  #Easiest to work with jags as numeric ordinal values
  indat$Hummingbird<-as.factor(indat$Hummingbird)
  indat$Iplant_Double<-as.factor(indat$Iplant_Double)
  indat$jBird<-as.numeric(indat$Hummingbird)
  indat$jPlant<-as.numeric(indat$Iplant_Double)
  
  jagsIndexBird<-data.frame(Hummingbird=levels(indat$Hummingbird),jBird=1:length(levels(indat$Hummingbird)))
  
  jagsIndexPlants<-data.frame(Iplant_Double=levels(indat$Iplant_Double),jPlant=1:length(levels(indat$Iplant_Double)))
  
  #Similiarly, the trait matrix needs to reflect this indexing.
  jTraitmatch<-traitmatchT[rownames(traitmatchT) %in% unique(indat$Hummingbird),colnames(traitmatchT) %in% unique(indat$Iplant_Double)]
  
  indat$Index<-1:nrow(indat)
  indat<-droplevels(indat)
  
  #Turn Time and ID into numeric indexes
  indat$jTime<-as.numeric(as.factor(indat$Time))
  indat$jID<-as.numeric(as.factor(indat$ID))
  
  #Inits
  InitStage <- function(){
    #A blank Y matrix - all present
    initY<-array(dim=c(Dat$Birds,Dat$Plants),1)
    initB<-rep(0.5,Dat$Birds)
    initp<-rep(1,Dat$Nobs)
    initpnew<-rep(1,Dat$Nnewdata)
    alpha=rep(0,Dat$Birds)
    beta1=rep(0,Dat$Birds)
    Ynew_pred<-rep(1,Dat$Nnewdata)
    list(dcam=initB,znew=initpnew,z=initp,Ynew_pred=Ynew_pred)}
  
  #Parameters to track
  ParsStage <- c("alpha","beta1","alpha_mu","alpha_sigma","beta1_sigma","beta1_mu","detect","E","E.new","fit","fitnew")
  
  #Jags Data
  Yobs_dat<-indat[indat$jinterval <= interval,]
  Ynew_dat<-indat[indat$jinterval > interval,]
  Yobs<-(Yobs_dat$Yobs > 0) *1
  Ynew<-(Ynew_dat$Yobs > 0) *1
  
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
      m2<-jags(data=Dat,parameters.to.save=ParsStage,inits=InitStage,model.file="models/TraitMatch.jags",n.thin=1,n.iter=10,n.burnin=0,n.chains=1,DIC=F)
    )
    return(m2)
}

getChains<-function(mod){
  pars_detect<-getPar(mod,data=indat,Bird="jBird",Plant="jPlant")
}

getPar<-function(x,data=indat,Bird="Bird",Plant="Plant"){
  #extract desired info from the models
  parsO<-melt(x$BUGSoutput$sims.array)
  colnames(parsO)<-c("Draw","Chain","parameter","estimate")
  
  #label species and plants
  l<-levels(parsO$parameter)
  
  #parameters to save
  totrack<-x$parameters.to.save
  
  #assign species index to ragged frame.
  sp_pl<-data.frame(parameter=l,species=as.numeric(str_match(l,pattern="\\[(\\d+)]")[,2]),par=str_extract(l,"\\w+"))
  
  #correct N samples
  i<-sp_pl$par %in% "E"
  
  #Species
  sp_pl[i,][,"species"]<-data[as.numeric(str_match(sp_pl[i,][,"parameter"],pattern="\\[(\\d+)]")[,2]),Bird]
  
  #Plant
  #add a NA plant columns
  sp_pl$plant<-NA
  sp_pl[i,][,"plant"]<-data[as.numeric(str_match(sp_pl[i,][,"parameter"],pattern="\\[(\\d+)]")[,2]),Plant]
  
  #merge levels
  pars<-merge(parsO,sp_pl)
  
  #take out deviance
  pars<-pars[!pars$par %in% "deviance",]
  return(pars)
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
  v=inv.logit(alpha + beta1 * trait)
  
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