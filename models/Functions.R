#extract and create a dataframe of posteriors

runModel<-function(indat,traitmatchT=traitmatchT){
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
  
  #Source model
  source("models/TraitMatch.R")
  
  #print model
  writeLines(readLines("models/TraitMatch.R"))
  
  #Inits
  InitStage <- function(){
    #A blank Y matrix - all present
    initY<-array(dim=c(Birds,Plants,Times),22)
    initB<-rep(0.5,Birds)
    list(S=initY,dcam=initB)}
  
  #Parameters to track
  ParsStage <- c("alpha","beta1","alpha_mu","alpha_sigma","beta1_sigma","beta1_mu","detect","E","E.new","fit","fitnew")
  
  #Jags Data
  Dat<-list(
    Yobs_camera = (indat$Yobs > 0) *1 ,
    Birds=max(indat$jBird),
    Bird=indat$jBird,
    Plant=indat$jPlant,
    Time=indat$jID,
    Plants=max(indat$jPlant),
    Times=max(indat$jID),
    Nobs=nrow(indat),
    Traitmatch=jTraitmatch)
  
  #MCMC options
  if(newModel){
    system.time(
      m2<-upda(data=Dat,parameters.to.save =ParsStage,inits=InitStage,model.file="models/TraitMatch.jags",n.thin=1,n.iter=10,n.burnin=0,n.chains=1,DIC=F)
    )
  }
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
  lapply(generated_networks,,function(x){
    genNetwork
  })
  
}


trajState<-function(alpha,beta,x,observed){
  
  #Bind together
  fdat<-data.frame(alpha=alpha,beta=beta)
  
  #fit regression for each input estimate
  sampletraj<-list()
  for (s in 1:nrow(fdat)){
    a<-fdat$alpha[s]
    b<-fdat$beta[s]
    yp=exp(a + b*x$value)
    
    #compute pred value
    state<-data.frame(x,State=rpois(length(yp),yp))
    
    #merge with observed state
    mstate<-merge(state,observed,by=c("Bird","Plant"))
    
    #Compute chisquared
    csq<-sum((mstate$Y-mstate$State)^2/(mstate$State+0.5))
    
    sampletraj[[s]]<-csq
  }
  
  #return as a vector
  return(unlist(sampletraj))
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
  
  sampletraj<-data.frame(trait=trait,resources=resources,y=as.numeric(v))
  
  #Compute CI intervals
  return(sampletraj)
}

#sample trajectory for a given posterior using quantile or hdi interval
trajLog<-function(alpha,beta1,x,type='quantile'){
  indat<-data.frame(alpha,beta1)
  
  #fit regression for each input estimate
  sampletraj<-list()
  
  for (y in 1:nrow(indat)){
    v=exp(indat$alpha[y] + indat$beta1[y] * x)
    
    sampletraj[[y]]<-data.frame(x=as.numeric(x),y=as.numeric(v))
  }
  
  sample_all<-rbind_all(sampletraj)
  
  #Compute CI intervals
  if(type=='quantile'){
    predy<-group_by(sample_all,x) %>% summarise(lower=quantile(y,0.025,na.rm=T),upper=quantile(y,0.975,na.rm=T),mean=mean(y,na.rm=T))
  }
  if(type=='hdi'){
    predy<-group_by(sample_all,x) %>% summarise(lower=hdi(y)[[1]],upper=hdi(y)[[2]],mean=mean(y,na.rm=T))
  }
  return(predy)
}

#predicted y for logistic
trajLogistic<-function(alpha,beta1,beta2,beta3,x,resources){
  indat<-data.frame(alpha,beta1,beta2,beta3)
  
  #fit regression for each input estimate
  sampletraj<-list()
  
  for (y in 1:nrow(indat)){
    v=exp(indat$alpha[y] + indat$beta1[y] * x + indat$beta2[y] * resources + indat$beta3[y] * x*resources)
    
    sampletraj[[y]]<-data.frame(x=as.numeric(x),y=as.numeric(v))
  }
  
  sample_all<-rbind_all(sampletraj)
  
  #Compute CI intervals
  predy<-group_by(sample_all,x) %>% summarise(lower=quantile(y,0.025,na.rm=T),upper=quantile(y,0.975,na.rm=T),mean=mean(y,na.rm=T))
}

#calculate poisson interactions

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
    aggm[x[j,"jBird"],x[j,"jPlant"]]<-rpois(1,lambda=x[j,"phi"])
  }
}



ggplot(nstat,aes(x=value,fill=L1)) + geom_density(alpha=0.7) + facet_wrap(~Metric,scales='free',nrow=3) + scale_fill_discrete("Resource Availability") + theme_bw() + scale_fill_manual("Resource Availability",values=c("Grey80","Grey50","Black"))
ggsave("Figures/NetworkStatistics.jpeg",height=4,width=6,dpi=600) 
