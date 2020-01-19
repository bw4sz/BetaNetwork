sink("models/test.jags")
cat("
    model {
    
    #Ecological Process Model - True interaction state
    for (i in 1:Birds){
    for(j in 1:Plants){
  
      #Interaction
      logit(s[i,j])<-alpha[i,j]

      for(k in 1:Cameras){
    
        #Occurrence 
        logit(psi[i,j,k])<-alpha_occ[i] + beta_occ[i] * elevation[k] 
        occ[i,j,k] ~ dbern(psi[i,j,k])
    
        #Conditional probability of interaction|occurrence
        rho[i,j,k] <- s[i,j] * occ[i,j,k]
        p[i,j,k] ~ dbern(rho[i,j,k])
    }

    }
    }

    
    #Observation Model
    for (x in 1:Nobs){
    
    #Detection Process
    z[x] <- detect[Bird[x]] * p[Bird[x],Plant[x],Camera[x]]

    #Observation, conditional on detection and occurrence.
    Yobs[x] ~ dbern(z[x])

    }
    
    #Priors
    
    #Occurrence model
    for(x in 1:Birds){
    alpha_occ[x] ~ dnorm(0,0.386)
    beta_occ[x] ~ dnorm(0,0.386)
    }
    
    #Observation model
    #Detect priors, logit transformed - Following lunn 2012 p85
    for(x in 1:Birds){
    logit(detect[x])<-dcam[x]
    dcam[x]~dnorm(omega_mu,omega_tau)
    }
    
    #Process Model
    for (i in 1:Birds){
    for (j in 1:Plants){
    #Intercept
    #logit prior, then transform for plotting
    alpha[i,j] ~ dnorm(0,0.386)
    }
    }
    
    #Observation group prior
    omega_mu ~ dnorm(0,0.386)
    omega_tau ~ dunif(0,10)
    
    }
    ",fill=TRUE)

sink()

#Run Model
runModel<-function(Yobs_dat){
  
  #Inits
  InitStage <- function(){
    
    #A blank Y matrix - all present
    initY<-rep(1,Dat$Nobs)
    initB<-rep(0.5,Dat$Birds)
    occ<-array(dim=c(Dat$Birds,Dat$Plants,Dat$Cameras),data=1)
    p<-array(dim=c(Dat$Birds,Dat$Plants,Dat$Cameras),data=1)
    
    list(dcam=initB,occ=occ,p=p)}
  
  #Parameters to track
  ParsStage <- c("s")
  
  #Jags Data
  Yobs<-Yobs_dat$Yobs

  Dat<-list(
    Yobs=Yobs,
    elevation=Yobs_dat$ele,
    Nobs=length(Yobs),
    Birds=max(Yobs_dat$jBird),
    Bird=Yobs_dat$jBird,
    Cameras=max(Yobs_dat$jID),
    Camera=Yobs_dat$jID,
    Plant=Yobs_dat$jPlant,
    Plants=max(Yobs_dat$jPlant))
  
  #MCMC options
  system.time(
    m2<-jags(data=Dat,parameters.to.save=ParsStage,inits=InitStage,model.file="models/test.jags",n.thin=1,n.iter=10,n.burnin=5,n.chains=2,DIC=F)
  )
  return(m2)
}


runModel(Yobs_dat=Yobs_dat)
