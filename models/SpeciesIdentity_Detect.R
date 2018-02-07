sink("models/SpeciesIdentity_Detect.jags")
cat("
    model {
    
    #True State for each bird, plant, camera
    for (i in 1:Birds){
      for(j in 1:Plants){
        for(k in 1:Cameras){
          logit(s[i,j,k])<-alpha[i,j]
          phi[i,j,k]~ dbern(s[i,j,k])
        }
      }
    }

    #Observation Model
    for (x in 1:Nobs){
      
      #Detection Process
      z[x] ~ dbern(detect[Bird[x]]) 
      
      #Observation
      p[x]<-z[x] * phi[Bird[x],Plant[x],Camera[x]]
      Yobs[x] ~ dbern(p[x])
      
      #Observed discrepancy
      #E[x]<-abs(Yobs[x]- s[Bird[x],Plant[x],Camera[x]])
    }
    
    #Assess Model Fit - Predict remaining data

    for(x in 1:Nnewdata){
    
      #Generate prediction
      znew[x] ~ dbern(detect[NewBird[x]])
      pnew[x] <- znew[x]*phi[NewBird[x],NewPlant[x],NewCamera[x]]
  
      #Predicted observation
      Ynew_pred[x]~dbern(pnew[x])
      
      #Assess fit, proportion of corrected predicted links
      Enew[x]<-abs(Ynew[x]-Ynew_pred[x])
    
    }
    
    #Priors
    #Observation model
    #Detect priors, logit transformed - Following lunn 2012 p85
    for(x in 1:Birds){
      logit(detect[x])<-dcam[x]
      dcam[x]~dnorm(omega_mu,omega_tau)
    }
    
    #Process Model
    #Species level priors
    for (i in 1:Birds){
      for (j in 1:Plants){
        #Intercept
        #logit prior, then transform for plotting
        alpha[i,j] ~ dnorm(0,0.386)
      } 
    }
    
    #OBSERVATION PRIOR
    omega_mu ~ dnorm(0,0.386)
    omega_tau ~ dunif(0,10)
    
    #derived posterior check
    #fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(Enew[])
    
    }
    ",fill=TRUE)

sink()
