sink("models/SpeciesIdentity_Detect.jags")
cat("
    model {
    
    #True interaction probability for each bird, plant
    for (i in 1:Birds){
      for(j in 1:Plants){
      for(k in 1:Cameras){
          logit(s[i,j,k])<-alpha[i,j]
          z[i,j,k] ~ dbern(s[i,j,k])
          }
        }
      }

    #Observation Model
    for (x in 1:Nobs){
      
      #Detection Process
      p[x]<-detect[Bird[x]] * z[Bird[x],Plant[x],Camera[x]]

      #Observation
      Yobs[x] ~ dbern(p[x])
    }
    
    #Assess Model Fit - Predict remaining data

    for(x in 1:Nnewdata){
    
      #Generate prediction
      pnew[x] <- detect[NewBird[x]] * z[NewBird[x],NewPlant[x],NewCamera[x]]

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
        #Logit Intercept
        alpha[i,j] ~ dnorm(0,0.386)
      } 
    }
    
    #OBSERVATION PRIOR
    omega_mu ~ dnorm(0,0.386)
    omega_tau ~ dunif(0,10)
    
    #derived posterior predictive error
    fitnew<-sum(Enew[])
    
    }
    ",fill=TRUE)

sink()
