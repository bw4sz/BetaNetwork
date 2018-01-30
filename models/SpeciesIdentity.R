sink("models/SpeciesIdentity.jags")
cat("
    model {
    
    for (x in 1:Nobs){

    #observation
    logit(s[x])<-alpha[Bird[x],Plant[x]]
    Yobs[x] ~ dbern(s[x])
    
    #Observed discrepancy
    E[x]<-abs(Yobs[x]- s[x])/Nobs
    }
    
    #Assess Model Fit - Predict remaining data
    for(x in 1:Nnewdata){
    
      #Generate prediction
      logit(snew[x])<-alpha[NewBird[x],NewPlant[x]]
      Ynew_pred[x]~dbern(snew[x])
    
      #Assess fit, proportion of corrected predicted links
      E.new[x]<-abs(Ynew[x]-Ynew_pred[x])/Nnewdata

    }
    
    #Priors

    #Species level priors
    for (i in 1:Birds){
      for (j in 1:Plants){

        #Intercept
        #logit prior, then transform for plotting
        alpha[i,j] ~ dnorm(0,0.386)
      } 
    }

    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    }
    ",fill=TRUE)

sink()
