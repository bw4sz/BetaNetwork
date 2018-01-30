sink("models/TraitMatch.jags")
cat("
    model {
    
    #Observation Model
    for (x in 1:Nobs){
    
    #Observation Process
    #True state
    z[x] ~ dbern(detect[Bird[x]]) 
    
    #observation
    logit(s[x])<-alpha[Bird[x],Plant[x]]
    p[x]<-z[x] * s[x]
    Yobs[x] ~ dbern(p[x])
    
    #Observed discrepancy
    E[x]<-abs(Yobs[x]- s[x])/Nobs
    }
    
    #Assess Model Fit - Predict remaining data
    for(x in 1:Nnewdata){
    
      #Generate prediction
      znew[x] ~ dbern(detect[Bird[x]])
      logit(snew[x])<-alpha[Bird[x],Plant[x]]
      pnew[x]<-znew[x]*snew[x]
      Ynew_pred[x]~dbern(pnew[x])
    
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
        alpha_tau ~ dt(0,1,1)I(0,)
        alpha_sigma<-pow(1/alpha_tau,0.5) 
    
    }
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()
