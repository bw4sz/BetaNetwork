sink("models/test.jags")

cat("
    model {

    #Observation Model
    for (x in 1:Nobs){
    
    #Observation Process
    #True state
    z[x] ~ dbern(detect[Bird[x]]) 
    
    #observation
    logit(s[x])<-alpha[Bird[x]] + beta1[Bird[x]] * Traitmatch[Bird[x],Plant[x]] 
    p[x]<-z[x] * s[x]
    Yobs[x] ~ dbern(p[x])
    
    #Observed discrepancy
    #E[x]<-abs(Yobs[x]-S[Bird[x],Plant[x]])
    }
    
    #Assess Model Fit - Predict remaining data
    for(x in 1:Nnewdata){
      
      #Generate prediction
      znew[x] ~ dbern(detect[NewBird[x]])
      pnew[x]<-znew[x]*alpha[NewBird[x]] + beta1[NewBird[x]] * Traitmatch[NewBird[x],NewPlant[x]] 
      Ynew_pred[x]~dbern(pnew[x])
      
      #Assess fit
      E.new[x]<-abs(Ynew[x]-Ynew_pred[x])
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
    
    #Intercept
    #logit prior, then transform for plotting
    alpha[i] ~ dnorm(alpha_mu,alpha_tau)
    
    #Traits slope 
    beta1[i] ~ dnorm(beta1_mu,beta1_tau)    
    
    }
    
    #OBSERVATION PRIOR
    omega_mu ~ dnorm(0,0.386)
    omega_tau ~ dunif(0,10)
    
    #Group process priors
    
    #Intercept 
    alpha_mu ~ dnorm(0,0.386)
    alpha_tau ~ dt(0,1,1)I(0,)
    alpha_sigma<-pow(1/alpha_tau,0.5) 
    
    #Trait
    beta1_mu~dnorm(0,0.386)
    beta1_tau ~ dt(0,1,1)I(0,)
    beta1_sigma<-pow(1/beta1_tau,0.5)
    
    #derived posterior check
    #fit<-sum(E[]) #Discrepancy for the observed data
    #fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()