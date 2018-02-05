sink("models/TraitMatch.jags")
cat("
    model {
    
    for (x in 1:Nobs){
  
    logit(s[x])<-alpha[Bird[x]] + beta1[Bird[x]] * Traitmatch[Bird[x],Plant[x]] 
    Yobs[x] ~ dbern(s[x])
    
    #Observed discrepancy
    E[x]<-abs(Yobs[x]- s[x])
    }
    
    #Assess Model Fit - Predict remaining data
    for(x in 1:Nnewdata){
    
    #Generate prediction
    logit(snew[x])<-alpha[NewBird[x]] + beta1[NewBird[x]] * Traitmatch[NewBird[x],NewPlant[x]] 
    Ynew_pred[x]~dbern(snew[x])
    
    #Assess fit, proportion of corrected predicted links
    Enew[x]<-abs(Ynew[x]-Ynew_pred[x])
    
    }
    
    #Priors

    #Process Model
    #Species level priors
    for (i in 1:Birds){
    
    #Intercept
    #logit prior, then transform for plotting
    alpha[i] ~ dnorm(alpha_mu,alpha_tau)
    
    #Traits slope 
    beta1[i] ~ dnorm(beta1_mu,beta1_tau)    
    
    }
    
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
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(Enew[])
    
    }
    ",fill=TRUE)

sink()