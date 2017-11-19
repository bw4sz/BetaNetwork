
sink("models/TraitMatch.jags")

cat("
    model {
    #Compute true state for each pair of birds and plants
    for (i in 1:Birds){
    for (j in 1:Plants){
    for (k in 1:Times){
    
    #Process Model
    logit(rho[i,j,k])<-alpha[i] + beta1[i] * Traitmatch[i,j] 
    
    #True State
    S[i,j,k] ~ dbern(rho[i,j,k])
    }
    }
    }
    
    #Observation Model
    for (x in 1:Nobs){
    
    #Observation Process for cameras
    Yobs_camera[x] ~ dbin(detect[Bird[x]],S[Bird[x],Plant[x],Time[x]])    
    
    #     #Assess Model Fit - Posterior predictive check
    # 
    #     #Fit discrepancy statistics
    #Camera
    eval_cam[x]<-detect[Bird[x]]*S[Bird[x],Plant[x],Time[x]]
    E[x]<-pow((Yobs_camera[x]-eval_cam[x]),2)/(eval_cam[x]+0.5)
    #     
    ynew_cam[x]~dbin(detect[Bird[x]], S[Bird[x],Plant[x],Time[x]])
    E.new[x]<-pow((ynew_cam[x]-eval_cam[x]),2)/(eval_cam[x]+0.5)
    }
    
    #Priors
    #Observation model
    #Detect priors, logit transformed - Following lunn 2012 p85
    
    for(x in 1:Birds){
    #For Cameras
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
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()
