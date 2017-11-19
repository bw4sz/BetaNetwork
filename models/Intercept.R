
sink("Bayesian/Intercept.jags")

cat("
    model {
    #Compute intensity for each pair of birds and plants
    for (x in 1:Nobs){
    
    #Process Model
    log(lambda[Bird[x],Plant[x],Time[x]])<-alpha[Bird[x],Plant[x]]
    
    #For each camera - there is a latent count
    Yobs[x] ~ dpois(lambda[Bird[x],Plant[x],Time[x]])

    #Assess Model Fit
    
    #Fit discrepancy statistics
    eval[x]<-lambda[Bird[x],Plant[x],Time[x]]
    E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
    
    ynew[x]~dbin(detect[Bird[x]],N[Bird[x],Plant[x],Camera[x]])
    E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
    }
    }
    }
    
    #Species level priors
    
    for (b in 1:Birds){
      for(p in 1:Plants){

      alpha[b,p] ~ dnorm(intercept,tau_alpha)
      }

    }
    
    #Hyperpriors
    #Slope grouping
    gamma1~dnorm(0,0.0001)
    gamma2~dnorm(0,0.0001)
    gamma3~dnorm(0,0.0001)
    
    
    #Intercept grouping
    intercept~dnorm(0,0.0001)
    
    # Group intercept variance
    tau_alpha ~ dgamma(0.0001,0.0001)
    sigma_int<-pow(1/tau_alpha,0.5) 
    
    #Derived Quantity
    
    #Slope variance, turning precision to sd
    
    #Group Effect of traits
    tau_beta1 ~ dgamma(0.0001,0.0001)
    sigma_slope1<-pow(1/tau_beta1,0.5)
    
    #Group Effect of Resources
    tau_beta2 ~ dgamma(0.0001,0.0001)
    sigma_slope2<-pow(1/tau_beta2,0.5)
    
    #Group Effect of Resources * Traits
    tau_beta3 ~ dgamma(0.0001,0.0001)
    sigma_slope3<-pow(1/tau_beta3,0.5)
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()