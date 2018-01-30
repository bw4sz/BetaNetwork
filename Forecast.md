# Beta





```
## [1] "Run Completed at 2017-11-24 11:05:58"
```


```r
#reload if needed
#load("ObservedModel.RData")
```

#Load in data


```r
#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv("data/FlowerMorphology.csv")

#use effective corolla where possible.
fl.morph$Corolla<-fl.morph$EffectiveCorolla

fl.morph[is.na(fl.morph$Corolla),"Corolla"]<-fl.morph[is.na(fl.morph$Corolla),"TotalCorolla"]

#fuchsia macrostigma has an undue influence on this analysis, being 3x longer than other flowers, its not clear that birds really have to reach down the full corolla lenghth, use effective corolla length.
#fl.morph[fl.morph$Group.1 %in% "Fuchsia macrostigma","Corolla"]<-50

#First row is empty
fl.morph<-fl.morph[-1,]

#Bring in Hummingbird Morphology Dataset, comes from
hum.morph<-read.csv("data/HummingbirdMorphology.csv")

#taxonomy change, we are calling them Crowned Woodnymph's now.
hum.morph$English<-as.character(hum.morph$English)

hum.morph$English[hum.morph$English %in% "Green-crowned Woodnymph"]<-"Crowned Woodnymph"

#Bring in Interaction Matrix
int<-read.csv("data/HummingbirdInteractions.csv")

int$timestamp<-as.POSIXct(paste(int$Time,int$DateP),format="%H:%M:%S %Y-%m-%d")

#one date error
int[int$DateP %in% '2013-07-25',"Month"]<-7

#one duplicate camera error, perhaps two GPS records.
int<-int[!(int$ID %in% "FH1108" & int$Date_F %in% '2014-12-01'),]

#Correct known taxonomic disagreements, atleast compared to traits
int[int$Iplant_Double %in% "Onagaraceae fuschia","Iplant_Double"]<-"Fuchsia macrostigma"
int[int$Iplant_Double=="Alloplectus purpureus","Iplant_Double"]<-"Glossoloma purpureum"
int[int$Iplant_Double=="Capanea affinis","Iplant_Double"]<-"Kohleria affinis"
int[int$Iplant_Double=="Columnea cinerea","Iplant_Double"]<-"Columnea mastersonii"
int[int$Iplant_Double=="Alloplectus teuscheri","Iplant_Double"]<-"Drymonia teuscheri"
int[int$Iplant_Double=="Drymonia collegarum","Iplant_Double"]<-"Alloplectus tetragonoides"

#Some reasonable level of presences, 4 points
keep<-names(which(table(int$Hummingbird) > 4))

#int<-int[int$Hummingbird %in% keep & !int$Hummingbird %in% c("Sparkling Violetear"),]

m.dat<-droplevels(int[colnames(int) %in% c("ID","Video","Time","Hummingbird","Sex","timestamp","TransectID","Transect_R","Iplant_Double","Pierce","DateP","Month","ele","Type")])

#Does the data come from camera or transect?
m.dat$Type<-(is.na(m.dat$TransectID))*1

m.dat$Year<-years(as.Date(m.dat$DateP))

#one missing date
m.dat$Year[m.dat$Year %in% 2012]<-2013
m.dat$Year[m.dat$Year %in% 2106]<-2016

#Number of bird species
h_species<-nlevels(m.dat$Hummingbird)

#Number of plant species
plant_species<-nlevels(m.dat$Iplant_Double)

#Get english name
dath<-merge(m.dat,hum.morph, by.x="Hummingbird",by.y="English",keep=all)

#Merge to flowers
int.FLlevels<-levels(factor(dath$Iplant_Double))

#Which flowers are we missing info for?
missingTraits<-int.FLlevels[!int.FLlevels %in% fl.morph$X]

#print(paste("Missing Trait Information:",missingTraits))
dath<-merge(dath,fl.morph, by.x="Iplant_Double",by.y="X")

#Drop piercing events, since they don't represent correlation
#dath<-dath[!dath$Pierce %in% c("y","Y"),]
```

##Match Species to Morphology


```r
#observed traitmatching
traitmatchF<-abs(t(sapply(hum.morph$Bill,function(x){x-fl.morph$Corolla})))
rownames(traitmatchF)<-hum.morph$English
colnames(traitmatchF)<-fl.morph$Group.1
```


```r
#match names #Round to 2 decimals #Convert to cm for winbugs, avoids numerical underflow
traitmatchT<-round(traitmatchF[rownames(traitmatchF) %in% dath$Hummingbird,colnames(traitmatchF) %in% dath$Iplant_Double],2)
traitmatchT<-traitmatchT[sort(rownames(traitmatchT)),sort(colnames(traitmatchT))]
```

##Elevation ranges

Create a binary variable whether each observation was in a low elevation or high elevation transect. We have some species that just occur at the top of the gradient, and are not present in the sampling window of flowers at the low elevation.

Accounting for non-availability.
We have to figure out which plants were sampled in which periods, and if it was sampled, the non-detection are 0 if it wasn't the non-detection are NA. then remove all the Na's.


```r
elevH<-read.csv("data/HummingbirdElevation.csv",row.names=1)
head(elevH)
```

```
##                 Hummingbird    Low        m   High Index
## 1    White-whiskered Hermit 1340.0 1397.917 1606.4     1
## 2            Andean Emerald 1370.0 1416.417 1588.5     1
## 3    Stripe-throated Hermit 1360.0 1420.425 1640.0     1
## 4 Rufous-tailed Hummingbird 1368.5 1430.636 1690.0     1
## 5         Crowned Woodnymph 1343.5 1466.714 2300.0     1
## 6  Wedge-billed Hummingbird 1331.0 1616.850 2038.5     3
```

```r
colnames(elevH)[5]<-"Elevation"
elevH$Bird<-1:nrow(elevH)

#high elevation or low elevation
elevP<-read.csv("data/PlantElevation.csv",row.names=1)
colnames(elevP)[5]<-"Elevation"
elevP$Plant<-1:nrow(elevP)
elevP$Iplant_Double<-as.character(elevP$Iplant_Double)

#Merge to observed data
#plants
dathp<-merge(dath,elevP,by="Iplant_Double")

#birds
datph<-merge(dathp,elevH,by="Hummingbird")
```

What elevation transect is each observation in?
The camera data need to be inferred from the GPS point.


```r
#cut working best on data.frame
datph<-as.data.frame(datph)

#which elevation bin is each observation within
labs<-paste(seq(1300,2500,200),seq(1500,2700,200),sep="_")

#for the couple points that have 1290 elevation, round up to 300 for convienance
datph$ele[datph$ele < 1300]<-1301
datph$ele<-as.numeric(datph$ele)
datph$Transect_R[is.na(datph$Transect_R)]<-as.character(cut(datph[is.na(datph$Transect_R),]$ele,seq(1300,2700,200),labels=labs))

#Elev for the transects is the midpoint
tran_elev<-datph[datph$Survey_Type=='Transect',"Transect_R"]
datph[datph$Survey_Type=='Transect',"ele"]<-sapply(tran_elev,function(x){
  mean(as.numeric(str_split(x,"_")[[1]]))
})
```

### Define Time Events


```r
#ID for NA is holger transects, make the id's 1:n for each day of transect at each elevation, assuming no elevation was split across days.
datph$ID<-as.character(datph$ID)
noid<-datph[is.na(datph$ID),]

id_topaste<-paste(noid$Month,noid$Year,"Transect",sep="_")
datph[which(is.na(datph$ID)),"ID"]<-id_topaste

#Create year month combination
datph$Time<-paste(datph$Month,datph$Year,sep="_")

#Label survey type
datph$Survey_Type<-NA

mt<-!is.na(datph$TransectID)*1
datph$Survey_Type[mt==1]<-"Transect"
datph$Survey_Type[!datph$Survey_Type %in% "Transect"]<-"Camera"

datph<-datph[datph$Survey_Type=="Camera",]

#time filter

#sort by timestamp
datph<-datph[order(datph$timestamp),]

dotime<-function(d){
  d$Timediff<-NA
  if(nrow(d)>1){
  for (x in 2:nrow(d)){
  d$Timediff[x]<-difftime(d$timestamp[x],d$timestamp[x-1],units="mins")  
  }
  }
  return(d)
}

datph<-datph %>% group_by(ID,Hummingbird) %>% do(dotime(.))

#eliminate interaction by the same species within five minutes
datph<-datph[!1:nrow(datph) %in% which(datph$Timediff<5),]
#Day level
#add day ID
sdat<-split(datph,list(datph$ID),drop = T)

sdat<-lapply(sdat,function(x){
  x<-droplevels(x)
  x$Day<-as.numeric(as.factor(x$DateP))
  return(x)
})

indatraw<-rbind_all(sdat)

#Species names
for (x in 1:nrow(indatraw)){
  indatraw$Hummingbird[x]<-as.character(elevH[elevH$Bird %in% indatraw$Bird[x],"Hummingbird"])
  indatraw$Iplant_Double[x]<-as.character(elevP[elevP$Plant %in% indatraw$Plant[x],"Iplant_Double"])
}

#match the traits
traitmelt<-melt(traitmatchT)
colnames(traitmelt)<-c("Hummingbird","Iplant_Double","Traitmatch")

#dummy presence variable
indatraw$Yobs<-1

#prune columsn to make more readable
indatraw<-indatraw[,c("Hummingbird","Iplant_Double","ID","Time","Month","Year","Transect_R","ele","DateP","Yobs","Day","Survey_Type","Pierce")]
```

##Summarize daily interactions
To estimate the daily detectability, there can only be a max of one interaction per day.
We use mean elevation to average across observations within a transect

```r
indatraw<-indatraw %>% group_by(Hummingbird,Iplant_Double,ID,Day) %>% summarize(Yobs=sum(Yobs),Time=unique(Time),Transect_R=unique(Transect_R),Month=unique(Month),Year=unique(Year),ele=mean(ele),DateP=unique(DateP),Survey_Type=unique(Survey_Type)) %>% ungroup()
```


```r
indat<-prepData(indatraw)
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
```

#Hierarchical Model

For hummingbird species i feeding on plant species j observed at time k and sampling event observed by transect 

Observation Model:

$$ Yobs_{i,j,k,d} \sim Binomial(N_{i,j,k},\omega) $$

Process Model:

$$ N_{i,j,k} \sim Binomial(\lambda_{i,j,k}) $$
$$ logit(\lambda_{i,j,k}) = \alpha_i + \beta_{1,i} * Traitmatch_{i,j} $$

**Priors**

$$\phi \sim Normal(0,0.386) $$

$$ \alpha_i \sim Normal(\alpha_\mu,\alpha_\tau)$$
$$ \beta_{1,i} \sim Normal(\mu_{\beta_1,\tau_{beta_1}})$$

Group Level Logit Transformed Means
$$ \mu_\alpha \sim Normal(0,1.67)$$

$$ \mu_{\beta_1} \sim Normal(0,1.67)$$

Group Level Variance
$$ \tau_{\alpha} \sim Half cauchy(0,1,1)$$

$$ \tau_{\beta_1} \sim Half cauchy(0,1,1)$$


```r
#Source model
source("models/TraitMatch.R")
  
#print model
writeLines(readLines("models/TraitMatch.R"))
```

```
## sink("models/TraitMatch.jags")
## cat("
##     model {
##     
##     #Observation Model
##     for (x in 1:Nobs){
##     
##     #Observation Process
##     #True state
##     z[x] ~ dbern(detect[Bird[x]]) 
##     
##     #observation
##     logit(s[x])<-alpha[Bird[x]] + beta1[Bird[x]] * Traitmatch[Bird[x],Plant[x]] 
##     p[x]<-z[x] * s[x]
##     Yobs[x] ~ dbern(p[x])
##     
##     #Observed discrepancy
##     E[x]<-abs(Yobs[x]- s[x])/Nobs
##     }
##     
##     #Assess Model Fit - Predict remaining data
##     for(x in 1:Nnewdata){
##     
##     #Generate prediction
##       znew[x] ~ dbern(detect[NewBird[x]])
##       logit(snew[x])<-alpha[NewBird[x]] + beta1[NewBird[x]] * Traitmatch[NewBird[x],NewPlant[x]] 
##       pnew[x]<-znew[x]*snew[x]
##       Ynew_pred[x]~dbern(pnew[x])
##     
##     #Assess fit, proportion of corrected predicted links
##       E.new[x]<-abs(Ynew[x]-Ynew_pred[x])/Nnewdata
## 
##     }
##     
##     #Priors
##     #Observation model
##     #Detect priors, logit transformed - Following lunn 2012 p85
##     
##     for(x in 1:Birds){
##     logit(detect[x])<-dcam[x]
##     dcam[x]~dnorm(0,0.386)
##     }
##     
##     #Process Model
##     #Species level priors
##     for (i in 1:Birds){
##     
##     #Intercept
##     #logit prior, then transform for plotting
##     alpha[i] ~ dnorm(alpha_mu,alpha_tau)
##     
##     #Traits slope 
##     beta1[i] ~ dnorm(beta1_mu,beta1_tau)    
##     
##     }
##     
##     #Group process priors
##     
##     #Intercept 
##     alpha_mu ~ dnorm(0,0.386)
##     alpha_tau ~ dt(0,1,1)I(0,)
##     alpha_sigma<-pow(1/alpha_tau,0.5) 
##     
##     #Trait
##     beta1_mu~dnorm(0,0.386)
##     beta1_tau ~ dt(0,1,1)I(0,)
##     beta1_sigma<-pow(1/beta1_tau,0.5)
##     
##     #derived posterior check
##     fit<-sum(E[]) #Discrepancy for the observed data
##     fitnew<-sum(E.new[])
##     
##     }
##     ",fill=TRUE)
## 
## sink()
```

```r
#Cut Model into timeslives
indat<-indat %>% arrange(DateP)
indat$interval<-cut(as.POSIXct(indat$DateP),2)
indat$jinterval<-as.numeric(indat$interval)
intervals<-unique(indat$jinterval)
  
#split data
Yobs_dat<-indat[indat$jinterval <= intervals[1],]
Ynew_dat<-indat[indat$jinterval > intervals[1],]

#Run Model
mod<-runModel(Yobs_dat=Yobs_dat,Ynew_dat=Ynew_dat,jTraitmatch=jTraitmatch)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 3680
##    Unobserved stochastic nodes: 9310
##    Total graph size: 48563
## 
## Initializing model
```

```r
#Get Chains
chains<-data.frame(getChains(mod),jinterval=intervals[x])
predicted<-data.frame(getPredictions(mod,Ynew_dat=Ynew_dat),jinterval=intervals[[1]])
```

##Assess Convergence


```r
###Chains
ggplot(chains[chains$parameter %in% c("alpha_mu","beta1_mu"),],aes(x=Draw,y=value,col=as.factor(chain))) + geom_line() + facet_wrap(~parameter,scale="free") + theme_bw() + labs(col="Chain") 
```

<img src="figureObserved/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />



```r
ggplot(chains[chains$parameter %in% c("detect"),],aes(x=value),alpha=0.2)  + geom_density() + facet_wrap(~Index,scale="free",ncol=3) + theme_bw()  + ggtitle("Species Level")
```

<img src="figureObserved/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

```r
ggplot(chains[chains$parameter %in% c("fitnew"),],aes(x=value))  + geom_density()  + theme_bw() 
```

<img src="figureObserved/unnamed-chunk-14-2.png" style="display: block; margin: auto;" />


```r
ggplot(chains[chains$par %in% c("beta1_mu","beta1_sigma","alpha_mu","alpha_sigma"),],aes(x=Draw,y=value,col=as.factor(chain))) + geom_line() + theme_bw() + labs(col="Chain") + ggtitle("Group Level Parameters") + facet_wrap(~par,scales="free")
```

<img src="figureObserved/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

## Model fit


```r
sumfit<-chains[chains$par %in% c("fitnew"),]
ggplot(sumfit,aes(x=1-value)) + geom_density(fill="black") + theme_bw() + labs(x="Proportion of correctly predicted observations") + scale_x_continuous(labels=scales::percent)
```

<img src="figureObserved/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

```r
ggsave("Figures/pred_density.svg")
ggsave("Figures/pred_density.jpeg",height=3,width=7)
```

#Generate network


```r
posterior<-chains %>% filter(parameter %in% c("alpha","beta1")) %>% select(-par,jBird=Index) %>% spread(parameter,value) %>% merge(jagsIndexBird) 

genNetwork<-function(){
  m<-matrix(nrow=length(unique(Ynew_dat$Hummingbird)),ncol=length(unique(Ynew_dat$Iplant_Double)))
colnames(m)<-unique(Ynew_dat$Iplant_Double)
rownames(m)<-unique(Ynew_dat$Hummingbird)

for(bird in unique(Ynew_dat$Hummingbird)){
  for(plant in unique(Ynew_dat$Iplant_Double)){
    traitdiff<-traitmatchT[bird,plant]
    samp<-posterior %>% filter(Hummingbird %in% bird) %>% sample_n(1)
    m[bird,plant]<-genLink(alpha=samp$alpha,beta=samp$beta,traitdiff)
  }
}

nstat<-networklevel(m,index=c("connectance","niche overlap"),level="lower")
return(nstat)
}

netstat<-melt(t(sapply(1:100,function(k) genNetwork()))) 
  colnames(netstat)<-c("Iteration","Metric","value")

ggplot(netstat,aes(x=value)) + facet_wrap(~Metric,scales="free")  + geom_density(fill="black") + theme_bw()
```

<img src="figureObserved/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />


```r
byintervalspecies<-predicted %>% group_by(Hummingbird) %>% summarize(mean=mean(Yobs-value),upper=quantile(Yobs-value,0.95),lower=quantile(Yobs-value,0.05))
ggplot(byintervalspecies,aes(x=Hummingbird,y=mean)) + geom_point() + geom_line(aes(group=1)) + theme_bw() + facet_wrap(~Hummingbird,scales="free") + geom_hline(yintercept=0,linetype="dashed")
```

<img src="figureObserved/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

#Compare to model estimate when running the data


```r
#Run Model
mod2<-runModel(Yobs_dat=Ynew_dat,Ynew_dat=Ynew_dat,jTraitmatch=jTraitmatch)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 2783
##    Unobserved stochastic nodes: 8413
##    Total graph size: 43505
## 
## Initializing model
```

```r
#Get Chains
chains2<-data.frame(getChains(mod2),jinterval=intervals[x])
predicted2<-data.frame(getPredictions(mod2,Ynew_dat=Ynew_dat),jinterval=intervals[[1]])
```
How did the fit change


```r
chains$Model<-"Predict"
chains2$Model<-"Estimate"
alldat<-bind_rows(chains,chains2)
ggplot(alldat[alldat$parameter %in% c("beta1_mu"),],aes(x=value,fill=Model)) + geom_density(alpha=0.2) + facet_wrap(~parameter,scale="free") + theme_bw() 
```

<img src="figureObserved/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />


```r
sumfit<-chains[chains$par %in% c("fitnew"),]
sumfit2<-chains2[chains2$par %in% c("fitnew"),]
ggplot(sumfit2,aes(x=1-value)) + geom_density(fill="black",alpha=0.2) + geom_density(data=sumfit,fill="red",alpha=0.2) + theme_bw() + labs(x="Proportion of correctly predicted observations") + scale_x_continuous(labels=scales::percent)
```

<img src="figureObserved/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

```r
ggsave("Figures/PredLinksBoth.svg")
ggsave("Figures/PredLinksBoth.jpeg",height=3,width=7)
```

## Network


```r
posterior<-chains2 %>% filter(parameter %in% c("alpha","beta1")) %>% select(-par,jBird=Index) %>% spread(parameter,value) %>% merge(jagsIndexBird) 

genNetwork<-function(){
  m<-matrix(nrow=length(unique(Ynew_dat$Hummingbird)),ncol=length(unique(Ynew_dat$Iplant_Double)))
colnames(m)<-unique(Ynew_dat$Iplant_Double)
rownames(m)<-unique(Ynew_dat$Hummingbird)

for(bird in unique(Ynew_dat$Hummingbird)){
  for(plant in unique(Ynew_dat$Iplant_Double)){
    traitdiff<-traitmatchT[bird,plant]
    samp<-posterior %>% filter(Hummingbird %in% bird) %>% sample_n(1)
    m[bird,plant]<-genLink(alpha=samp$alpha,beta=samp$beta,traitdiff)
  }
}

nstat<-networklevel(m,index=c("connectance","niche overlap"),level="lower")
return(nstat)
}

netstat2<-melt(t(sapply(1:100,function(k) genNetwork()))) 
colnames(netstat2)<-c("Iteration","Metric","value")

ggplot(data=netstat2,aes(x=value)) + facet_wrap(~Metric)  + geom_density(data=netstat,fill="red",alpha=0.2)+ geom_density(fill="black",alpha=0.2) + theme_bw()
```

<img src="figureObserved/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

```r
ggsave("Figures/Networkstat.jpeg",height=3,width=7)
ggsave("Figures/Networkstat.svg",height=3,width=7)
```


```r
rect <- data.frame(xmin=as.POSIXct(min(indat$DateP)), xmax=as.POSIXct(unique(indat$interval)[2]), ymin=-Inf, ymax=Inf)

timeplot<-indat %>% group_by(DateP) %>% summarize(n=sum((Yobs>0*1)))
ggplot(timeplot,aes(x=as.POSIXct(DateP),n)) + geom_line() + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red",alpha=0.2,inherit.aes = FALSE) + theme_bw() + scale_x_datetime() + labs(x="Date",y="Observed Interactions")
```

<img src="figureObserved/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

```r
ggsave("Figures/timeplot.svg")
ggsave("Figures/timeplot.jpeg",height=3,width=7)
```



```r
save.image("ObservedModel.RData")
```
