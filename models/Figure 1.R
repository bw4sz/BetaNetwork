library(ggplot2)
library(dplyr)
set.seed(1)

qplot(rpois(1e5,3)) + labs(y="",x="Interaction Intensity") + theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("Figures/Eq1.svg")


out<-list()
for(x in seq(1,12,4)){
  out[[x]]<-data.frame(Month=month.name[x],Lambda=rpois(1e5,runif(1,0,5)))
} 
dat<-bind_rows(out)

ggplot(dat) + geom_histogram(position=position_dodge(),aes(x=Lambda,fill=Month),binwidth = 1) + labs(y="",x="Interaction Intensity") + theme_bw()  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("Figures/Eq2.svg")

out<-list()
for(x in seq(1,12,4)){
  abundance=rep(x,1e3)
  alpha=abs(rnorm(1e3,0.1,0.1))
  lambda<-exp(alpha+0.4*abundance)
  obs<-sapply(lambda,function(x) rpois(1,x))
  out[[x]]<-data.frame(Month=month.name[x],obs,abundance)
} 
dat<-bind_rows(out)
ggplot(dat) + geom_histogram(position=position_dodge(),aes(x=obs,fill=Month),binwidth = 1) + labs(y="",x="Interaction Intensity") + theme_bw() 
ggsave("Figures/Eq3.svg")
ggplot(dat,aes(x=abundance,y=obs,col=Month)) + geom_point() + stat_smooth(method = "glm",aes(group=1)) + theme_bw() + labs(x="Abundance of i",y="Interaction Intensity")
ggsave("Figures/Eq3_2.svg")

out<-list()

traiti<-10.0
traitj=c(3.0,10.4,15.0)

for(x in 1:length(seq(1,12,4))){
  abundance=rep(seq(1,12,4)[x],1e3)
  alpha=rep(rnorm(100,3,0.01),10)
  tdiff<-abs(traiti-traitj[x])
  Lambda=sapply(exp(alpha+-0.6*tdiff + 0.2* abundance ),function(x) {rpois(1,x)})
  out[[x]]<-data.frame(Month=month.name[x],Lambda,tdiff,abundance)
} 
dat<-bind_rows(out)
ggplot(dat) + geom_histogram(position=position_dodge(),aes(x=Lambda,fill=as.factor(Month))) + labs(y="",x="Interaction Intensity",fill="Month") + theme_bw() 

ggsave("Figures/Eq4.svg")
ggplot(dat,aes(x=tdiff,y=Lambda)) + geom_point() + stat_smooth(method = "glm",aes(group=1),method.args = list(family="poisson")) + theme_bw() + labs(x="Disimilarity in morphology",col="Site",y="Interaction Intensity")
ggsave("Figures/Eq4_2.svg")



out<-list()

traiti<-10.0
traitj=c(10.4,10.4,10.4)
elev<-c(0.1,1,3)
alpha2=0.03
beta2=0.7
alpha=0.01

for(x in 1:length(seq(1,12,4))){
  abundance=rpois(1e3,lambda=exp(alpha2 + beta2*elev[x]))
  tdiff<-abs(traiti-traitj[x])
  Lambda=sapply((exp(alpha+-0.2*tdiff) * abundance),function(x) {rpois(1,x)})
  out[[x]]<-data.frame(Month=month.name[x],Lambda,tdiff,abundance,elev=elev[x])
} 
dat<-bind_rows(out)
ggplot(dat) + geom_histogram(position=position_dodge(),aes(x=Lambda,fill=as.factor(elev*1000))) + labs(y="",x="Interaction Intensity",fill="Site Elevation (m)") + theme_bw() +   theme(axis.title.y=element_blank(),
                                                                                                                                                                                        axis.text.y=element_blank(),
                                                                                                                                                                                        axis.ticks.y=element_blank())
ggsave("Figures/Eq5.svg")
ggplot(dat,aes(x=elev*1000,y=Lambda)) + geom_point() + stat_smooth(method = "glm",aes(group=1),method.args = list(family="poisson")) + theme_bw() + labs(x="Site Elevation (m)",col="Site",y="Interaction Intensity")
ggsave("Figures/Eq5_2.svg")

