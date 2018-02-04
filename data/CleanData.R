#Prep Data to create full dataset.
#This is a legacy to document steps to create the fulldata.csv in the repo
library(dplyr)
library(tidyr)
library(stringr)
library(chron)
#Load data
#read in flower morphology data, comes from Nectar.R
fl.morph<-read.csv("data/FlowerMorphology.csv")

#use effective corolla where possible.
fl.morph$Corolla<-fl.morph$EffectiveCorolla
fl.morph[is.na(fl.morph$Corolla),"Corolla"]<-fl.morph[is.na(fl.morph$Corolla),"TotalCorolla"]

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

#Still some time formats to be fixed!
int$timestamp[is.na(int$timestamp)]<-as.POSIXct(int$DateP[is.na(int$timestamp)],format="%Y-%m-%d")

int$timestamp<-as.character(int$timestamp)

#one date error
int[int$DateP %in% '2013-07-25',"Month"]<-7

#one duplicate camera error, perhaps two GPS records.
int<-int[!(int$ID %in% "FH1108" & int$Date_F %in% '2014-12-01'),]

#Correct known taxonomic disagreements, atleast compared to traits
int[int$Iplant_Double %in% "Onagaraceae fuschia","Iplant_Double"]<-"Fuchsia macrostigma"
int[int$Iplant_Double=="Alloplectus purpureus","Iplant_Double"]<-"Glossoloma purpureum"
int[int$Iplant_Double=="Alloplectus purpuruem","Iplant_Double"]<-"Glossoloma purpureum"
int[int$Iplant_Double=="Capanea affinis","Iplant_Double"]<-"Kohleria affinis"
int[int$Iplant_Double=="Columnea cinerea","Iplant_Double"]<-"Columnea mastersonii"
int[int$Iplant_Double=="Alloplectus teuscheri","Iplant_Double"]<-"Drymonia teuscheri"
int[int$Iplant_Double=="Drymonia collegarum","Iplant_Double"]<-"Alloplectus tetragonoides"

#Some reasonable level of presences, 3 points
keep<-names(which(table(int$Hummingbird) > 3))

m.dat<-droplevels(int[colnames(int) %in% c("ID","Video","Time","Hummingbird","Sex","timestamp","TransectID","Transect_R","Iplant_Double","Pierce","DateP","Month","ele","Type")])

#Does the data come from camera or transect?
m.dat$Type<-(is.na(m.dat$TransectID))*1

#remove transect data
m.dat<-m.dat %>% filter(Type==1)
m.dat$Year<-years(as.Date(m.dat$DateP))

#one missing date
m.dat$Year[m.dat$Year %in% 2012]<-2013
m.dat$Year[m.dat$Year %in% 2106]<-2016

##Combine with newer data
#Combine with more recent interaction matrix
ndat<-read.csv("/Users/ben/Dropbox/HummingbirdProject/Data/HummingbirdProjectCleaned/Interactions.csv")

#For the moment, kill all data with no elevation records.
ndat<-ndat %>% filter(site %in% c("Maquipucuna","SantaLuciaLower","SantaLuciaUpper"))  %>% select(ID=Camera_ID,folder,DateP=date,Time=time,Latin_Hummingbird=hummingbird,Sex=sex,Pierce=piercing,ele,Iplant_Double=plant_field_name) %>% mutate(Video=str_match(folder,"/(\\w+)")[,2]) %>% select(-folder) %>% filter(!is.na(ele),!Time=="")

ndat$timestamp<-as.character(strptime(paste(ndat$DateP,ndat$Time,sep=" "),"%d/%m/%Y %H:%M"))

#A few malformed timestamps
ndat$Year<-years(ndat$timestamp)
ndat$timestamp[ndat$Year %in% c("17","18")]<-as.character(strptime(paste(ndat$DateP[ndat$Year %in% c("17","18")],ndat$Time[ndat$Year %in% c("17","18")],sep=" "),"%d/%m/%y %H:%M"))

name_list<-hum.morph %>% select(Latin_Hummingbird=double,Hummingbird=English) 

# TODO Add in empress brilliant "Heliodoxa imperatrix"
ndat<-ndat %>% inner_join(name_list) %>% select(-Latin_Hummingbird) 

fulldat<-bind_rows(m.dat,ndat) %>% select(-Type,-Year,-Month,-TransectID,-Transect_R)
write.csv(fulldat,"data/fulldat.csv")
