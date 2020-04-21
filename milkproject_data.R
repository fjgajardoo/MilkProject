## Data processing
## Load packages-----
library(MASS)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)
library(lubridate)
library(psych)
library(tidyverse)
library(minpack.lm)
library(nlme)
library(lme4)

## Import data----

dir.frans <- c('C:/Users/Francisco Gajardo/Documents/GitHub/MilkProject/')
active.dir <- dir.frans; setwd(active.dir) #Setting directory

##Load from datamilk.csv
data <- read.csv(paste0(active.dir,
                      'datamilk.csv'), header=T, sep = ",")

##Load from treatments
treatment <- read.csv(paste0(active.dir,
                        'F1DB7_HistoryTreatments.csv'), header=T, sep = ";")

##Transform and create variables----

#From Treatment records
treatment$TreatStartDate <- as.POSIXct(treatment$TreatStartDate,"%Y-%m-%d %H:%M:%S", tz = "GMT")
treatment$TreatmentName<-ifelse(treatment$TreatmentName=="","Unspecified",treatment$TreatmentName)

# Removing NA Values (24 from data, 96 Quarter level)<- FAILED MILK SESSIONS DISCARDED----
data<-data %>% filter(!is.na(TMY)&!is.na(MYLF)&!is.na(MYRF)&!is.na(MYLH)&!is.na(MYRH)&!
                        is.na(Lac)&!is.na(AniId)&!is.na(DIM)&!is.na(MI)&!is.na(ECLF)&!
                        is.na(ECRF)&!is.na(ECLH)&!is.na(ECRH))

#Milking corrected format
data$StartTime <- as.POSIXct(data$StartTime,"%Y-%m-%d %H:%M:%S", tz = "GMT") #Start time of session
data$EndTime <- as.POSIXct(data$EndTime, "%Y-%m-%d %H:%M:%S", tz = "GMT") #Endtime time of session
data$BDate <- as.POSIXct(data$BDate, "%Y-%m-%d %H:%M:%S", tz = "GMT") #Cow birthdate (age)
data$Calving <- as.POSIXct(data$Calving, "%d-%b-%Y %H:%M:%S", tz = "GMT") #Calving date
data$TMY<-data$TMY*1000 #Milk yield in grams
data$MYLF<-data$MYLF*1000
data$MYLH<-data$MYLH*1000
data$MYRF<-data$MYRF*1000
data$MYRH<-data$MYRH*1000

#Creating auxiliary variables
data$DateMilking<-as.Date(data$StartTime) #Only date
data$grp <-paste(data$AniId,data$Lac) #Animal-Lactation ids
data <- data %>% arrange (AniId,Lac,StartTime)%>%group_by(AniId,Lac) %>% 
  mutate(session = row_number()) #Session per Animal-Lactation
data$EffDIM <- difftime(data$StartTime,data$Calving,units = "days") #Effective DIM
data$DiffDIM<- as.numeric(data$DIM - data$EffDIM) #Difference BTWN DIM and effective DIM
data$SQMY <- data$MYLF + data$MYLH + data$MYRF + data$MYRH #Summed MY (4 quarters)
data$dev <- ifelse(data$TMY==0, NaN, ((data$TMY - data$SQMY)/data$TMY)) #Percentual deviation

#Correct Lactation which are evidently two or more
MaxDIMS<-data%>%group_by(AniId,Lac, grp)%>%summarise(MaxDIM=max(DIM)) #Identyfing MaxDIM for each lactation
MaxDIMS_threshold<-subset(MaxDIMS,MaxDIM>400) #Subset of Milking from long lactations (Arbitrary bound)
for (var in 1:length(data$AniId)){
  if(data$grp[var]=='29 1' && data$DIM[var]>=359){data$Lac[var]=5; data$DIM[var]=data$DIM[var]-359}else
    if(data$grp[var]=='52 4' && data$DIM[var]>=333){data$Lac[var]=6; data$DIM[var]=data$DIM[var]-333}else
      if(data$grp[var]=='90 1' && data$DIM[var]>=388){data$Lac[var]=3; data$DIM[var]=data$DIM[var]-388}else
        if(data$grp[var]=='157 1' && data$DIM[var]>=344){data$Lac[var]=5; data$DIM[var]=data$DIM[var]-344}
} 

#Remove lactations with less than 200 days in milking
LowDIMS<- MaxDIMS$grp[which(MaxDIMS$MaxDIM<200)]
data <- data[!(data$grp %in% LowDIMS),] #Remove lactations with MaxDIM<200

data$MI_hr <- (data$MI)/60^2 #MI was in seconds. New variable expressed in hours
data$TMY_corrected <- data$TMY/(data$MI_hr) #New variable, MI corrected TMY
data<-subset(data, AniId!=144) #Excluded Animal 144 (Lactation zero, long MI, etc.)
data$logDIM <- ifelse(log(data$DIM)<0,0,log(data$DIM)) #Use instead of log(DIM)
data$PAR2<-factor(ifelse(data$Lac<=1,'Uniparous','Multiparous')) #2 Level factor
data <- data %>% arrange (AniId,Lac,StartTime)%>%group_by(AniId,Lac) %>% mutate(session = row_number())
data$EffDIM <- difftime(data$StartTime,data$Calving,units = "days") #Effective DIM
data$DiffDIM<- as.numeric(data$DIM - data$EffDIM) #Difference BTWN DIM and effective DIM

#Removing datapoints with EC out of range (>110% avg in at least 1 quarter)
data$EC_AVG<-(data$ECLF+data$ECLH+data$ECRF+data$ECRH)/4
data<-data%>%filter(!(ECLF>EC_AVG*1.1 | ECRF>EC_AVG*1.1 | ECLH>EC_AVG*1.1 | ECRH>EC_AVG*1.1))

##Data Cleaning----

#Delete MI below 4
#length(data[data$MI<4*60^2,]$AniId) #174 milking sessions under 4 hours
# ggplot(data, aes(MI/60^2)) + geom_density()+ ggtitle('MI Density')+
#   geom_vline(xintercept = 4, col='cadetblue')+labs(y='Milk Interval (hrs)')
# data<-data%>%filter(MI_hr>=4) #To clean data from <4 MI

#Define as NaN MI from first sessions extremely high (bound=14.9)
bound<-quantile(data$MI_hr, na.rm = T)[4] + IQR(data$MI_hr, na.rm = T)*1.5
data$MI_hr<-ifelse(data$session==1 & as.numeric(data$MI_hr)>bound, NaN, data$MI_hr)
data$MI<-ifelse(data$session==1 & as.numeric(data$MI_hr)>bound, NaN, data$MI)

#TMY Deviation: Exclude data where SQMY is deviated in a 20% from TMY
data<-data[data$dev %inrange% c(-0.05,0.05),]

#Correct possible duplicates
# data[duplicated(data), ] #No duplicated data

#Create Quarter datasets----
dataQTR <- data %>% gather(Quarter, QMY, MYLF:MYRH) %>% 
  mutate(Quarter=substr(Quarter, 3, 4), QEC=ifelse(Quarter=='LF', ECLF, 
                                                   ifelse(Quarter=='LH', ECLH, 
                                                          ifelse(Quarter=='RF', ECRF, ECRH))))

dataQTR <- dataQTR  %>% group_by(AniId, Lac, Quarter) %>% select(-c(ECLF:ECRH))
dataQTR$QMY_corrected <- dataQTR$QMY/(dataQTR$MI_hr) #QMY considering MI
dataQTR$POS2<- factor(ifelse(dataQTR$Quarter %in% c('LF','RF'),'Front','Hind'))#Create 2 level factor of position #
dataQTR$grpQTR<-paste(dataQTR$grp,dataQTR$Quarter) #Create Animal-Lactation-Quarter ID

#Exclude datapoints with MI<4 as well as previous milking
dataQTR$Aux_MinMI<-0
for (i in 1:length(dataQTR$Aux_MinMI)){
if(!is.na(dataQTR$MI_hr[i]) & dataQTR$MI_hr[i]<4 & dataQTR$session[i]>1){
  dataQTR$Aux_MinMI[i]<-1
  dataQTR$Aux_MinMI[i-1]<-1} else
    if(!is.na(dataQTR$MI_hr[i]) & dataQTR$MI_hr[i]<4 & dataQTR$session[i]==1){
      dataQTR$Aux_MinMI[i]<-1}
}
dataQTR_1<- dataQTR %>% filter(QMY>0.25*1000 | TMY>1*1000 , MI_hr<24, Aux_MinMI!=1, DIM<=305)

#Create dataset at day level (For 2-step data cleaning process (Wood1 and Wood2)  
dataQTR_daily<-dataQTR_1%>%group_by(AniId,Lac, Quarter,DateMilking, grp, PAR2, POS2)%>%
  summarise(DQMY=sum(QMY/1000),DIM=ceiling(mean(as.numeric(DIM))),std.DQMY=sum(QMY/1000)/sum(MI_hr))%>%filter(DIM<=305)

