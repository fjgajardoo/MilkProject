#### SETUP ----
## Load packages-----
library(MASS)
library(dplyr)
library(tidyverse)
library(tidyr)
# library(ggplot2)
# library(GGally)
library(lubridate)
library(psych)
# library(ggthemes)
library(tidyverse)
library(minpack.lm)
# library(gganimate)
library(nlme)
library(lme4)

## Import data----

dir.frans <- c('C:/Users/Francisco Gajardo/Documents/GitHub/MilkProject/')
# theme_set(theme_bw())
active.dir <- dir.frans; setwd(active.dir)

##Load from datamilk.csv
data <- read.csv(paste0(active.dir,
                      'datamilk.csv'), header=T, sep = ",")
# data[duplicated(data), ] #No duplicated data

##Load from treatments
treatment <- read.csv(paste0(active.dir,
                        'F1DB7_HistoryTreatments.csv'), header=T, sep = ";")

##Transform and create variables----

# treatment$TreatStartDate <- as.POSIXct(treatment$TreatStartDate,"%Y-%m-%d %H:%M:%S", tz = "GMT")
# treatment$TreatmentName<-ifelse(treatment$TreatmentName=="","Unspecified",treatment$TreatmentName)

#Milking dates corrected format
data$StartTime <- as.POSIXct(data$StartTime,"%Y-%m-%d %H:%M:%S", tz = "GMT") #Start time of session
data$EndTime <- as.POSIXct(data$EndTime, "%Y-%m-%d %H:%M:%S", tz = "GMT") #Endtime time of session
data$BDate <- as.POSIXct(data$BDate, "%Y-%m-%d %H:%M:%S", tz = "GMT") #Cow birthdate (age)
data$Calving <- as.POSIXct(data$Calving, "%d-%b-%Y %H:%M:%S", tz = "GMT") #Calving date
data$TMY<-data$TMY*1000 #Milk yield in grams
data$MYLF<-data$MYLF*1000
data$MYLH<-data$MYLH*1000
data$MYRF<-data$MYRF*1000
data$MYRH<-data$MYRH*1000

#Auxiliary variables
data$DateMilking<-as.Date(data$StartTime)
data$StartTimeHMS<-strftime(data$StartTime, format = "%H:%M:%S")
data$StartTimeHMS<-as.POSIXct(data$StartTimeHMS, format="%H:%M:%S", tz="GMT")
data$DIMaux<-floor(data$DIM) #Create aux DIM (rounded down)
data$grp <-paste(data$AniId,data$Lac)
data <- data %>% arrange (AniId,Lac,StartTime)%>% mutate(session = seq_along(AniId & Lac))
data$EffDIM <- difftime(data$StartTime,data$Calving,units = "days") #Effective DIM
data$DiffDIM<- as.numeric(data$DIM - data$EffDIM) #Difference BTWN DIM and effective DIM
data$SQMY <- data$MYLF + data$MYLH + data$MYRF + data$MYRH #Summed MY (4 quarters)
data$dev <- data$TMY - data$SQMY #Declared TMY
data$marker<-as.factor(ifelse(abs(data$dev)>2.6*1000,1,0))
MaxDIMS<-data%>%group_by(AniId,Lac, grp)%>%summarise(MaxDIM=max(DIM))
MaxDIMS_threshold<-subset(MaxDIMS,MaxDIM>400)
data$TMY_corrected <- data$TMY/(data$MI/60^2) #New variable, MI corrected TMY
data<-subset(data, AniId!=144) #Excluded Animal 144, explained below
# plot(density(MaxDIMS$MaxDIM))
# plot(density(MaxDIMS_threshold$MaxDIM))
data$MI_hr <- (data$MI)/60^2
data$logDIM <- ifelse(log(data$DIM)<0,0,log(data$DIM))
data$PAR2<-ifelse(data$Lac<=1,'Uniparous','Multiparous')

#DIVIDE LACTATION WHICH ARE EVIDENTLY TWO OR MORE
for (var in 1:length(data$AniId)){
  if(data$grp[var]=='29 1' && data$DIM[var]>=359){data$Lac[var]=5; data$DIM[var]=data$DIM[var]-359}else
    if(data$grp[var]=='52 4' && data$DIM[var]>=333){data$Lac[var]=6; data$DIM[var]=data$DIM[var]-333}else
      if(data$grp[var]=='90 1' && data$DIM[var]>=388){data$Lac[var]=3; data$DIM[var]=data$DIM[var]-388}else
        if(data$grp[var]=='157 1' && data$DIM[var]>=344){data$Lac[var]=5; data$DIM[var]=data$DIM[var]-344}
} 

data$DIMaux<-floor(data$DIM) #Create aux DIM (rounded down)
data$grp <-paste(data$AniId,data$Lac)
data <- data %>% arrange (AniId,Lac,StartTime)%>%group_by(AniId,Lac) %>% mutate(session = row_number())
data$EffDIM <- difftime(data$StartTime,data$Calving,units = "days") #Effective DIM
data$DiffDIM<- as.numeric(data$DIM - data$EffDIM) #Difference BTWN DIM and effective DIM
MaxDIMS<-data%>%group_by(AniId,Lac, grp)%>%summarise(MaxDIM=max(DIM))
MaxDIMS_threshold<-subset(MaxDIMS,MaxDIM>400)
LowDIMS<- MaxDIMS$grp[which(MaxDIMS$MaxDIM<200)]
data <- data[!(data$grp %in% LowDIMS),] #Remove lactations with MaxDIM<200
data$EC_AVG=(data$ECLF+data$ECLH+data$ECRF+data$ECRH)/4
data%>%filter(!(ECLF>EC_AVG*1.1 | ECRF>EC_AVG*1.1 | ECLH>EC_AVG*1.1 | ECRH>EC_AVG*1.1))
# length(data$AniId) #Now 367424

#MY corrected by MI ANALYSIS----
# count(data[data$MI<4*60^2,]) #296 milking sessions under 4 hours
# View(data[data$MI<4*60^2,])
# aux_MI<- which(data$MI<4*60^2)
# aux_MI2<-aux_MI-1
# aux_MI3<- sort(rbind(aux_MI,aux_MI2), decreasing = F)
# 
# aux_dataMI<-(data[aux_MI,])
# aux_dataMI2<-(data[aux_MI2,])
# aux_dataMI3<-(data[aux_MI3,])
# 
# ggplot(data,aes(x=DIM, y=TMY))+geom_point()
# ggplot(aux_dataMI,aes(x=DIM, y=TMY))+geom_point()

# outliers_TMY <- boxplot(data$TMY)$out
# outliers<-which(data$TMY %in% outliers_TMY)

# count(data[which(aux_MI%in%outliers),])
# count(data[which(aux_MI2%in%outliers),])
# count(data[which(aux_MI3%in%outliers),])

#NA Values (24 from data, 96 Quarter level)<- FAILED MILK SESSIONS DISCARDED----
data_NA<-data %>% filter(is.na(TMY)|is.na(MYLF)|is.na(MYRF)|is.na(MYLH)|is.na(MYRH)|
                           is.na(Lac)|is.na(AniId)|is.na(DIM)|is.na(MI)|is.na(ECLF)|
                           is.na(ECRF)|is.na(ECLH)|is.na(ECRH))
data<-data %>% filter(!is.na(TMY)&!is.na(MYLF)&!is.na(MYRF)&!is.na(MYLH)&!is.na(MYRH)&!
                        is.na(Lac)&!is.na(AniId)&!is.na(DIM)&!is.na(MI)&!is.na(ECLF)&!
                        is.na(ECRF)&!is.na(ECLH)&!is.na(ECRH))

##Data Cleaning----

#Delete MI below 4
# count(data[data$MI<4*60^2,]) #296 milking sessions under 4 hours
# ggplot(data, aes(MI/60^2)) + geom_density()+ ggtitle('MI Density')+
#   geom_vline(xintercept = 4, col='cadetblue')+labs(y='Milk Interval (hrs)')
# data<-data%>%filter(MI_hr>=4) #To clean data from <4 MI


#PENDIENTE Correcting MI of first milking in lactation ----
# data$first_session<-ifelse(data$session==1,1,0)
# ggplot(data, aes(y=MI/60^2, x=session)) + geom_point()+ facet_grid(factor(data$first_session))
# hist(subset(data, session==1)$MI_hr)
# describe(subset(data, session==1)$MI_hr)
# describe(subset(data, session!=1)$MI_hr)


bound<-quantile(data$MI_hr, na.rm = T)[4] + IQR(data$MI_hr, na.rm = T)*1.5

data$MI_hr<-ifelse(data$session==1 & as.numeric(data$MI_hr)>bound, NaN, data$MI_hr)
data$MI<-ifelse(data$session==1 & as.numeric(data$MI_hr)>bound, NaN, data$MI)


# mean(!is.nan(subset(data,session==1)$MI/60^2))
# data$MI<-ifelse(data$MI==48*60^2,NaN,data$MI)
# data<-data[,-first_session]


#TMY Deviation
# summary(data$TMY)
# summary(data$SQMY)
# summary(data$dev)
# ggplot(data, aes(x=StartTime, y=dev))+geom_point()+ggtitle("MY deviation: Total vs Sum of quarters")+
#   geom_smooth(method = "lm", formula = y~x, colour = "cadetblue", level=0.5)+ labs(x='Milking date', y='deviation (kgs)')
# quantile(abs(data$dev),.999) #Under 2.6 is the 99% in Absolute values (4194 out?)
# ggplot(data, aes(x=abs(dev)))+geom_density()
# count(data[abs(data$dev)>2.6,])/length(data$AniId)

#CHECK INFLUENTIAL OBSERVATIONS AFTER WOOD AND ADRIAENS

#Delete Lactation Zero data (DECIDED TO EXCLUDE THIS ANIMAL 2, WEIRD LACTATIONS)

# ggplot(data, aes(as.factor(Lac))) + geom_bar(fill='cadetblue') + ggtitle('Lactations frequency')+
#   labs(x='Lactation number',y='count')
# freq<-as.data.frame(table(data$Lac));colnames(freq)=c('Lac', 'Freq')
# percent<-as.data.frame(prop.table(table(data$Lac))); freq$percent <- round(percent[,2]*100,2)
# freq #1006 observations
# percent #0.26% of datapoints
# Lac0.df<-data[which(data$Lac==0),]
# unique(Lac0.df[,'AniId']) #LACTATIONS ZERO COME FROM THE SAME ANIID 144
# 
# for(i in 144:144){
#   aux1 <- data%>%filter(AniId==i)
#   for(j in 0:0){
#     aux2 <- aux1%>%filter(Lac==j)
#     if (length(aux2$TMY)>=1){
#       plot(aux2$TMY_corrected~aux2$DIM, type="l", col=aux2$AniId[1], xlab='DIM',ylab='TMY',
#            main=paste('Cow',aux2$AniId[1],' Lactation', aux2$Lac[1]))}
#     par(new=T)
#     plot((aux2$MI/60^2)~aux2$DIM,type='l', col='red')
#     abline(h=20, col='blue')
#   }}
# 
# data<-subset(data, AniId!=144)

#Plot lactations longer than 400 days

#Correct Effective DIM--- Zero for every case
# describe(data$DIM)
# boxplot(data$DiffDIM, ylim=range(-0.001,0.001))
# plot(data$DiffDIM, ylim=range(-0.001,0.001))

#CORRECT POSSIBLE DUPLICATES----

# data[duplicated(data), ] #No duplicated data

#Create Daily Dataset---- NOT NECESSARY

# data_daily<-data%>%group_by(AniId,Lac,DateMilking)%>%summarise(DTMY=sum(TMY),DIM=mean(as.numeric(DIMaux)))
# data_daily$DIM<-data_daily$DIM+1

#Create Quarter datasets----
dataQTR <- data %>% gather(Quarter, QMY, MYLF:MYRH) %>% 
  mutate(Quarter=substr(Quarter, 3, 4), QEC=ifelse(Quarter=='LF', ECLF, 
                                                   ifelse(Quarter=='LH', ECLH, 
                                                          ifelse(Quarter=='RF', ECRF, ECRH))))

dataQTR <- dataQTR  %>% group_by(AniId, Lac, Quarter) %>% select(-c(ECLF:ECRH))
dataQTR$QMY_corrected <- dataQTR$QMY/(dataQTR$MI_hr) #QMY considering MI
dataQTR$POS2<- ifelse(dataQTR$Quarter %in% c('LF','RF'),'Front','Hind')
dataQTR$grpQTR<-paste(dataQTR$grp,dataQTR$Quarter)
dataQTR<- subset(dataQTR, select = -c(Name,UserN,LifeNumber, Dest))

dataQTR$Aux_MinMI<-0
for (i in 1:length(dataQTR$Aux_MinMI)){
if(!is.na(dataQTR$MI_hr[i]) & dataQTR$MI_hr[i]<4 & dataQTR$session[i]>1){
  dataQTR$Aux_MinMI[i]<-1
  dataQTR$Aux_MinMI[i-1]<-1} else
    if(!is.na(dataQTR$MI_hr[i]) & dataQTR$MI_hr[i]<4 & dataQTR$session[i]==1){
      dataQTR$Aux_MinMI[i]<-1}
}
 
# dataQTR_1<- dataQTR %>% filter(QMY>0.25*1000 | TMY>1*1000 , MI_hr<24, MI_hr>=4, DIM<=305)
dataQTR_1<- dataQTR %>% filter(QMY>0.25*1000 | TMY>1*1000 , MI_hr<24, Aux_MinMI!=1, DIM<=305)
#Try to add that previous milking need to have pMI<24 and pMI>4 too

#Data QTR daily (Para Wood1)  
dataQTR_daily<-dataQTR_1%>%group_by(AniId,Lac, Quarter,DateMilking, grp, PAR2, POS2)%>%
  summarise(DQMY=sum(QMY/1000),DIM=ceiling(mean(as.numeric(DIM))),std.DQMY=sum(QMY/1000)/sum(MI_hr))%>%filter(DIM<=305)

