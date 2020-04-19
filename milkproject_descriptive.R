#### DESCRIPTIVE ANALYSIS----

#Milking data set
# length(data$AniId) #392495 data points
# length(dataQTR$AniId) #1569980 data points
# length(unique(data$AniId)) #216 cows
# count(unique(data[,c('AniId','Lac')]))#501 different lactations
# # p<-ggplot(data, aes(x=DateMilking,fill=factor(marker)))+geom_bar()+labs(y='count of sessions', x='Milking date')#Milking sessions per day
# p<-ggplot(data, aes(x=DateMilking))+geom_bar(col='cadetblue')+labs(y='count of sessions', x='Milking date')#Milking sessions per day
# p + scale_fill_manual(values=c("cadetblue", "brown"))+ggtitle('Count of sessions per date')
# cowsperday <- data%>%group_by(DateMilking)%>% summarise(count = n_distinct(AniId))
# ggplot(cowsperday, aes(x=DateMilking, y=count))+geom_col(fill='cadetblue')+ggtitle('Count of cows per date')+
#   labs(y='count of sessions', x='Milking date')#Number of animals per day
# 
# #Currently 89 AniIds coincide btw both datasets
# length(treatment$Animal)
# milking_ids<-data.frame(unique(data$AniId)); colnames(milking_ids)<-c("IDS")#216
# treatment_ids<-data.frame(unique(treatment$Animal)); colnames(treatment_ids)<-c("IDS")#436
# merged_ids<-merge(milking_ids,treatment_ids,by.x ="IDS", by.y="IDS"); df_both<-data.frame(merged_ids)#89
# milking_ids[-merged_ids$IDS,];df_onlymilking<-data.frame(milking_ids)
# treatment_ids[-merged_ids$IDS,]; df_onlytreatment<-data.frame(treatment_ids)
# View(treatment$TreatStartDate)
# str(data); str(dataQTR)
# describe(data); describe(dataQTR)
# 
# #MY descriptives----
# summary(data$TMY) #24 NA'S
# ggplot(data, aes(y=TMY,x=factor(1)))+geom_violin(fill='cadetblue')+ggtitle('TMY Violin Plot')+labs(x="")
# summary(dataQTR$QMY) #96 NA'S ALREADY REMOVED!
# ggplot(dataQTR, aes(y=QMY,x=factor(1)))+geom_violin(fill='cadetblue')+ggtitle('QMY Violin Plot')+labs(x="")+facet_grid(.~Quarter,margins = T)
# quantile(dataQTR$QMY,.99) #Under 5.6 is the 99%
# #ISSUE: Extreme values in Quarter Yields
# summary(data$MYLF);summary(data$MYLH);summary(data$MYRF);summary(data$MYRH)
# summary(dataQTR$QMY)
# p<-ggplot(dataQTR, aes(y=QMY),fill=factor(Quarter))+geom_boxplot()+facet_grid(.~Quarter)+
#   geom_hline(yintercept = mean(dataQTR$QMY),col='red', lty=2)
# p+theme(axis.text.x = element_blank(), axis.ticks = element_blank(), legend.title = element_blank())+
#   ggtitle('QMY by Quarter position')
# 
# ggplot(data, aes(x=TMY))+geom_density()
# ggplot(dataQTR, aes(x=QMY))+geom_density()
# 
# outliers_TMY <- boxplot.stats(data$TMY)$out
# outliers_QMY <- boxplot.stats(dataQTR$QMY)$out
# max_dev <- which(data$marker==1)
# out<-length(which(data$TMY %in% outliers_TMY))#By regular IQR rule 17758 outliers
# out/length(data$AniId)#%4,5%
# 
# #Lactation descriptive----
# Lactations <- data %>% group_by(AniId, Lac) %>% summarise(Frequency = n_distinct(StartTime))
# summary(Lactations)
# ggplot(Lactations, aes(x=Frequency))+geom_bar() #One case with 200 registers
# ggplot(Lactations, aes(y=Frequency))+geom_boxplot()
# 
# Parities<- data %>% group_by(Lac)%>%summarise(Frequency=n_distinct(AniId)/501)
# 
# ggplot(data, aes(as.factor(Lac))) + geom_bar() + ggtitle('Lactations frequency')
# 
# ggplot(data, aes(y=TMY, x=factor(Lac))) + geom_boxplot(fill='cadetblue') + 
#   ggtitle('TMY Boxplots by Lactation') + labs(x="Lactation")
# 
# ggplot(dataQTR, aes(y=QMY, x=factor(Lac))) + geom_boxplot(fill='cadetblue') + 
#   ggtitle('QMY Boxplots by Lactation')+ labs(x="Lactation")
# 
# 
# ##EC (Not)----
# #EC Descriptives (Don't consider ECin your first analyses)
# # summary(data$ECLF); summary(data$ECLH); summary(data$ECRF); summary(data$ECRH)
# # summary(dataQTR$QEC)
# 
# ##DIM descriptives----
# summary(data$DIM)
# ggplot(data, aes(y=DIM,x=factor(1)))+geom_violin(fill='cadetblue')+ggtitle('DIM Violin Plot')+labs(x="")
# DIM_density_total = ggplot(data) +
#   stat_density_2d(aes(x=DIM,y=TMY, fill=..density..), geom = "raster", contour = FALSE) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_fill_gradient(low="white", high="red")
# DIM_density_total+ggtitle('DIM density heatmap')
# # plot(dataQTR$DIM, dataQTR$QMY)
# 
# #Considering milkings
# ggplot(data, aes(x=DIM))+geom_histogram(binwidth = 10,fill='darkblue',alpha=0.7)+ggtitle('DIM per session')+labs(x='DIM',y='count of sessions')
# ggplot(data, aes(x=DIM))+stat_ecdf(geom = "step",color='darkblue')+geom_hline(yintercept = .95)
# quantile(data$DIM,.95) #Under 357 is the 95%
# 
# #Considering lactations
# ggplot(MaxDIMS, aes(x=MaxDIM))+geom_histogram(binwidth = 10,fill='darkblue',alpha=0.7)+ggtitle('Max DIM per Lactation')+labs(x='DIM',y='count of lactations')
# ggplot(MaxDIMS, aes(x=MaxDIM))+stat_ecdf(geom = "step",color='darkblue')+geom_hline(yintercept = c(.95,.05))
# quantile(MaxDIMS$MaxDIM,.95) #Under 452 is the 95%
# quantile(MaxDIMS$MaxDIM,.05) #Above 35 is the 95%
# summary(MaxDIMS$MaxDIM)
# # DIM_density_quarter = ggplot(dataQTR) +
# #   stat_density_2d(aes(x=DIM,y=QMY, fill=..density..), geom = "raster", contour = FALSE) +
# #   scale_x_continuous(expand=c(0,0)) +
# #   scale_y_continuous(expand=c(0,0)) +
# #   scale_fill_gradient(low="white", high="red")
# # DIM_density_quarter
# 
# ggplot(subset(data,DIM<452), aes(y=TMY, x=DIM)) + geom_smooth() + ggtitle('TMY on DIM')
# ggplot(subset(dataQTR,DIM<452), aes(y=QMY, x=DIM)) + geom_smooth() + ggtitle('QMY on DIM')
# 
# ggplot(subset(data,Lac==1 & DIM<452), aes(y=TMY, x=DIM)) + geom_smooth() + ggtitle('TMY on DIM - Lactation 1')
# ggplot(subset(dataQTR,Lac==1 & DIM<452), aes(y=QMY, x=DIM)) + geom_smooth() + ggtitle('QMY on DIM - Lactation 1')
# 
# 
# ggplot(subset(data,Lac>1 & DIM<452), aes(y=TMY, x=DIM)) + geom_smooth() + ggtitle('TMY on DIM - Lactation 2+')
# ggplot(subset(dataQTR,Lac>1 & DIM<452), aes(y=QMY, x=DIM)) + geom_smooth() + ggtitle('QMY on DIM - Lactation 2+')
# 
# ##MI descriptives
# 
# summary(data$MI/60^2)
# 
# MI_density_total = ggplot(data) +
#   stat_density_2d(aes(x=MI/60^2,y=TMY, fill=..density..), geom = "raster", contour = FALSE) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   scale_fill_gradient(low="white", high="blue")
# MI_density_total+ggtitle('MI density heatmap')
# 
# 
# ggplot(dataQTR, aes(y=MI/60^2, x=factor(1), fill=factor(Quarter))) + geom_boxplot() +facet_grid(.~Quarter, margins=T)+
#   ggtitle('Milk Interval Boxplots by Quarter')+ labs(y='MI (hours)', x='')+
#   theme(axis.text.x = element_blank())#Confirm why I have same for LF,RF and LH,RH
# # Within each lactation, on average 2,64% of the observations were
# # corrected for a low MI (MI<4HOURS)
# 

#Lactation Curves----

# par(mfrow=c(2,3))
# 
# for(i in 3:8){
#   aux1 <- data%>%filter(AniId==i)
#   for(j in 1:max(aux1$Lac)){
#     aux2 <- aux1%>%filter(Lac==j)
#     if (length(aux2$TMY)>=1){
#     plot(aux2$TMY~aux2$DIM, type="l", col=aux2$AniId[1], xlab='DIM',ylab='TMY',
#          main=paste('Cow',aux2$AniId[1],' Lactation', aux2$Lac[1]))}
# }}


# #Treatments descriptives----
# aux1<-treatment%>%filter(DiagnosesName=='Mastitis')
# merged_aux<-merge(milking_ids,aux1,by.x ="IDS", by.y="Animal"); df_both_mastitis<-data.frame(merged_aux)#89
# 
# diag_summary1<-treatment%>%filter(!is.na(DiagnosesName))%>%
#   group_by(DiagnosesName)%>%summarise(n=n())%>%arrange(desc(n))
# 
# ggplot(data=subset(diag_summary1,n %in% n[1:5]), aes(x=reorder(factor(DiagnosesName), -n),y=n)) + geom_bar(stat="identity", fill='cadetblue') +
#   ggtitle('Diagnoses') + labs(x = "Diagnoses",y='count of cases') + 
#   theme(axis.text.x = element_text(size=8, angle=30, vjust = 0.6))
# 
# diag_summary2<-treatment%>%mutate(TreatmentName = fct_recode(TreatmentName, "Unknown" = ""))%>%filter(!is.na(DiagnosesName))%>%
#   group_by(DiagnosesName,TreatmentName)%>%summarise(n=n())%>%
#   filter(DiagnosesName=='Mastitis')%>%arrange(desc(n))
# 
# ggplot(data=subset(diag_summary2, n %in% n[1:4]), aes(x=reorder(factor(TreatmentName), -n),y=n)) + geom_bar(stat="identity", fill='cadetblue') +
#   ggtitle('Mastitis Treatments')+ labs(x = "Treatment",y='count of treatments') +
#   theme(axis.text.x = element_text(size=8, angle=30, vjust = 0.6))
# 
# Treat_Ani<-treatment%>%filter(DiagnosesName=='Mastitis')%>%group_by(Animal)%>%summarise(count=n())
# length(Treat_Ani$Animal) #155 Animals were treated (DISTINCT)
# ggplot(Treat_Ani,aes(x=count))+geom_bar(stat='count',fill='cadetblue')+
#   ggtitle('Diagnosis recurrence')+labs(x = "# Diagnoses",y='# Animals')

