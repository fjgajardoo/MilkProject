##APPLYING LACTATION MODELS ----
library(sjstats)
library(Metrics)

#SAMPLING---- 
#80% Training

milking_ids<-data.frame(unique(data$grp)); colnames(milking_ids)<-c("IDS")#505

# set.seed(777)
# smp_size <- floor(0.80 * nrow(milking_ids))
# sample <-sample.int(n = nrow(milking_ids), size = smp_size, replace = F)
# 
# train_ind <- milking_ids[sample,]
# test_ind <- milking_ids[-sample,]
# 
# data.sample<-subset(data,grp %in% train_ind)%>%filter(TMY>1.0*1000,(MI/60^2)<24, DIM<=305)
# dataQTR.sample<-subset(dataQTR,grp %in% train_ind)%>%filter(QMY>0.25*1000 | TMY>1.0*1000, (MI/60^2)<24, DIM<=305)%>%
#   mutate(MI_hr=MI/60^2, logDIM=ifelse(log(DIM)<0,0,log(DIM)))
# 
# data.test<-subset(data,grp %in% test_ind)%>%filter(TMY>1.0*1000,(MI/60^2)<24, DIM<=305)
# dataQTR.test<-subset(dataQTR,grp %in% test_ind)%>%filter(QMY>0.25*1000 | TMY>1.0*1000, (MI/60^2)<24, (MI/60^2)>4, DIM<=305)%>%
#   mutate(MI_hr=MI/60^2, logDIM=ifelse(log(DIM)<0,0,log(DIM)))


# length(dataQTR.sample$AniId); length(dataQTR.test$AniId)  #Length 1123907 milkings
# length(unique(dataQTR.sample$grp)) #402 lactations
# length(unique(dataQTR.sample$AniId)) #200 cows

# ,T= as.factor(AniId),LP= seq_along(grp) #Piensa si puedes agregar Cow y Lactation en cnsecutivos
# 
# dataQTR_daily.sample.wood<-subset(dataQTR_daily,grp %in% train_ind)
# length(dataQTR_daily.sample.wood$AniId) #Length 385583 daily milkings
# length(unique(dataQTR_daily.sample.wood$grp)) #403 lactations
# length(unique(dataQTR_daily.sample.wood$AniId)) #202 cows


#DEFINING WOOD----
wood<-function(t, a, b, c){
  a*(t^b)*exp(-c*t)}

dim <-seq(0, 305)
# y<-wood(dim,1.31,0.69,0.005)
# max_line<-0.69/0.005
# qplot(dim,y,geom="line")+geom_vline(xintercept = max_line, col='red')

#STD NON LINEAR WOOD (QUARTER)----

fit.wood1 <- nls(std.DQMY~wood(DIM,a,b,c), data=dataQTR_daily, start=list(a=1, b=1, c=0.009))#Not same values than Adriaens 0.2 0.2 0.005
sum_wood1<-summary(fit.wood1)

RSS.w1 <- c(crossprod(sum_wood1$residuals))
MSE.w1 <- RSS.w1 / length(sum_wood1$residuals)
RMSE.w1 <- sqrt(MSE.w1)
MAE.w1<-mae(dataQTR_daily$std.DQMY, fitted(fit.wood1))
MPE.w1<- sum(abs(dataQTR_daily$std.DQMY - fitted(fit.wood1))/dataQTR_daily$std.DQMY)/length(sum_wood1$residuals)
#MPE is undefined due to values near from zero
AIC.w1<-AIC(fit.wood1)
BIC.w1<-BIC(fit.wood1)
fit.measures.wood1<-rbind(c(RMSE.w1,MAE.w1,MPE.w1, AIC.w1,BIC.w1)); colnames(fit.measures.wood1)<- c('RMSE','MAE','MPE','AIC','BIC')

# fitted.data0 <- data.frame(x=as.numeric(dim), y=predict(fit0.sample, list(DIM=as.numeric(dim))))

# p0<-ggplot(dataQTR_daily.sample.wood, aes(x=DIM, y=std.DQMY/1000)) + 
#   geom_line(size=0.01, alpha=0.3)
# p0+geom_line(data=fitted.data0, aes(x=x, y=y/1000),col='cadetblue')+geom_vline(xintercept = sum_fit0.sample$coefficients[2]/sum_fit0.sample$coefficients[3],col='red',size=1)+
#   ggtitle('Non Linearized Wood Model, Quarter Level - Std.Sample')+ labs(x='DIM',y='Std. Quarter DMY')

####PREDICTING AND ERASING LOWER 80% PER 4 DAYS

dataQTR_daily$std.DQMY_wood <- predict(fit.wood1, dataQTR_daily$DIM)
dataQTR_daily$filter1 <- ifelse(dataQTR_daily$std.DQMY_wood /dataQTR_daily$std.DQMY<0.8,1,0) #Lower than 80%
# dataQTR_daily$filter1.1 <- ifelse(dataQTR_daily$filter1==1,(sequence(rle(as.numeric(dataQTR_daily$filter1))$lengths)),0)#Number of consecutive days below 80%
# dataQTR_daily$filter1.2<-0

# for (i in 1:length(dataQTR_daily$AniId)){
#   if(dataQTR_daily$filter1.1[i]<4) 
#   {dataQTR_daily$filter1.2[i]=0} else
#     if(dataQTR_daily$filter1.1[i]>4){
#       dataQTR_daily$filter1.2[i]=1} else
#         if (dataQTR_daily$filter1.1[i]==4){
#           dataQTR_daily$filter1.2[i]=1
#           dataQTR_daily$filter1.2[i-1]=1
#           dataQTR_daily$filter1.2[i-2]=1
#           dataQTR_daily$filter1.2[i-3]=1
#         }
# }


dataQTR_daily.wood2<-dataQTR_daily[dataQTR_daily$filter1!=1,]
length(unique(paste(dataQTR_daily.wood2$grp,
                    dataQTR_daily.wood2$Quarter))) #1620 QL
length(unique(dataQTR_daily.wood2$AniId)) #190

length(dataQTR_daily.wood2$grp) #349180

# length(unique(dataQTR_daily.sample.wood$AniId))
# length(unique(dataQTR_daily.sample.wood$grp))
# length(unique(dataQTR_daily.sample.filtered$AniId))
# length(unique(dataQTR_daily.sample.filtered$grp))
# 
# length(dataQTR_daily.sample.wood$AniId)
# length(dataQTR_daily.sample.filtered$AniId)
# 
# length(unique(dataQTR_daily.sample.filtered$grp))
# length(unique(paste(dataQTR_daily.sample.filtered$grp, dataQTR_daily.sample.filtered$Quarter)))
# 
# a<-dataQTR_daily.sample.filtered[dataQTR_daily.sample.filtered$POS2=='Front',]; length(unique(a$grp)) *2 # Times 2 for each pair of quarters
# b<-dataQTR_daily.sample.filtered[dataQTR_daily.sample.filtered$POS2!='Front',]; length(unique(b$grp)) *2 # Times 2 for each pair of quarters
# 
# c<-dataQTR_daily.sample.filtered[dataQTR_daily.sample.filtered$PAR2=='Uniparous',]; length(unique(c$grp))*4 # Times 4 for each quarter
# d<-dataQTR_daily.sample.filtered[dataQTR_daily.sample.filtered$PAR2!='Uniparous',]; length(unique(d$grp))*4 # Times 4 for each quarter

fit.wood2 <- nls(std.DQMY~wood(DIM,a,b,c), data=dataQTR_daily.wood2, start=list(a=1, b=1, c=0.009))#Not same values than Adriaens 0.2 0.2 0.005
sum_wood2<-summary(fit.wood2)

RSS.w2 <- c(crossprod(sum_wood2$residuals))
MSE.w2 <- RSS.w2 / length(sum_wood2$residuals)
RMSE.w2 <- sqrt(MSE.w2)
MAE.w2<-mae(dataQTR_daily.wood2$std.DQMY, fitted(fit.wood2))
MPE.w2<- sum(abs(dataQTR_daily.wood2$std.DQMY - fitted(fit.wood2))/dataQTR_daily.wood2$std.DQMY)/length(sum_wood2$residuals)
AIC.w2<-AIC(fit.wood2)
BIC.w2<-BIC(fit.wood2)
fit.measures.wood2<-rbind(c(RMSE.w2,MAE.w2, MPE.w2, AIC.w2,BIC.w2)); colnames(fit.measures.wood2)<- c('RMSE','MAE','MPE','AIC','BIC')


dataQTR_daily.wood2$filter1<-0
dataQTR_daily.wood2$filter1.1<-0
dataQTR_daily.wood2$filter1.2<-0
dataQTR_daily.wood2$std.DQMY_wood <- predict(fit.wood2, dataQTR_daily.wood2$DIM)
dataQTR_daily.wood2$filter1 <- ifelse(dataQTR_daily.wood2$std.DQMY_wood /dataQTR_daily.wood2$std.DQMY<0.8,1,0) #Lower than 80%
dataQTR_daily.wood2$filter1.1 <- ifelse(dataQTR_daily.wood2$filter1==1,(sequence(rle(as.numeric(dataQTR_daily.wood2$filter1))$lengths)),0)#Number of consecutive days below 80%
dataQTR_daily.wood2$filter1.2<-0

for (i in 1:length(dataQTR_daily.wood2$AniId)){
  if(dataQTR_daily.wood2$filter1.1[i]<4) 
  {dataQTR_daily.wood2$filter1.2[i]=0} else
    if(dataQTR_daily.wood2$filter1.1[i]>4){
      dataQTR_daily.wood2$filter1.2[i]=1} else
        if (dataQTR_daily.wood2$filter1.1[i]==4){
          dataQTR_daily.wood2$filter1.2[i]=1
          dataQTR_daily.wood2$filter1.2[i-1]=1
          dataQTR_daily.wood2$filter1.2[i-2]=1
          dataQTR_daily.wood2$filter1.2[i-3]=1
        }
}

# filter.wood2<-unique(paste(dataQTR_daily.wood2[dataQTR_daily.wood2$filter1.2==1,]$grp,
#              dataQTR_daily.wood2[dataQTR_daily.wood2$filter1.2==1,]$Quarter))
# 
dataQTR_daily.wood2$grpQTR <- paste(dataQTR_daily.wood2$grp,dataQTR_daily.wood2$Quarter)
# dataQTR_daily.wood2_1<-dataQTR_daily.wood2[!(dataQTR_daily.wood2$grpQTR %in% filter.wood2),]

dataQTR_daily.wood2_1<-dataQTR_daily.wood2[dataQTR_daily.wood2$filter1.2!=1,]

length(unique(paste(dataQTR_daily.wood2_1$grp,
                    dataQTR_daily.wood2_1$Quarter))) #1615 QL

length(dataQTR_daily.wood2_1$grp) #298324 milkings
length(unique(paste(dataQTR_daily.wood2_1$grp,
                    dataQTR_daily.wood2_1$Quarter))) #1615 QL
length(unique(dataQTR_daily.wood2_1$AniId)) #190 Animals

#WOOD LINEARIZED QUARTER LEVEL----
#NEW DATA:

filter.final<-unique(paste(dataQTR_daily.wood2_1$grpQTR,dataQTR_daily.wood2_1$DateMilking))
length(filter.final)

dataQTR.final<-subset(dataQTR,(paste(grpQTR,DateMilking)) %in% filter.final)%>%filter(QMY>0.25*1000, TMY>1.0*1000, (MI_hr)<24, DIM<=305)%>% 
  mutate(MIprime= (2.5/(1+5*exp(-0.25*MI_hr)))-(2.5/(1+5*exp(-0.25)))) #Ver si lo necesito y como hacerlo

length(unique(dataQTR.final$AniId))
length(unique(dataQTR.final$grpQTR))
length(unique(dataQTR.final[dataQTR.final$POS2=='Hind',]$grpQTR))
length(unique(dataQTR.final[dataQTR.final$POS2=='Front',]$grpQTR))

length(unique(dataQTR.final[dataQTR.final$PAR2=='Uniparous',]$grpQTR))
length(unique(dataQTR.final[dataQTR.final$PAR2=='Multiparous',]$grpQTR))


# dim <-seq(0, 305, by=0.5)

# wood.lm.corrQMY<-formula(log(QMY_corrected)~1+DIM+logDIM) #BEWARE of transformation to avoid indefinitions
wood.lm.QMY<-formula(log(QMY)~1+DIM+logDIM)
woodplus.lm.QMY<-formula(log(QMY)~1+DIM+logDIM+POS2*PAR2+PAR2:DIM+POS2:logDIM+MIprime+MIprime*DIM)

fit.wood.lm.QMY<-lm(wood.lm.QMY, data = dataQTR.final)
sum_fitwood_QMY<-summary(fit.wood.lm.QMY)

RSS.wood <- c(crossprod(sum_fitwood_QMY$residuals))
MSE.wood <- RSS.wood / length(sum_fitwood_QMY$residuals)
RMSE.wood <- sqrt(MSE.wood)
AIC.wood<-AIC(fit.wood.lm.QMY)
BIC.wood<-BIC(fit.wood.lm.QMY)
MAE.wood<- sum(abs(dataQTR.final$QMY - exp(fitted(fit.wood.lm.QMY))))/length(sum_fitwood_QMY$residuals)
MPE.wood<- sum(abs(dataQTR.final$QMY - exp(fitted(fit.wood.lm.QMY)))/dataQTR.final$QMY)/length(sum_fitwood_QMY$residuals)
fit.measures.wood<-rbind(c(RMSE.wood,MAE.wood,MPE.wood,AIC.wood,BIC.wood)); colnames(fit.measures.wood)<- c('RMSE','MAE','MPE','AIC','BIC')


fit.woodplus.lm.QMY<-lm(woodplus.lm.QMY, data = dataQTR.final)
sum_fitwoodplus_QMY <- summary(fit.woodplus.lm.QMY)

RSS.woodplus <- c(crossprod(sum_fitwoodplus_QMY$residuals))
MSE.woodplus <- RSS.woodplus / length(sum_fitwoodplus_QMY$residuals)
RMSE.woodplus <- sqrt(MSE.woodplus)
AIC.woodplus<-AIC(fit.woodplus.lm.QMY)
BIC.woodplus<-BIC(fit.woodplus.lm.QMY)
MAE.woodplus<- sum(abs(dataQTR.final$QMY - exp(fitted(fit.woodplus.lm.QMY))))/length(sum_fitwoodplus_QMY$residuals)
MPE.woodplus<- sum(abs(dataQTR.final$QMY - exp(fitted(fit.woodplus.lm.QMY)))/dataQTR.final$QMY)/length(sum_fitwoodplus_QMY$residuals)

fit.measures.woodplus<-rbind(c(RMSE.woodplus,MAE.woodplus,MPE.woodplus,AIC.woodplus,BIC.woodplus)); colnames(fit.measures.woodplus)<- c('RMSE','MAE','MPE','AIC','BIC')

# fit.woodalt.lm.QMY<-lm(woodalt.lm.QMY, data = dataQTR.sample.filtered)
# summary(fit.woodalt.lm.QMY)
# 
# fit.stepplus.lm.QMY<- stepAIC(fit.woodplus.lm.QMY, direction = 'both')
# summary(fit.stepplus.lm.QMY)

#Otros intentos----
# fitQTR1.sample <- nls(QMY_corrected~wood(DIM,a,b,c), data=dataQTR.sample, start=list(a=1, b=1, c=0.009))
# sum_fitQTR1.sample<-summary(fitQTR1.sample)
# 
# fitted.dataQTR1 <- data.frame(x=dim, y=predict(fitQTR1.sample, list(DIM=dim)))
# 
# p1.QTR<-ggplot(dataQTR.sample, aes(x=DIM, y=QMY_corrected)) + 
#   geom_line(color=as.factor(dataQTR.sample$AniId),group=as.factor(dataQTR.sample$grp), size=0.01, alpha=0.3)
# p1.QTR+geom_line(data=fitted.dataQTR1, aes(x=x, y=y))+
#   geom_vline(xintercept = sum_fitQTR1.sample$coefficients[2]/sum_fitQTR1.sample$coefficients[3],col='black',size=1)+
#   ggtitle('Wood Model, Quarter Level - Sample')+ labs(x='DIM',y='Predicted Corr.MY')

# #WOOD QUARTER LEVEL
# 
# fitQTR1.sample <- nls(QMY_corrected~wood(DIM,a,b,c), data=dataQTR.sample, start=list(a=1, b=1, c=0.009))
# sum_fitQTR1.sample<-summary(fitQTR1.sample)
# 
# fitted.dataQTR1 <- data.frame(x=dim, y=predict(fitQTR1.sample, list(DIM=dim)))
# 
# p1.QTR<-ggplot(dataQTR.sample, aes(x=DIM, y=QMY_corrected)) + 
#   geom_line(color=as.factor(dataQTR.sample$AniId),group=as.factor(dataQTR.sample$grp), size=0.01, alpha=0.3)
# p1.QTR+geom_line(data=fitted.dataQTR1, aes(x=x, y=y))+
#   geom_vline(xintercept = sum_fitQTR1.sample$coefficients[2]/sum_fitQTR1.sample$coefficients[3],col='black',size=1)+
#   ggtitle('Wood Model, Quarter Level - Sample')+ labs(x='DIM',y='Predicted Corr.MY')


 # fitQTR2 <- nlsLM(DQMY~wood(DIM,a,b,c), data=dataQTR_daily, start=list(a=1, b=1, c=0.009))
# sum_fitQTR2<-summary(fitQTR2)

# fitted.dataQTR2 <- data.frame(x=dim, y=predict(fitQTR2, list(DIM=dim)))
# qplot(dim,fitted.dataQTR2$y,geom="line")+
#   geom_vline(xintercept = sum_fitQTR2$coefficients[2]/sum_fitQTR2$coefficients[3])

# dim <-unique(subset(dataQTR,grpQTR=='29 4 LF')$DIM)
# response <-subset(dataQTR,grpQTR==paste('29 4 LF'))$QMY_corrected
# data_predicted <- data.frame(x=dim, y=predict(fitQTR1.sample, list(DIM=dim)), z=response)
# data_predicted$resid<-data_predicted$z-data_predicted$y
# 
# p4.QTR<-ggplot(data_predicted, aes(x=x, y=z)) +
#   geom_line(color='cadetblue', size=0.01, alpha=0.5)+ geom_point(aes(y = z), shape = 1, alpha=0.5)+
#   geom_line(aes(x=x, y=y))+geom_vline(xintercept = sum_fitQTR1.sample$coefficients[2]/sum_fitQTR1.sample$coefficients[3],col='orangered',size=0.5, lty=2)+
#   ggtitle(paste('Wood Model, Quarter Level - 29, Lac4, LF'))+ labs(x='DIM',y='Predicted Corr.QMY')
# 
# p4.QTR
# 
# plot1.QTR<-p4.QTR+  geom_segment(aes(xend = x, yend = y), alpha = .2)
# plot1.QTR #Basically the same
# 
# #Residuals plot
# ggplot(data_predicted, aes(x=x,y=resid))+geom_point(size=0.5)+geom_segment(aes(xend = x, yend = 0), alpha = .2)+
#   ggtitle('Residuals Wood Model - 29, Lac4, LF')+ labs(x='DIM',y='Residuals')
# 
# animation1<-plot1.QTR +  transition_reveal(along = x) 
# anim_save("animationfinalQTRLF.gif", animation = animation1 )

#Recording Predictions and residuals----

# dim.df<-data.frame(DIM=dim, logDIM=log(dim))
# fitted.dataQTR.wood.corrQMY <- data.frame(x=dim, y=exp(predict(fit.wood.lm.corrQMY, newdata = dim.df)))
# p1.QTR<-ggplot(fitted.dataQTR.wood.corrQMY, aes(x=x, y=y)) + geom_line(size=0.01, alpha=0.3)
# p1.QTR
# # +geom_vline(xintercept = sum_fitQTR1.sample$coefficients[2]/sum_fitQTR1.sample$coefficients[3],col='black',size=1)+
# #   ggtitle('Wood Model, Quarter Level - Sample')+ labs(x='DIM',y='Predicted Corr.MY')
# fitted.dataQTR.wood.QMY <- data.frame(x=dim, y=exp(predict(fit.wood.lm.QMY, newdata = dim.df)))
# p2.QTR<-ggplot(fitted.dataQTR.wood.QMY, aes(x=x, y=y)) + geom_line(size=0.01, alpha=0.3)
# p2.QTR
# 
# dataQTR.sample.filtered$QMY_corrected_wood <- exp(predict(fit.wood.lm.corrQMY, newdata = dataQTR.sample.filtered, interval = "confidence"))
# dataQTR.sample.filtered$Residuals_corrected_wood <-dataQTR.sample.filtered$QMY_corrected - as.vector(dataQTR.sample.filtered$QMY_corrected_wood[,1])
# dataQTR.sample.filtered$Residuals_corrected_wood_lower <-dataQTR.sample.filtered$QMY_corrected - as.vector(dataQTR.sample.filtered$QMY_corrected_wood[,3])
# dataQTR.sample.filtered$Residuals_corrected_wood_upper <-dataQTR.sample.filtered$QMY_corrected - as.vector(dataQTR.sample.filtered$QMY_corrected_wood[,2])
# 
# plot(dataQTR.sample.filtered$Residuals_corrected_wood/1000, type = 'l')
# lines(dataQTR.sample.filtered$Residuals_corrected_wood_lower/1000, col='blue')
# lines(dataQTR.sample.filtered$Residuals_corrected_wood_upper/1000, col='red')
# describe(dataQTR.sample.filtered$Residuals_corrected_wood/1000)

dataQTR.final$QMY_wood <- exp(predict(fit.wood.lm.QMY, newdata = dataQTR.final, interval = "confidence"))
dataQTR.final$Residuals_wood <-dataQTR.final$QMY - as.vector(dataQTR.final$QMY_wood[,1])
dataQTR.final$Residuals_wood_lower <-dataQTR.final$QMY - as.vector(dataQTR.final$QMY_wood[,3])
dataQTR.final$Residuals_wood_upper <-dataQTR.final$QMY - as.vector(dataQTR.final$QMY_wood[,2])

describe(dataQTR.final$Residuals_wood/1000)
plot(dataQTR.final$Residuals_wood/1000, type = 'l')

dataQTR.final$QMY_woodplus <- exp(predict(fit.woodplus.lm.QMY, newdata = dataQTR.final, interval = "confidence"))
dataQTR.final$Residuals_woodplus <-dataQTR.final$QMY - as.vector(dataQTR.final$QMY_woodplus[,1])
dataQTR.final$Residuals_woodplus_lower <-dataQTR.final$QMY - as.vector(dataQTR.final$QMY_woodplus[,3])
dataQTR.final$Residuals_woodplus_upper <-dataQTR.final$QMY - as.vector(dataQTR.final$QMY_woodplus[,2])

describe(dataQTR.final$Residuals_woodplus/1000)
plot(dataQTR.final$Residuals_woodplus/1000, type = 'l')

#LMM from Adriaens

# describe(log(dataQTR.sample.filtered$QMY_corrected))
# describe(log(dataQTR.sample.filtered$QMY))

# library(car)
# library(MASS)
# 
# qqPlot(log(dataQTR.sample.filtered$QMY), "norm")
# qqPlot(dataQTR.sample.filtered$QMY, "norm")
# qqPlot(log(dataQTR.sample.filtered$QMY), "lnorm")
# qqPlot(dataQTR.sample.filtered$QMY, "lnorm")
# 
# nbinom <- fitdistr(dataQTR.sample.filtered$QMY, "Negative Binomial")
# qqp(dataQTR.sample.filtered$QMY, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
# 
# gamma <- fitdistr(dataQTR.sample.filtered$QMY, "gamma")
# qqp(log(dataQTR.sample.filtered$QMY), "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

# sampleo_prueba<-sample(1:5,length(dataQTR.sample.filtered$AniId), replace=T)
dataQTR.final$AniId_F <- factor(dataQTR.final$AniId)
dataQTR.final$grpQTR_F <- factor(dataQTR.final$grpQTR)


# adriaens.lmm.QMY1<-formula(log(QMY)~
#                   1 + DIM+log(DIM)+log(MI_hr)+(1|AniId_F))
                  # +DIM+log(DIM)+log(MI_hr)+
                  # as.factor(PAR2)*as.factor(POS2)+DIM:as.factor(PAR2)+
                  #   log(DIM):as.factor(POS2)+log(MI_hr):DIM+
                  #   (1+DIM+log(DIM)|AniId_F))

adriaens.lmm.QMY2<-formula(log(QMY)~
                             1+DIM+log(DIM)+factor(POS2)*factor(PAR2)+factor(POS2):log(DIM)+MIprime+
                             MIprime*DIM +(1|grpQTR_F))


# IDS_test <- dataQTR.sample.filtered%>% group_by(grpQTR) %>% summarise(Frequency = n())
# selection<- IDS_test$grpQTR[which(IDS_test$Frequency>100)]
# data.test<-dataQTR.sample.filtered%>%filter(grpQTR %in% selection)
# update(fit.adriaens.lmm.QMY3,data=data.test)

adriaens.lmm.QMY3<-formula(log(QMY)~
                             1+DIM+log(DIM)+factor(POS2)*factor(PAR2)+factor(POS2):log(DIM)+MIprime+
                             MIprime*DIM +
                             (1+DIM+log(DIM)|grpQTR_F))

# wood.linear.UDR<-formula(log(data.sample$TMY)~
#                       1+data.sample$DIM+log(data.sample$DIM)+log(data.sample$MI_hr)+
#                       data.sample$Lac+data.sample$DIM:data.sample$Lac+
#                       log(data.sample$MI_hr):data.sample$DIM)

# fit.adriaens.lmm.QMY1 <- lmer(adriaens.lmm.QMY1, data = dataQTR.sample.filtered, 
#                                  REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead", optCtrl = list(maxfun=20000)))

fit.adriaens.lmm.QMY2 <- lmer(adriaens.lmm.QMY2, data = dataQTR.final, 
                             REML = FALSE, control = lmerControl(optimizer ="bobyqa"))

# library(nlme)
sum_fitadriaens_QMY2 <- summary(fit.adriaens.lmm.QMY2)
RSS.adr2 <- c(crossprod(residuals(fit.adriaens.lmm.QMY2)))
MSE.adr2 <- RSS.adr2 / length(residuals(fit.adriaens.lmm.QMY2))
RMSE.adr2 <- sqrt(MSE.adr2)
MAE.adr2<- sum(abs(dataQTR.final$QMY-exp(predict(fit.adriaens.lmm.QMY2)))/length(residuals(fit.adriaens.lmm.QMY2)))
MPE.adr2<-  sum(abs(dataQTR.final$QMY-exp(predict(fit.adriaens.lmm.QMY2)))/dataQTR.final$QMY)/length(residuals(fit.adriaens.lmm.QMY2))
AIC.adr2<- AIC(fit.adriaens.lmm.QMY2)
BIC.adr2<- BIC(fit.adriaens.lmm.QMY2)

fit.measures.QMY2<-rbind(c(RMSE.adr2,MAE.adr2,MPE.adr2, AIC.adr2, BIC.adr2)); colnames(fit.measures.QMY2)<- c('RMSE','MAE','MPE','AIC','BIC')


fit.adriaens.lmm.QMY3 <- lmer(adriaens.lmm.QMY3, data = dataQTR.final, 
                              REML = FALSE, control = lmerControl(optimizer ="bobyqa", calc.derivs=F, optCtrl = list(maxfun=2e5)))

sum_fitadriaens_QMY3<- summary(fit.adriaens.lmm.QMY3)
RSS.adr3 <- c(crossprod(residuals(fit.adriaens.lmm.QMY3)))
MSE.adr3 <- RSS.adr3 / length(residuals(fit.adriaens.lmm.QMY3))
RMSE.adr3 <- sqrt(MSE.adr3)
MAE.adr3<- sum(abs(dataQTR.final$QMY-exp(fitted(fit.adriaens.lmm.QMY3)))/length(residuals(fit.adriaens.lmm.QMY3)))
MPE.adr3<- sum(abs(dataQTR.final$QMY-exp(fitted(fit.adriaens.lmm.QMY3)))/dataQTR.final$QMY)/length(residuals(fit.adriaens.lmm.QMY3))
AIC.adr3<-AIC(fit.adriaens.lmm.QMY3)
BIC.adr3<- BIC(fit.adriaens.lmm.QMY3)

fit.measures.QMY3<-rbind(c(RMSE.adr3,MAE.adr3,MPE.adr3, AIC.adr3, BIC.adr3)); colnames(fit.measures.QMY3)<- c('RMSE','MAE','MPE','AIC','BIC')


# length(getME(fit.adriaens.lmm.QMY3,"theta"))
# length(fixef(fit.adriaens.lmm.QMY3))
# 
numcols <- grep("^c\\.",names(data.test))
dfs <- data.test
dfs[,numcols] <- scale(dfs[,numcols])
m1_sc <- update(fit.adriaens.lmm.QMY3,data=dfs)
summary(m1_sc)

fit.adriaens.lmm.QMY4 <- lmer(adriaens.lmm.QMY3, data = dataQTR.sample.filtered, 
                              REML = TRUE, control = lmerControl(optimizer ="bobyqa", optCtrl = list(maxfun=2e5)))

update(fit.adriaens.lmm.QMY4,data=data.test)

update(fit.adriaens.lmm.QMY4,data=dfs)

# AF<-allFit(fit.adriaens.lmm.QMY3, verbose=F)
# AF_lliks<- sort(sapply(AF,logLik))



summary(fit.adriaens.lmm.QMY3)
View(coef(fit.adriaens.lmm.QMY2))
str(fit.adriaens.lmm.QMY2)


fitted<-fitted(fit.adriaens.lmm.QMY2)


dataQTR.sample.filtered$QMY_adriaens <- exp(as.vector(fitted))
dataQTR.sample.filtered$Residuals_adriaens <-dataQTR.sample.filtered$QMY - dataQTR.sample.filtered$QMY_adriaens
plot(dataQTR.sample.filtered$Residuals_wood/1000, type = 'l')

# install.packages('cAIC4')
# library(cAIC4)
# model_step <- stepcAIC(fit.adriaens.lmm.QMY2, direction = "backward", 
#                        trace = TRUE, data = dataQTR.sample.filtered)
# 
# AIC(fm1, fm2, fm3)
# anova(fm2, fm3)


# library(optimx)
# m1 <- lmer(adriaens.lmm.corrQMY, data = dataQTR.sample.filtered, 
#           REML = FALSE,
#           control = lmerControl(
#             optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
# 
# m2 <- lmer(adriaens.lmm.corrQMY, data = dataQTR.sample.filtered, 
#           REML = FALSE, 
#           control = lmerControl(
#             optimizer ='optimx', optCtrl=list(method='nlminb')))


