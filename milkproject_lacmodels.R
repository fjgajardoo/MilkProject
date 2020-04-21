##APPLYING LACTATION MODELS ----
library(sjstats)
library(performance)
library(Metrics)


#DEFINING WOOD----
wood<-function(t, a, b, c){
  a*(t^b)*exp(-c*t)}

#WOOD NON LINEAR MODEL for QL selection (QUARTER)---- 
#First filtering model(Wood1)

fit.wood1 <- nls(std.DQMY~wood(DIM,a,b,c), data=dataQTR_daily, start=list(a=0.2, b=0.2, c=0.005))#Same values than Adriaens 0.2 0.2 0.005
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

####PREDICTING AND ERASING LOWER THAN 80% DATAPOINTS

dataQTR_daily.wood2<-dataQTR_daily[dataQTR_daily$std.DQMY/predict(fit.wood1, dataQTR_daily$DIM)>=0.8,]
length(unique(paste(dataQTR_daily.wood2$grp,
                    dataQTR_daily.wood2$Quarter))) #1506 QL
length(unique(dataQTR_daily.wood2$AniId)) #185
length(dataQTR_daily.wood2$grp) #231564

#Second filtering model (Wood2)
fit.wood2 <- nls(std.DQMY~wood(DIM,a,b,c), data=dataQTR_daily.wood2, 
                 start=list(a=sum_wood1$coefficients[1], b=sum_wood1$coefficients[2], c=sum_wood1$coefficients[3]))#Not same values than Adriaens 0.2 0.2 0.005
sum_wood2<-summary(fit.wood2)

RSS.w2 <- c(crossprod(sum_wood2$residuals))
MSE.w2 <- RSS.w2 / length(sum_wood2$residuals)
RMSE.w2 <- sqrt(MSE.w2)
MAE.w2<-mae(dataQTR_daily.wood2$std.DQMY, fitted(fit.wood2))
MPE.w2<- sum(abs(dataQTR_daily.wood2$std.DQMY - fitted(fit.wood2))/dataQTR_daily.wood2$std.DQMY)/length(sum_wood2$residuals)
AIC.w2<-AIC(fit.wood2)
BIC.w2<-BIC(fit.wood2)
fit.measures.wood2<-rbind(c(RMSE.w2,MAE.w2, MPE.w2, AIC.w2,BIC.w2)); colnames(fit.measures.wood2)<- c('RMSE','MAE','MPE','AIC','BIC')

dataQTR_daily.wood2$filter1 <- ifelse(dataQTR_daily.wood2$std.DQMY/predict(fit.wood2, dataQTR_daily.wood2$DIM)<0.8,1,0) #Identifier for obs. lower than 80% of prediction
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

dataQTR_daily.wood2$grpQTR <- paste(dataQTR_daily.wood2$grp,dataQTR_daily.wood2$Quarter) #Add Quarter Lactation ID
dataQTR_daily.wood2_1<-dataQTR_daily.wood2[dataQTR_daily.wood2$filter1.2!=1,]

#FINAL MODELLING (after selection)----

#NEW DATA: Filtering Quarter level data for selected QL
filter.final<-unique(paste(dataQTR_daily.wood2_1$grpQTR,dataQTR_daily.wood2_1$DateMilking))
dataQTR.final<-subset(dataQTR,(paste(grpQTR,DateMilking)) %in% filter.final)%>%
  filter(QMY>0.25*1000, TMY>1.0*1000, (MI_hr)<24, DIM<=305)%>% 
  mutate(MIprime= (2.5/(1+5*exp(-0.25*MI_hr)))-(2.5/(1+5*exp(-0.25)))) #Creation of MI prime

length(dataQTR.final$AniId) #432101 observations/quarter milkings
length(unique(dataQTR.final$AniId)) #185 animals
length(unique(dataQTR.final$grpQTR)) #1467 QL
length(unique(dataQTR.final[dataQTR.final$POS2=='Hind',]$grpQTR)) #728 Hind QL
length(unique(dataQTR.final[dataQTR.final$POS2=='Front',]$grpQTR))#739 Front QL
length(unique(dataQTR.final[dataQTR.final$PAR2=='Uniparous',]$grpQTR))#536 Uniparous QL
length(unique(dataQTR.final[dataQTR.final$PAR2=='Multiparous',]$grpQTR)) #943 Uniparous QL

#Creating Training set and Testing Set
milking_ids<-data.frame(unique(dataQTR.final$grpQTR)); colnames(milking_ids)<-c("IDS")#1588 QL, sample from QL...
set.seed(777)
smp_size <- floor(0.75 * nrow(milking_ids)) #75-25 for Train-Test division
sample <-sample.int(n = nrow(milking_ids), size = smp_size, replace = F) #Sample of QL's
train_ind <- milking_ids[sample,]
test_ind <- milking_ids[-sample,]

#Training set
dataQTR.train<-subset(dataQTR.final,grpQTR %in% train_ind)%>%filter(QMY>0.25*1000 | TMY>1.0*1000, (MI/60^2)<24, DIM<=305)%>%
  mutate(MI_hr=MI/60^2, logDIM=ifelse(log(DIM)<0,0,log(DIM)))

#Testing set
dataQTR.test<-subset(dataQTR.final,grpQTR %in% test_ind)%>%filter(QMY>0.25*1000 | TMY>1.0*1000, (MI/60^2)<24, (MI/60^2)>4, DIM<=305)%>%
  mutate(MI_hr=MI/60^2, logDIM=ifelse(log(DIM)<0,0,log(DIM)))

#Comparing lengths of both sets
length(dataQTR.train$AniId); length(dataQTR.test$AniId)  #Length 324244 and 107227 milkings
length(unique(dataQTR.train$grpQTR));length(unique(dataQTR.test$grpQTR)) #1191 and 397 QL
length(unique(dataQTR.train$AniId)); length(unique(dataQTR.test$AniId)) #189 and 156 cows


#WOOD LINEARIZED MODEL (QUARTER LEVEL)----
#Two wood models proposed: Linear wood and Linear wood with extra covariates (from Afriaens)

#First Wood model
wood.lm.QMY<-formula(log(QMY)~1+DIM+logDIM)

fit.wood.lm.QMY<-lm(wood.lm.QMY, data = dataQTR.train)
sum_fitwood_QMY<-summary(fit.wood.lm.QMY)

RSS.wood <- c(crossprod(sum_fitwood_QMY$residuals))
MSE.wood <- RSS.wood / length(sum_fitwood_QMY$residuals)
RMSE.wood <- sqrt(MSE.wood)
AIC.wood<-AIC(fit.wood.lm.QMY)
BIC.wood<-BIC(fit.wood.lm.QMY)
MAE.wood<- sum(abs(dataQTR.train$QMY - exp(fitted(fit.wood.lm.QMY))))/length(sum_fitwood_QMY$residuals)
MPE.wood<- sum(abs(dataQTR.train$QMY - exp(fitted(fit.wood.lm.QMY)))/dataQTR.train$QMY)/length(sum_fitwood_QMY$residuals)
fit.measures.wood<-rbind(c(RMSE.wood,MAE.wood,MPE.wood,AIC.wood,BIC.wood)); colnames(fit.measures.wood)<- c('RMSE','MAE','MPE','AIC','BIC')

#Second Wood model
woodplus.lm.QMY<-formula(log(QMY)~1+DIM+logDIM+POS2*PAR2+PAR2:DIM+POS2:logDIM+MIprime+MIprime*DIM)

fit.woodplus.lm.QMY<-lm(woodplus.lm.QMY, data = dataQTR.train)
sum_fitwoodplus_QMY <- summary(fit.woodplus.lm.QMY)

RSS.woodplus <- c(crossprod(sum_fitwoodplus_QMY$residuals))
MSE.woodplus <- RSS.woodplus / length(sum_fitwoodplus_QMY$residuals)
RMSE.woodplus <- sqrt(MSE.woodplus)
AIC.woodplus<-AIC(fit.woodplus.lm.QMY)
BIC.woodplus<-BIC(fit.woodplus.lm.QMY)
MAE.woodplus<- sum(abs(dataQTR.train$QMY - exp(fitted(fit.woodplus.lm.QMY))))/length(sum_fitwoodplus_QMY$residuals)
MPE.woodplus<- sum(abs(dataQTR.train$QMY - exp(fitted(fit.woodplus.lm.QMY)))/dataQTR.train$QMY)/length(sum_fitwoodplus_QMY$residuals)

fit.measures.woodplus<-rbind(c(RMSE.woodplus,MAE.woodplus,MPE.woodplus,AIC.woodplus,BIC.woodplus)); colnames(fit.measures.woodplus)<- c('RMSE','MAE','MPE','AIC','BIC')

# LMM Models----
#Two LMM models proposed (from Adriaens)

dataQTR.train$AniId_F <- factor(dataQTR.train$AniId)
dataQTR.train$grpQTR_F <- factor(dataQTR.train$grpQTR)
dataQTR.test$AniId_F <- factor(dataQTR.test$AniId)
dataQTR.test$grpQTR_F <- factor(dataQTR.test$grpQTR)

#First LMM model (Only intercept on Random effects)
adriaens.lmm.QMY2<-formula(log(QMY)~
                             1+DIM+logDIM+factor(POS2)*factor(PAR2)+factor(POS2):logDIM+MIprime+
                             MIprime*DIM +(1|grpQTR_F))



fit.adriaens.lmm.QMY2 <- lmer(adriaens.lmm.QMY2, data = dataQTR.train, 
                             REML = FALSE, control = lmerControl(optimizer ="bobyqa"))

sum_fitadriaens_QMY2 <- summary(fit.adriaens.lmm.QMY2)
RSS.adr2 <- c(crossprod(residuals(fit.adriaens.lmm.QMY2)))
MSE.adr2 <- RSS.adr2 / length(residuals(fit.adriaens.lmm.QMY2))
RMSE.adr2 <- sqrt(MSE.adr2)
MAE.adr2<- sum(abs(dataQTR.train$QMY-exp(predict(fit.adriaens.lmm.QMY2)))/length(residuals(fit.adriaens.lmm.QMY2)))
MPE.adr2<-  sum(abs(dataQTR.train$QMY-exp(predict(fit.adriaens.lmm.QMY2)))/dataQTR.train$QMY)/length(residuals(fit.adriaens.lmm.QMY2))
AIC.adr2<- AIC(fit.adriaens.lmm.QMY2)
BIC.adr2<- BIC(fit.adriaens.lmm.QMY2)

fit.measures.QMY2<-rbind(c(RMSE.adr2,MAE.adr2,MPE.adr2, AIC.adr2, BIC.adr2)); colnames(fit.measures.QMY2)<- c('RMSE','MAE','MPE','AIC','BIC')


##Second LMM model (Linear Wood on Random effects)
adriaens.lmm.QMY3<-formula(log(QMY)~
                             1+DIM+logDIM+factor(POS2)*factor(PAR2)+factor(POS2):logDIM+MIprime+
                             MIprime*DIM +
                             (1+DIM+logDIM|grpQTR_F))

fit.adriaens.lmm.QMY3 <- lmer(adriaens.lmm.QMY3, data = dataQTR.train, 
                              REML = TRUE, 
                              control = lmerControl(optimizer ="bobyqa", calc.derivs=T, 
                                                    check.conv.grad = .makeCC("warning", tol = 5e-3, relTol = NULL),
                                                    optCtrl = list(maxfun=2e4)))

sum_fitadriaens_QMY3<- summary(fit.adriaens.lmm.QMY3)
RSS.adr3 <- c(crossprod(residuals(fit.adriaens.lmm.QMY3)))
MSE.adr3 <- RSS.adr3 / length(residuals(fit.adriaens.lmm.QMY3))
RMSE.adr3 <- sqrt(MSE.adr3)
MAE.adr3<- sum(abs(dataQTR.train$QMY-exp(fitted(fit.adriaens.lmm.QMY3)))/length(residuals(fit.adriaens.lmm.QMY3)))
MPE.adr3<- sum(abs(dataQTR.train$QMY-exp(fitted(fit.adriaens.lmm.QMY3)))/dataQTR.train$QMY)/length(residuals(fit.adriaens.lmm.QMY3))
AIC.adr3<-AIC(fit.adriaens.lmm.QMY3)
BIC.adr3<- BIC(fit.adriaens.lmm.QMY3)

fit.measures.QMY3<-rbind(c(RMSE.adr3,MAE.adr3,MPE.adr3, AIC.adr3, BIC.adr3)); colnames(fit.measures.QMY3)<- c('RMSE','MAE','MPE','AIC','BIC')

#Comparison of fitting measures for 4 models (QLMM is best fit)
fit.measures.wood
fit.measures.woodplus
fit.measures.QMY2
fit.measures.QMY3

fittedQMY3<-fitted(fit.adriaens.lmm.QMY3)
dataQTR.train$QMY_adriaens3 <- exp(as.vector(fittedQMY3))
dataQTR.train$Residuals_adriaens3 <-dataQTR.train$QMY - dataQTR.train$QMY_adriaens3
plot(dataQTR.train$Residuals_adriaens3/1000, type = 'l')

rsquared.adriaens3<-r2(fit.adriaens.lmm.QMY3)#R-squared of QLMM
var.adriaens3<-(describe(dataQTR.train$Residuals_adriaens3/1000)$sd)^2 #Variance od residuals


#Getting fit measures for test set
MAE.adr3.test<- sum(abs(dataQTR.test$QMY-exp(predict(fit.adriaens.lmm.QMY3, dataQTR.test, allow.new.levels=TRUE)))/length(dataQTR.test$QMY))
MPE.adr3.test<- sum(abs(dataQTR.test$QMY-exp(predict(fit.adriaens.lmm.QMY3, dataQTR.test, allow.new.levels=TRUE)))/dataQTR.test$QMY)/length(dataQTR.test$QMY)
MAE.adr3.test;MPE.adr3.test

install.packages("remotes")
library(remotes)
library(devtools)
remotes::install_github("lme4/lme4")
allFit(fit.adriaens.lmm.QMY2)


#Alternative LMM fitting with different optimizers 

# library(optimx)
# m1 <- lmer(adriaens.lmm.QMY3, data = dataQTR.train,
#           REML = TRUE,
#           control = lmerControl(
#             optimizer ='optimx', 
#             optCtrl=list(method='nlminb'),
#             check.conv.grad = .makeCC('warning',tol= 5e-3, relTol = NULL)))
# 
# 
# sum_finalmodel<- summary(m1)
# RSS.final <- c(crossprod(residuals(m1)))
# MSE.final <- RSS.final / length(residuals(m1))
# RMSE.final <- sqrt(MSE.final)
# MAE.final<- sum(abs(dataQTR.train$QMY-exp(fitted(m1)))/length(residuals(m1)))
# MPE.final<- sum(abs(dataQTR.train$QMY-exp(fitted(m1)))/dataQTR.train$QMY)/length(residuals(m1))
# AIC.final<-AIC(m1)
# BIC.final<- BIC(m1)
# 
# fit.measures.finalmodel<-rbind(c(RMSE.final,MAE.final,MPE.final, AIC.final, BIC.final)); colnames(fit.measures.finalmodel)<- c('RMSE','MAE','MPE','AIC','BIC')
# 
# MAE.final2.test<- sum(abs(dataQTR.test$QMY-exp(predict(m1, dataQTR.test, allow.new.levels=TRUE)))/length(dataQTR.test$QMY))
# MPE.final2.test<- sum(abs(dataQTR.test$QMY-exp(predict(m1, dataQTR.test, allow.new.levels=TRUE)))/dataQTR.test$QMY)/length(dataQTR.test$QMY)
 
# numcols <- grep("^c\\.",names(dataQTR.final))
# dfs <- dataQTR.final
# dfs[,numcols] <- scale(dfs[,numcols])
# updated.adriaensQMY3 <- update(fit.adriaens.lmm.QMY3,data=dfs)
# updated.m1 <- update(m1,data=dfs)

