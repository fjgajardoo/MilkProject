#ARIMA modelling----
library(forecast)

###NEW DATA FOR ARIMA---

df<-subset(dataQTR.train, select=c(AniId,Lac,Quarter,grpQTR,DIM,DateMilking,Residuals_adriaens3))

df_daily<-df%>%group_by(AniId,Lac,Quarter,DateMilking) %>%
  summarise(Daily_residual = sum(Residuals_adriaens3)) %>%
  mutate(grpQTR=paste(AniId,Lac,Quarter))

length(unique(df_daily$grpQTR))

# data_s <- split(df_daily, df_daily$grpQTR)
# 
# mod <- lapply(data_s, function(x) auto.arima(x$Residuals_adriaens))


data_list<-list()
total_ts<-data.frame()
for (id in unique(df$grpQTR)){
  aux<-df_daily%>%filter(grpQTR==id)%>%
    pull(Daily_residual)%>%
    ts(., frequency = 305)
  data_list[[length(data_list)+1]]<-aux
} 

arimaFunc <- function(x){
  auto.arima(x[,'revenue'], xreg=x[,'orders'])
}

train_arimas <- lapply(data_list, auto.arima) 

train_arimas[[1]]$coef


