auto_reg<-0
integr<-0
mov_avg<-0
r_RMSE<-0
r_MAPE<-0
disp_arima<-cbind(auto_reg,integr,mov_avg,r_RMSE,r_MAPE)
disp_arima<-as.data.frame(disp_arima)
library(forecast)
for(i in 1:3){
  for(j in 1:2){
    for(k in 1:3){
      r_arima<-arima(train[,4],order=c(i,(j-1),k))
      p_arima<-predict(r_arima,n.ahead=nrow(test))
      col1<-i
      col2<-j-1
      col3<-k
      col4<-accuracy(p_arima$pred,test[,4])[2]
      col5<-accuracy(p_arima$pred,test[,4])[5]
      arima_new<-cbind(col1,col2,col3,col4,col5)
      colnames(arima_new)<-c("auto_reg","integr","mov_avg","r_RMSE","r_MAPE")
      arima_new<-as.data.frame(arima_new)
      disp_arima<-rbind(disp_arima,arima_new)
    }
  }
}
disp_arima<-disp_arima[-1,]
View(disp_arima)