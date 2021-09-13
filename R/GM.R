#' @export
#'
GM <- function(data){
  x <- cumsum(data)
  t <-length(data)-1
  k <-vector("numeric")
  y <-0
  for ( i in 1:t) {
    y[i] <- (x[i] + x[i+1])/2
    k[i] <-y[i]
  }
  k1 <-  -1*k
  M1 <- matrix(k1)
  z <-cbind(M1, c(1))
  v1=data[2:(t+1)]
  M2 <-matrix(c(v1) ,nrow =t , ncol=1)
  M3<-(t(z))%*%z
  M4 <-(t(z))%*%M2
  M5 <-solve(M3)%*%M4
  a=M5[1,1] # parameter a
  b=M5[2,1]  # parameter b
  t1<-length(data)
  fited<-vector("numeric")
  for (i in 2:t1) {
    fited[[i]]=(v1[1]+(b/(-a)))*(1-exp(a))* exp((-a)*(i-1))
  }
  hhf <- fited[2:t1]
  MAE_Grey=mean(abs(v1 - hhf))
  MAPE_Grey=(mean(abs(v1 - hhf)/v1))*100
  MSE_Grey=(mean((v1 - hhf)^2))
  RMSE_Grey=sqrt(mean((v1 - hhf)^2))
  return(list(a=a, b=b, MAE_Grey=MAE_Grey, MAPE_Grey=MAPE_Grey, MSE_Grey=MSE_Grey, RMSE_Grey=RMSE_Grey, fitted=fited))
}
