#' @export
#'
fcast_grey <- function(data, h=3){
  fit <- GM(data)
  a <- fit$a
  b <- fit$b
  t1<-length(data)
  hh1<-vector("numeric")
  for (i in (t1+1):(t1+h)) {
    hh1[[i]]=(data[1]+(b/(-a)))*(1-exp(a))* exp((-a)*(i-1))}
  fcast=hh1[(t1+1):(t1+h)]
  return(Forecasted_value=fcast)
}
