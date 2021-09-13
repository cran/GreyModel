#' @export
#'
GM_test<-function(data){
  kk=length(data)-1
  a1<-vector("numeric")
  for (i in 1:kk) {
    a1[[i]]=(data[i]/data[i+1])}
  yf<-vector("numeric")
  for (i in 1:kk){
    if(a1[i] > 7.389){
      yf[[i]]=1
    } else {
      yf[[i]]=0
    }}
  yf1<-vector("numeric")
  for (i in 1:kk){
    if(a1[i] < 0.1345){
      yf1[[i]]=1
    } else {
      yf1[[i]]=0
    }}
  yf2=yf+yf1
  yf3=sum(yf2)
  if (yf3>0){
    Test_Result<- (" Data is not suitable for Grey modelling")
  } else {
    Test_Result<- ("Data is suitable for Grey modelling")
  }
  return(Test_Result)
  }
