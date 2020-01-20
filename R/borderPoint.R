borderPoint <-
function(Q){
Q<-as.data.frame(Q)
  m1<-lm(y~x,data=Q[1:2,])
  m2<-lm(y~x,data=Q[2:3,])
  m3<-lm(y~x,data=Q[3:4,])
  m4<-lm(y~x,data=Q[c(1,4),])
  models<-list(m1=m1,m2=m2,m3=m3,m4=m4)
  return(models)
}
