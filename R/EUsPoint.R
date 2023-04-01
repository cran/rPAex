EUsPoint <-
function(Rbook,EU){
  Qin<-Rbook$coordinates.EU
  nx = Rbook$parameters$nx
  ny = Rbook$parameters$ny
  a<-c(1,2,2*(nx+1),2*nx+1)
  b<-NULL
for(j in 1:ny){ 
  for(i in seq(0,2*(nx-1),2)){
  b <-rbind(b,a+i)
  }
  a<-a+4*nx 
}
  q<-b[EU,]
  Q<-Qin[q,]
  return(Q)
}
