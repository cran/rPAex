# Generates location coordinates of Q in select unit
EUsPoint<-function(Rbook,unit){
  n=2*Rbook$parameters$nx;m=2*Rbook$parameters$ny
  rp<-Rbook$coordinates.unit
  #----------
  b=2*unit+ 2*n*((unit-0.01)%/%n)
  a=b-1
  v=b+2*n
  w=v-1
  vw=c(a,b,v,w)
  for(i in 1:4){
    Q[i,]<-rp[vw[i],]
  }
  return(Q)
}

