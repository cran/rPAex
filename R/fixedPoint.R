fixedPoint <-
function(start,end,segments,length) {
L=sqrt((start[1]-end[1])^2+(start[2]-end[2])^2)
n<-2*segments
X<-matrix(0,nrow=n,ncol=2)
X[1,]<-start
X[n,]<-end
if(segments > 1){
e = (L - segments*length)/(segments-1)
di<-end-start
d <-sqrt(sum(di^2))
delta<-(length+e)*di/d
for(i in seq(3,n,2)){
  X[i,]<-X[i-2,]+delta
}
for(i in seq(n-2,2,-2)){
  X[i,]<-X[i+2,]-delta
}
}
return(X)
}
