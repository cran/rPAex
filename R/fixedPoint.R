fixedPoint <-
function(start,end,npoints,long) # definir los parametros con nombres propios
{
if(end[1]<start[1]){
temp<- end
end<-start
start<-temp
}
if(start[1]==end[1]) start[1]<- start[1]+ 1e-6
b<-(start[2]-end[2])/(start[1]-end[1])
a<- start[2]-b*start[1]
d<-sqrt((start[1]-end[1])^2+(start[2]-end[2])^2)
dx<-abs(start[1]-end[1])
Lx<-long*dx/d
distancia<-dx-Lx
r<-seq(0,distancia,length=npoints)
x0<-r+start[1]
x1<-Lx+x0
x<-c(x0,x1)
x<-sort(x)
y<-a+b*x
xy<-cbind(x,y)
return(xy)
}
