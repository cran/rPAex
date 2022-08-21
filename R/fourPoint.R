fourPoint <-function(P) {
x<-P$x;y<-P$y
if (length(P[[1]])==3){
# Margen de error menor de 10^(-6)
if(x[2]==x[1]) x[1]<-x[1]+1.e-6 
if(x[3]==x[2]) x[3]<-x[3]+1.e-6
m1<-(y[2]-y[1])/(x[2]-x[1])
m2<-(y[3]-y[2])/(x[3]-x[2])
x[4]<-(y[3]-y[1]-m1*x[3]+m2*x[1])/(m2-m1)
y[4]<-y[3]-m1*(x[3]-x[4])
}
Q<-cbind(x=x,y=y)
return(Q)
}

