imageField <-
function(r, Q,ny,nx,dy,dx,start=1,plotting=TRUE ,...){
    n<-dim(r)[3]
    X<-r
    EU<-1:(ny*nx)
    xy1<-fixedPoint(start=Q[1,1:2],end=Q[4,1:2],segments=ny,length=dy)
    wz1<-fixedPoint(start=Q[2,1:2],end=Q[3,1:2],segments=ny,length=dy)
    M1 <-fixedPoint(start=xy1[1,1:2],end=wz1[1,1:2],segments=nx,length=dx)
    beta<-(Q[1,2]-Q[4,2])/(Q[1,1]-Q[4,1])
    for(i in 2:(2*ny)){
      N1<-fixedPoint(start=xy1[i,1:2],end=wz1[i,1:2],segments=nx,length=dx)
      M1<-rbind(M1,N1)
    }
    Qin<-M1
    k<-0
    a<-c(1,2,2*(nx+1),2*nx+1)
    for(j in 1:ny){ 
      for(i in seq(0,2*(nx-1),2)){
        k<-k+1
        area<-rbind(Qin[a+i,])
        pol <- terra::vect(area,"polygons")
        Y<-terra::extract(X, pol,xy=TRUE)
        EU<-k
        v<-Y[,2:(n+1)]
        P<-cbind(EU,x=Y$x,y=Y$y,v)
        if(k==1)Qbase<-P
        else Qbase<-rbind(Qbase,P)
        if(plotting)polygon(area,...)
      }
      a<-a+4*nx
    }
    if(beta>0){
      s<-Qbase[,1]
      w<-nx*((s-1)%/%nx+1)-(s-nx*( (s-1)%/%nx))+1
      nplot<-nx*ny
#     w<-1+abs(v-nplot)
      Qbase[,1]<-w
    }
    colnames(Qbase)[4:(n+3)]<-c(paste("L",1:n,sep=""))
    Qbase[,1] <-Qbase[,1]+start-1
    Qbase<-data.frame(Qbase)
    parameters <- list(Q=Q, ny=ny,nx=nx,dy=dy,dx=dx)
    outPlot <- list(parameters = parameters, Qbase = Qbase, coordinates.EU=Qin)
    return(outPlot)
  }

