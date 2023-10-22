borderPoint <-
function (r,Rbook,distance,plotting=TRUE,...) 
{
  parameters<-Rbook$parameters
  Q<- parameters$Q
  ny<-parameters$ny
  nx<-parameters$nx
  dy<-parameters$dy
  dx<-parameters$dx
delta<-1.0e-10
M <-Q1<-matrix(0,nrow=4,ncol=2)
# Ec 1: P1,P2
M[1,2]<-(Q[2,2]-Q[1,2])/(Q[2,1]-Q[1,1]+delta)
M[1,1]<-Q[1,2]-M[1,2]*Q[1,1]+distance
# Ec 2: P2,P3
M[2,2]<-(Q[3,2]-Q[2,2])/(Q[3,1]-Q[2,1]+delta)
M[2,1]<-Q[2,2]-M[2,2]*Q[2,1]-M[2,2]*distance
# Ec 3: P3,P4
M[3,2]<-(Q[4,2]-Q[3,2])/(Q[4,1]-Q[3,1]+delta)
M[3,1]<-Q[3,2]-M[3,2]*Q[3,1]-distance
# Ec 4: P4,P1
M[4,2]<-(Q[4,2]-Q[1,2])/(Q[4,1]-Q[1,1]+delta)
M[4,1]<-Q[4,2]-M[4,2]*Q[4,1]+M[4,2]*distance
# resolver las ecuaciones y=a + bx
#-------------------
x = (M[4,1]-M[1,1])/(M[1,2]-M[4,2])
y = M[1,1]+M[1,2]*x
Q1[1,1]<-x; Q1[1,2]<-y
#-------------------
x = (M[2,1]-M[1,1])/(M[1,2]-M[2,2])
y = M[1,1]+M[1,2]*x
Q1[2,1]<-x; Q1[2,2]=y
#-------------------
x = (M[2,1]-M[3,1])/(M[3,2]-M[2,2])
y = M[3,1]+M[3,2]*x
Q1[3,1]<-x; Q1[3,2]=y
#-------------------
x = (M[4,1]-M[3,1])/(M[3,2]-M[4,2])
y = M[3,1]+M[3,2]*x
Q1[4,1]<-x; Q1[4,2]=y
#-------------------
u <- imageField(r, Q1, 1, 1, 0, 0, plotting = FALSE)$Qbase
w<-terra::rast(u[,-1],type="xyz")
u <- imageField(w, Q1, 1, 1, 0, 0, plotting = FALSE)$Qbase
v <- imageField(w, Q, ny, nx, dy,dx, plotting = FALSE)$Qbase
s1 <- paste(u[, 2], u[, 3], sep = "-")
s2 <- paste(v[, 2], v[, 3], sep = "-")
s <- s1 %in% s2
border <- u[!s,-1 ]
w<-terra::rast(border,type="xyz")
if(plotting)terra::image(w,...)
return(list(Qborder=Q1,Border=border))
}
