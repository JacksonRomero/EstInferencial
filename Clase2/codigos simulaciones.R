#semana 2 Distribución de la media muestral

pob<-c(3,4,7,9,12)
a<-sapply(1:1e6,function(x){mean(sample(pob,2,T))})
mean(a)
var(a)

a1<-replicate(1e6,mean(sample(pob,2,T)))
mean(a1)
var(a1)

b<-replicate(1e6,mean(sample(pob,2,F)))
mean(b)
var(b)
round(mean(b),0)
round(var(b),2)
############################

##poblacion
poba<-c(0,0,0,0,1,1,1,1,2,2,3,4)
length(poba)#length: longitud de un vector
mean(poba)
var(poba)*11/12

###muestra
n=49
#P( 1<=xbar<=2)
e2<-replicate(1e6,mean(sample(poba,n,T)))
#mean: por media muestral
mean(e2)
var(e2)
1.52/49

e3<-replicate(1e6,mean(sample(0:4,n,T,prob=c(4/12,4/12,2/12,1/12,1/12))))
mean(e3)
var(e3)
1.52/49

x:  0    1    2    3     4
fx 4/12 4/12 2/12  1/12  1/12

mu=1.25
xvar=1.52/49
#mu+3*(xvar)^.5 para determinar límites de la gráfica
curve(dnorm(x,1.25,(xvar)^0.5),0.70,1.78,col='red', lwd=2,
      xlab='', ylab='', axes=F)
axis(1, lwd=2)# eje x
abline(v=1.25,col='red',lty=2)
x11<-seq(1,2,l=100)
polygon(c(1,x11,2),c(0,dnorm(x11,1.25,(xvar)^0.5),0)
        ,density = 20,angle = 45,col='blue')

## forma clásica
xmu<-rnorm(1e6,mu,sqrt(xvar))
xmu
 sum(xmu>=1 & xmu<=2)/1e6

 ## Simulación de montecarlo
 

 1.25+4*sqrt(xvar)
 1.25-4*sqrt(xvar)
  curve(dnorm(x,1.25,(xvar)^0.5),0.5454,1.9545,col='red', lwd=2,
       xlab='', ylab='', axes=F)
 axis(1, lwd=2)# eje x
 abline(v=1.25,col='red',lty=2)

 rect(1,0,1.25,dnorm(1.25,1.25,(xvar)^0.5))
 
##parte 
 

 
 mu=1.25
 xvar=1.52/49
  curve(dnorm(x,1.25,(xvar)^0.5),0.5454,1.9545,col='red', lwd=2,
       xlab='', ylab='', axes=F)
 axis(1, lwd=2)# eje x
 abline(v=1.25,col='red',lty=2)
 
 rect(1,0,2,dnorm(1.25,1.25,(xvar)^0.5))
 
 x22<-runif(1e6,1,2)
y22<-runif(1e6,0, dnorm(1.25,1.25,(xvar)^0.5))
 
 ca1<-ifelse(y22<=dnorm(x22,1.25,(xvar)^0.5),'red','black')
 points(x22,y22,col=ca1,pch=c(19,19)) 
 (table(ca1)[2]/1e6)*(2-1)*dnorm(1.25,1.25,(xvar)^0.5)
 ## Usando la normal estandar
 mu=1.25
 xvar=1.52/49
 
 (1-mu)/sqrt(xvar)
 
 
 (2-mu)/sqrt(xvar)
 
 
par(mfrow=c(2,1)) 

curve(dnorm(x,1.25,(xvar)^0.5),0.5454,1.9545,col='red', lwd=2,
      xlab='', ylab='', axes=F)
axis(1, lwd=2)# eje x
abline(v=1.25,col='red',lty=2) 

curve(dnorm(x),-4,4,col='red', lwd=2,
      xlab='', ylab='', axes=F)
axis(1, lwd=2)# eje x
abline(v=0,col='red',lty=2) 


par(mfrow=c(1,1)) 
#P( -1.42<z<4.26)=P(Z<4.26)-P(Z<-1.42)
pnorm(4.26)-pnorm(-1.42) #normal estándar
mu=1.25
xvar=1.52/49
pnorm(2,mu,sqrt(xvar))-pnorm(1,mu,sqrt(xvar)) #normal general


 