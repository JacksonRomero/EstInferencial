#poblacional
set.seed(1002)#semilla
mepob<-3600
vapob<-280^2
#muestra
n<-196
## P(3580 <xbar <3620)
#Def. Teorema

##simulación clásica

B<-1e6
xmu<-rnorm(B,mepob,(vapob/n)^0.5)
xmu
sum(xmu>=3580 & xmu<=3620)/B

## solucuón gráfica
sdmu<-(vapob/n)^0.5

curve(dnorm(x,3600,sdmu),3520,3680,lwd=2,col='red', axes=F,
      xlab='',ylab='')
#3600-4*sdmu
axis(1, lwd=2)
abline(v=3600, col='red', lty=2)
x11<-seq(3580,3620,l=100)
polygon(c(3580,x11,3620), c(0,dnorm(x11,3600,sdmu),0),
        density = 20, angle = 45, col='red')

## P(3580 <xbar <3620)
sdmu<-(vapob/n)^0.5
sdmu

#par(mfrow=c(2,1))
#curve(dnorm(x),-4,4)
#rect(-1,0,1,dnorm(0))

#curve(dnorm(x,3600,sdmu),3520,3680,lwd=2,col='red', axes=F,
 #     xlab='',ylab='')
#axis(1, lwd=2)
#rect(3580,0,3620,dnorm(3600,3600,sdmu))
#par(mfrow=c(1,1))

(3620-3600)/sdmu

(3580-3600)/sdmu

#P(-1 <Z<1)=0.68
#P(-1 <Z<1)=P(Z<1)-P(Z<-1)
pnorm(1)-pnorm(-1)

### Simulación de montecarlo
sdmu<-(vapob/n)^0.5

curve(dnorm(x,3600,sdmu),3520,3680,lwd=2,col='red', axes=F,
      xlab='',ylab='')
#3600-4*sdmu
axis(1, lwd=2, col='red')
abline(v=3600, col='red', lty=2)
rect(3580,0,3620,dnorm(3600,3600,sdmu))
x21<-runif(1e6,3580,3620)
y21<-runif(1e6,0,dnorm(3600,3600,sdmu))
ca<-ifelse(y21<=dnorm(x21,3600,sdmu),'red','black')
points(x21,y21,col=ca, pch=c(19,19))
#animación
#for(k in 1:1e2){
 # ca<-ifelse(y21[1:k] <=dnorm(x21[1:k],3600,sdmu),'red','black')
  #points(x21[1:k],y21[1:k],col=ca, pch=c(19,19))
  #Sys.sleep(1)
#}

(table(ca)[2]/1e6)*(3620-3580)*dnorm(3600,3600,sdmu)


############################################
mepob1=1/3
vapob1=1/18
n1=36
#P(xbar>0.42)
#forma clásica
set.seed(1001)
mu<-rnorm(1e6,mepob1,(vapob1/n1)^0.5)
sum(mu>0.42)/1e6
##solución gráfica
curve(dnorm(x,mepob1,(vapob1/n1)^0.5),0.1762,0.4905,lwd=2,
      axes=F, xlab = '', ylab='')
#mepob1-4*(vapob1/n1)^0.5
axis(1, lwd=2)
abline(v=mepob1,lty=2, lwd=2)
xa<-seq(0.42,0.4905,l=200)
polygon(c(0.42,xa,0.4905),c(0,dnorm(xa,mepob1,(vapob1/n1)^0.5),0),
density = 20, angle = 45,col='blue')
#P(xbar>0.42)=1-P(xbar<=0.42)=1-P(Z>(0.42-mepob1)/(vapob1/n1)^0.5)

(0.42-mepob1)/(vapob1/n1)^0.5
#1-P(Z<=2.21)
1-pnorm(2.21)


##simulación de montecarlo

curve(dnorm(x,mepob1,(vapob1/n1)^0.5),0.1762,0.4905,lwd=2,
      axes=F, xlab = '', ylab='')
#mepob1-4*(vapob1/n1)^0.5
axis(1, lwd=2)
abline(v=mepob1,lty=2, lwd=2)
rect(0.42,0,0.4905,dnorm(0.42,mepob1,(vapob1/n1)^0.5))
x23<-runif(1e6,0.42,0.4905)
y23<-runif(1e6,0,dnorm(0.42,mepob1,(vapob1/n1)^0.5))
ca1<-ifelse(y23<=dnorm(x23,mepob1,(vapob1/n1)^0.5),'red','black')
points(x23,y23,col=ca1, pch=c(19,19))

(table(ca1)[2]/1e6)*(0.4905-0.420)*dnorm(0.42,mepob1,(vapob1/n1)^0.5)

######################

mepob3<-38000
varpob3<-3000^2
Y=0.2X+100
#P(Y> 8900)
#P(0.2X+100> 8900)=P(X>44000)
(8900-100)/0.2
x3pob<-rnorm(1e6,mepob3,varpob3^0.5)
sum(x3pob>44000)/1e6

$7541 utulidad promedio

#########################



curve(dnorm(x,200,14),144,256, lwd=2, col='red',xlab='', ylab='',axes=F)
axis(1, lwd=2)
abline(v=200, lwd=2, lty=2)
x4<-seq(165,235,l=200)
polygon(c(165,x4,235),c(0,dnorm(x4,200,14),0),density = 20,
        angle = 45)
        

          