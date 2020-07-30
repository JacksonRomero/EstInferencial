
#Distribución de la varianza muestral
#X:1,2,3
pob<-c(1,2,3)
pob1<-(pob-mean(pob))^2
vpob<-sum(pob1)/3
vpob
n=3
b<-replicate(1e6,var(sample(1:3,n,T)))
#b
mean(b) #E(s^2)

var(b)

(4/9)*vpob

#Notas de examen en escala 0-100.
#varianza es desconocida
notas<-c(86,81,79,75,95,66,94,90,86,88)
n=600
b1<-replicate(1e5,mean(sample(notas,n,T)))
#b1
z<-(b1-mean(b1))/sd(b1)
hist(z,col='grey80',border='white',lwd=3,axes = F,
     main='',ylab='',xlab='', freq = F)

abline(v=0)
curve(dnorm(x),type='l',-4,4,col='red', lwd=2,lty=2,add=T)
curve(dt(x,n-1),type='l',-4,4,col='blue', lwd=2,lty=2,add=T)
abline(v=-1)
dnorm(-1)
dt(-1,n-1)

#Ejercicio 1

p=0.4
n=600
#Simulación
ej1<-rnorm(1e6,p,(p*(1-p)/n)^0.5)
sum(ej1>=0.37 & ej1<= 0.45)/1e6
#####################################
0.4-3*0.02
  
(0.4*0.6/600)^0.5

curve(dnorm(x,p, sqrt(p*(1-p)/n)),0.34,0.46,lwd=2,
            axes=F,xlab='',ylab='')
axis(1, lwd=2)#eje X
abline(v=0.4, lwd=2)
x11<-seq(0.37,0.45,l=100)
polygon(c(0.37,x11,0.45),c(0,dnorm(x11,p, sqrt(p*(1-p)/n)),0),
        density = 20,angle = 45,col='blue')

#P(0.37 <pest< 0.45)=P( (0.37-0.4)/(0.4*0.6/600)^0.5 <(pest-p)/t(p*(1-p)/n)^0.5< 0.45-0.4/(0.4*0.6/600)^0.5 )
#P(-1.5<Z<2.5)=P(Z<2.5)-P(Z<-1.5)


pnorm(2.5)- pnorm(-1.5)

par(mfrow=c(2,1))

curve(dnorm(x,p, sqrt(p*(1-p)/n)),0.34,0.46,lwd=2,
      axes=F,xlab='',ylab='')
axis(1, lwd=2)#eje X
abline(v=0.4, lwd=2)
x11<-seq(0.37,0.45,l=100)
polygon(c(0.37,x11,0.45),c(0,dnorm(x11,p, sqrt(p*(1-p)/n)),0),
        density = 20,angle = 45,col='blue')
curve(dnorm(x,0,1),-3,3,lwd=2,axes=F)
axis(1)
x12<-seq(-1.5,2.5,l=100)
polygon(c(-1.5,x12,2.5),c(0,dnorm(x12),0),
        density = 20,angle = 45,col='blue')
par(mfrow=c(1,1))

#b.
P(|pes-p|<0.02)=0.97

P(|pes-p|/(pq/n)^0.5<0.02/(0.4*0.6/600)^0.5)

0.02/(0.4*0.6/600)^0.5

0.04n^0.5=2.17 

n=(qnorm(0.985)/0.04)^2
n

##
p=0.02
n=400
(p*(1-p)/n)^0.5

0.02-3*0.007
curve(dnorm(x,0.02,0.007),-0.001,0.041,lwd=2, axes=F,xlab='',
      ylab='', main='')
axis(1, lwd=2)
abline(v=0.02, lwd=2)
x13<-seq(0.03,0.041,l=100)
polygon(c(0.03,x13,0.041),c(0,dnorm(x13,0.02,0.007),0),
        density = 20,angle = 45,col='blue')

#P(pes>0.03|p=0.02)=P(Z>(0.03-0.02)/0.007)
#P(Z>1.43)=1-P(Z<1.43)
1-pnorm(1.43)

#b.
curve(dnorm(x,0.02,0.007),-0.001,0.07, lwd=2,axes=F,
      xlab='',
      ylab='', main='')
abline(v=0.02, lwd=2, col='blue')
axis(1, lwd=2)
x13<-seq(0.03,0.041,l=100)
polygon(c(0.03,x13,0.041),c(0,dnorm(x13,0.02,0.007),0),
        density = 20,angle = 45,col='blue')
f<-function(x){
  dnorm(x,0.04,0.0098)
}
xx3<-seq(0.0106,0.694,l=200)

#round((0.04*(1-0.04)/400)^0.5,4)
#0.04+3*0.0098
points(xx3,f(xx3),type='l', lwd=2)
abline(v=0.04, lwd=2, col='red')
xx31<-seq(0.0106,0.03,l=100)

polygon(c(0.0106,xx31,0.03),c(0,dnorm(xx31,0.04,0.0098),0),
        density = 20,angle = 45,col='red')

#P(pest<0.03|p=0.04)=P(Z<-1.02)
pnorm(-1.02)

