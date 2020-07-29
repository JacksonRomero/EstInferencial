9-3*3
#Q1=6.99 y Q3=11.01
  
mu=9
sigma=3
curve(dnorm(x,mu,sigma),0,18, col='black', lwd=2,axes=F, xlab='',
      ylab='')
#axis(1,lwd=2)
x1a<-seq(0,6.99,l=100)
polygon(c(0,x1a,6.99),c(0,dnorm(x1a,mu,sigma),0),col='blue',
        density = 20, angle=45)
x1b<-seq(11.01,18,l=100)
polygon(c(11.01,x1b,19),c(0,dnorm(x1b,mu,sigma),0),col='blue',
        density = 20, angle=45)
abline(v=mu, col='red', lty=2)
text(2.5,0.04,expression(paste(0.25)))
text(15.5,0.04,expression(paste(0.25)))
text(6.99,0,"Q1")
text(10.58,0,"Q3")


media=mean(c(6.99,11.01))
media
# P(X<=6.99)=0.25
#P(x-mu/sigma<=(6-.99-9/sigma))=0.25
#P(Z<=-2.01/sigma)=0.25
-2.01/sigma=-0.67
 qnorm(0.25)
sigma=-2.01/qnorm(0.25)
sigma=3
mu=9

#b. Media muestral
n=4

curve(dnorm(x,mu,sigma/2),3,15, col='black', lwd=2,axes=F, xlab='',
      ylab='')

axis(1,lwd=2)
abline(v=9, lwd=2, lty=2, col='red')
x1b1<-seq(6,12,l=100)
polygon(c(6,x1b1,12),c(0,dnorm(x1b1,mu,sigma/2),0),col='grey70',
        density = 20, angle=45)
text(9,0.06, expression(paste(0.9544)))
text(6.2,0, "a")
text(11.8,0, "b")
dnorm(12,9,1.5)
segments(6,dnorm(12,9,1.5),9, dnorm(12,9,1.5),lty=3,col='red')
segments(9,dnorm(12,9,1.5),12, dnorm(12,9,1.5),lty=3,col='red')

text(7.5,0.0385,expression(paste("m")))
text(10.5,0.0385,expression(paste("m")))
b=9+m
a=9-m
#P(a< Xbar<b)=P((9-m-9)/1.5< (Xbar-mu)/sigma/n^0.5<(9+m-9)/1.5)
P(-m/1.5<Z<m/1.5)=0.9544
P(Z<|-m/1.5|)=2P(Z<m/1.5)-1=0.9544
P(Z<m/1.5)=0.9772

qnorm(0.9772)
m=2*1.5
m
a
b

#Propiedades
P(|x-mu|<2sigma)=0.9544
9-2*1.5


### Ejercicio 2

u=250
s=3
n=36



curve(dnorm(x,250,3/6),248.5,251.5, col='black', lwd=2,axes=F, xlab='',
      ylab='')
axis(1, labels =T,lwd=2)
abline(v=250, lty=2, col='red')
x2<-seq(248.5,249,l=100)
polygon(c(248.5,x2,249),c(0,dnorm(x2,250,0.5),0),col='blue',
        density = 20, angle=45)
x3<-seq(251,251.5,l=100)
polygon(c(251,x3,251.5),c(0,dnorm(x3,250,0.5),0),col='blue',
        density = 20, angle=45)
#P(xbar<249|u=250)
#P(Z<249-250/0.5)P(Z<-2)
pnorm(-2)
prob<-2*0.0227
prob

#B.
curve(dnorm(x,250,3/6),246.5,251.5, col='red', lwd=2,axes=F, xlab='',
      ylab='')
axis(1, labels =T,lwd=2)
abline(v=250, lty=2, col='red', lwd=2)

x2b<-seq(248.5,249,l=100)
polygon(c(248.5,x2,249),c(0,dnorm(x2,250,0.5),0),col='blue',
        density = 20, angle=45)
f<-function(x) dnorm(x,248,0.5)
xbb<-seq(246.5,249.5,l=100)
points(xbb, f(xbb),type='l',col='blue')
abline(v=248,lwd=2, lty=2)
xb1<-seq(249,249.5,l=100)
polygon(c(249,xb1,249.5),c(0,dnorm(xb1,248,0.5),0),col='red',
        density = 20, angle=45)
#P(xbar>249|u=248)=P(Z>249-248/0.5)=P(z>2)=
1-pnorm(2)
## Simulación montecarlo
#a
curve(dnorm(x,250,0.5),248.5, 251.5,col='red',lwd=2)  
rect(248.5,0,249,dnorm(249,250,0.5))
xm1<-runif(1e5,248.5,249)  
ym1<-runif(1e5,0,dnorm(249,250,0.5))  
co<-ifelse(ym1<=dnorm(xm1,250,0.5),'red','black')
points(xm1,ym1,col=co, pch=c(19,19))
iqz<-table(co)[2]/1e5*(249-248.5)*dnorm(249,250,0.5)
iqz
2*iqz


#Simulación clásica
xp<-rnorm(1e3,250,0.5)
a<-NULL
for(i in 1:1e5){
  a[i]=sample(xp,36,T)
}
a
pro<-table(a<249)[2]/1e5
2*pro

#Ejercicio 3

#a 
1000-3*100
curve(dnorm(x,1000,100),700,1300,col='red', lwd=2,
    xlab='',ylab='', axes=F)
abline(v=1000,lwd=2,lty=2)
axis(1,lwd=2)
x31<-seq(700,800,l=100)
polygon(c(700,x31,800),c(0,dnorm(x31,1000,100),0),col='blue',
        density = 20, angle=45)
x32<-seq(1200,1300,l=100)
polygon(c(1200,x32,1300),c(0,dnorm(x32,1000,100),0),col='blue',
        density = 20, angle=45)
(1-0.9544)/2
#P(Xbar<800)=P(Z< (n^0.5)(800-1000)/1000)=
#P(Z<-0.2n^0.5)=0.0228
qnorm(0.0228) 
-2
-0.2n^0.5=-2
n<-(2/0.2)^2
n
#b.

#P(xbar>1100)=P(Z>1100-1000/100)=P(Z>1)=1-P(Z<=1)
1-pnorm(1)

curve(dnorm(x,1000,100),700,1300,col='red', lwd=2,
      xlab='',ylab='', axes=F)
axis(1, lwd=2)
abline(v=1000, lwd=2, lty=2)
rect(1100,0,1300,dnorm(1100,1000,100))
n1=1e5
x3mc<-runif(n1,1100,1300)
y3mc<-runif(n1,0,dnorm(1100,1000,100))
cp3<-ifelse(y3mc<=dnorm(x3mc,1000,100),'red','black')
points(x3mc,y3mc, col=cp3,'red','black')
       ,pch=c(19,19))

#for(k in 1:50){
#points(x3mc[1:k],y3mc[1:k], col=ifelse(y3mc[1:k]<=dnorm(x3mc[1:k],1000,100),'red','black')
 #      ,pch=c(19,19))
#  Sys.sleep(1)
#}

 (table(cp3)[2]/n1)*(1300-1100)*dnorm(1100,1000,100)
   
   
   
   
manuel cordova zamora estadistica inferencial   
manuel cordova zamora estadistica decriptiva e inferencial
 
 
 
 
 
 
 