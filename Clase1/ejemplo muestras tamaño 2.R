
#con reemplazo
xpob<-c(3,4,7,9,12)

b=1000000
n=5
mu<-rep(0,b)
mu
for(i in 1:b){
  mu[i] = mean(sample(xpob,n,replace = TRUE))
}


table(mu)/b
barplot(table(mu)/b)

varpob<-var(xpob)*(4/5)
mean(mu)
mean(xpob)

var(mu)
varpob/n

#sin reemplazo
xpob<-c(3,4,7,9,12)

b=1000000
n=2
mu<-rep(0,b)
mu
for(i in 1:b){
  mu[i] = mean(sample(xpob,n,replace = FALSE))
}


table(mu)/b
barplot(table(mu)/b)

varpob<-var(xpob)*(4/5)
mean(mu)
mean(xpob)

var(mu)
(varpob/n)*(3/4)


