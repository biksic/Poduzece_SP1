#ZADATAK 1

#1.a.
pod1 = statpr.2122.kol1.zad1$V1[statpr.2122.kol1.zad1$V2==1]
pod2 = statpr.2122.kol1.zad1$V1[statpr.2122.kol1.zad1$V2==2]
pod3 = statpr.2122.kol1.zad1$V1[statpr.2122.kol1.zad1$V2==3]
pod1
pod2
pod3
#1.b.
#svako ima svoju velicinu frekvencija
frekvencije <- function(x){
  freq = numeric(max(x))
  for(i in 0:max(x)){
    freq[i+1] = sum(x==i)
  }
  return(freq)
}

freq1 = frekvencije(pod1)
freq1
freq2 = frekvencije(pod2)
freq2
freq3 = frekvencije(pod3)
freq3

rel_freq3 = freq3/sum(freq3)
rel_freq1 = freq1/sum(freq1)
rel_freq2 = freq2/sum(freq2)
rel_freq1
rel_freq2
rel_freq3

barplot(c(rel_freq1, rel_freq2, rel_freq3))

#1.c.
rasp1 = max(pod1)-min(pod1)
rasp2 = max(pod2)-min(pod2)
rasp3 = max(pod3)-min(pod3)

pros1 = mean(pod1)
pros2 = mean(pod2)
pros3 = mean(pod3)

var1 = var(pod1)
var2 = var(pod2)
var3 = var(pod3)

#mislim da ne dolaze iz iste distribucije, jer im se prikaz relativnih
#frekvencija ne podudara


#1.d.
#iskoristili da je pros3 MLE za lambda
pod3
lam3 = mean(pod3)
n3 = length(pod3)

pi3 = dpois(min(pod3):max(pod3), lambda=lam3)
pi3[length(pi3)] = 1-ppois(max(pod3)-1, lambda=lam3)
sum(pi3)
pi3

nj3 = n3*pi3

nj3 = c(sum(nj3[1:3]), nj3[4:7], sum(nj3[8:11]))
nj3
freq3 = c(sum(freq3[1:3]), freq3[4:7], sum(freq3[8:11]))
freq3

H = sum((freq3-nj3)^2/nj3)
H
df = length(nj3) -1 -1
p_value = 1 - pchisq(H, df=df)
p_value
#p vrijednost veca od svih standarnih razina znacajnosti 
#pa nemamo dovoljno podataka da odbacimo H0 u korist H1

#1.e.
alpha = 0.01
t <- sum(pod2)
n2 = length(pod2)

F <- function(lambda){
  ppois(t, lambda=n2*lambda)
}
curve(F, from=0, to=10)

G <- function(lambda){
  1-ppois(t, lambda=n2*lambda)
}
curve(G, from=0, to=10)

F_pom <- function(lambda){
  F(lambda) - alpha/2
}

rub1 = uniroot(F_pom, lower=2, upper=5)$root

G_pom <- function(lambda){
  G(lambda)-alpha/2
}

rub2 = uniroot(G_pom, lower=2, upper=4)$root
rub1 #3.835437
rub2 #2.59641
