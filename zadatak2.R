#ZADATAK 2

#2.a.
x <- statpr.2122_kol1.zad2$spol
n = length(x)
n_zene = length(x[x=="Z"])
p = n_zene/n
p #0.4666667

#2.b.
#X~B(1, p)
#T = X1+X2+...X45 ~B(45, p)
alpha = 0.05
t = n_zene
# t/n je pouzdan procjenitelj za p, a var od B(1, p) je p*(1-p)

r1 = t/n - qnorm(1-alpha/2)*sqrt((t/n)*(1-t/n))/sqrt(n)
r1 #0.3209046
r2 = t/n + qnorm(1-alpha/2)*sqrt((t/n)*(1-t/n))/sqrt(n)
r2 #0.6124288
#[0.3209046, 0.6124288] je p.i., pa ne mozemo zaljuciti da je vecina zaposlenih muskaraca


#2.c.
y <- statpr.2122_kol1.zad2$primanja
raspon = (max(y)-min(y))/6
raspon <- 1725.7
gran = seq(from = min(y)-0.05, by=raspon, length.out=7)
gran

hist(y, breaks = gran)
#iz prikaza se vidi da bi podaci mogli biti normalno distribuirani

#2.d.
uzorak = sort(y)
mi = mean(y)
sd = sd(y)

dn <- 0
for (i in 1:n) {
  d1<-abs((i-1)/n-pnorm(uzorak[i],mi,sd))
  d2<-abs(i/n-pnorm(uzorak[i],mi,sd))
  m<-max(d1,d2)
  if(m>dn)
  {dn<-m}
}
dn

ks.test(uzorak, "pnorm", mean=mi, sd=sd)
#dobili isti dn JUHUUU
#p_value = 0.1556 sto je vece od svih standardnih razina distribucija
#pa ne odbacijemo hipotezu da podaci pripadaju normalnoj distribuciji 
# s parametrima mi i sd


#2.e.
#H0: mi = 7000
#H1: ne H0

alpha = 0.01
sd = 2000
mi0 = 7000
mi = mean(y)

t = (mi-mi0)/sd*sqrt(n)
t #2.42638

#n je dovoljno velik 
cv1 = qnorm(1-alpha/2)
cv2 = qnorm(alpha/2)
cv1 #2.575829
cv2 #-2.575829

# t ne upada u kritično podrucje, dakle ne odbacujemo hipotezu H0
# u korist H1 na razini znacajnosti 0.01

p1 = 1-pnorm(t)
p2 = pnorm(t)

p_value = 2*min(p1,p2)
p_value 


#2.f.
gamma <- function(mi){
  1 - pnorm(cv1-(mi-mi0)/sd*sqrt(n)) +  pnorm(cv2-(mi-mi0)/sd*sqrt(n))
}

curve(gamma, from=5000, to=9000)




