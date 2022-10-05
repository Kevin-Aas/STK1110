### Eksempler Kap. 8

## Illustrasjon av oevre kvantil i normalfordelingen

x = seq(-4,4,0.01)
plot(x,dnorm(x),type="l",xlab="",ylab="",axes=FALSE)
axis(1,at=c(-4,-2,0,qnorm(0.9),2,4),labels=c(c(-4,-2,0),expression(z[alpha]),c(2,4)))
axis(2)
abline(v=qnorm(0.9),col=3,lty=2,lwd=2)
z = x[x > qnorm(0.9)]
y = c(0,dnorm(z),0)
z = c(z[1],z,z[length(z)])
polygon(z,y,col=3,border=3,lwd=2)
text(1.9,0.1,expression(alpha),col=3,cex=2)

## Illustrasjon av oevre kvantil i t-fordelingen

x = seq(-6,6,0.01)
plot(x,dt(x,df=3),type="l",xlab="",ylab="",axes=FALSE)
axis(1,at=c(-6,-3,0,qt(0.9,df=3),3,6),labels=c(c(-6,-3,0),expression(t[paste(alpha,",",nu)]),c(3,6)))
axis(2)
abline(v=qt(0.9,3),col=3,lty=2,lwd=2)
z = x[x > qt(0.9,df=3)]
y = c(0,dt(z,df=3),0)
z = c(z[1],z,z[length(z)])
polygon(z,y,col=3,border=3,lwd=2)
text(2.4,0.08,expression(alpha),col=3,cex=2)

## Illustrasjon av oevre kvantil i xi^2-fordelingen

x = seq(0,20,0.1)
plot(x,dchisq(x,df=5),type="l",xlab="",ylab="",axes=FALSE)
axis(1,at=c(0,5,qchisq(0.9,df=5),15,20),labels=c(c(0,5),expression(chi[paste(alpha,",",nu)]^2),c(15,20)))
axis(2)
abline(v=qchisq(0.9,5),col=3,lty=2,lwd=2)
z = x[x > qchisq(0.9,df=5)]
y = c(0,dchisq(z,df=5),0)
z = c(z[1],z,z[length(z)])
polygon(z,y,col=3,border=3,lwd=2)
text(10.5,0.04,expression(alpha),col=3,cex=2)

## Konsentrasjonsmaalinger

# Leser inn konsentrasjonsmaalingene:
kons = c(2.52,2.49,2.62,2.45,2.56)
n = length(kons)

# Normalt qq-plott
qqnorm(kons)
qqline(kons)

# Beregner gjennomsnitt og empirisk standardavvik
gjsn = mean(kons)
st.av = sd(kons)
c(gjsn,st.av)

# Bestemmer oevre (5/2)%-kvantil i t-fordelingen:
t.kvant = qt(0.975,n-1)

# Beregner 95% konfidensintervall:
low = gjsn-t.kvant*st.av/sqrt(n)
up = gjsn+t.kvant*st.av/sqrt(n)
print(c(low,up),digits=3)

# 95% prediksjonsintervall for en ny maaling:
low.pred=gjsn-t.kvant*st.av*sqrt(1+1/n)
up.pred=gjsn+t.kvant*st.av*sqrt(1+1/n)
print(c(low.pred,up.pred),digits=3)

# Bestemmer nedre oevre (5/2)%-kvantil i chi^2-fordelingen:
chi2.kvant.low = qchisq(0.025,df=n-1)
chi2.kvant.up = qchisq(0.975,df=n-1)

print(c(st.av^2,st.av),digits=3)

# 95% konfidensintervall for variansen:
low.var=(n-1)*st.av^2/chi2.kvant.up
up.var=(n-1)*st.av^2/chi2.kvant.low
print(c(low.var,up.var),digits=3)

# 95% konfidensintervall for standardavviket:
print(sqrt(c(low.var,up.var)),digits=3)

## Tipsdata

# Leser inn data fra eksempel 8.17
tip = c(22.7,16.3,13.6,16.8,29.9,15.9,14.0,15.0,14.1,18.1,22.8,27.6,16.4,16.1,19.0,
      13.5,18.9,20.2,19.7,18.2,15.4,15.7,19.0,11.5,18.4,16.0,16.9,12.0,40.1,19.2)
n = length(tip)
mean.tip = mean(tip)
sd.tip = sd(tip)
c(mean.tip,sd.tip)

# Histogram og normalt Q-Q-plott av dataene
par(mfrow=c(1,2))
hist(tip)
abline(v=mean.tip,lty=2,lwd=2)
abline(v=median(tip),lty=3,lwd=2)
qqnorm(tip)
qqline(tip)

# Konfidensintervall basert paa normalfordeling
low.norm = mean.tip-qt(0.975,n-1)*sd.tip/sqrt(n)
up.norm = mean.tip+qt(0.975,n-1)*sd.tip/sqrt(n)
print(c(low.norm,up.norm),digits=3)

# Beregner bootstrap-estimat
B=1000
mean.boot=rep(0,B)
for (b in 1:B)
{
  x.star = sample(tip,n,replace=TRUE)
  mean.boot[b] = mean(x.star)
}

# Lager histogram og normalt Q-Q-plott av bootstrap-estimatene
par(mfrow=c(1,2))
hist(mean.boot)
abline(v=mean(mean.boot),lty=2,lwd=2)
abline(v=median(mean.boot,lty=2,lwd=2))
qqnorm(mean.boot)
qqline(mean.boot)

# Beregner gjennomsnitt og standardavvik for bootstrap-estimatene
c(mean(mean.boot),sd(mean.boot))

# Beregner 95% konfidensinterval basert paa normalfordeling av bootstrap-estimatene
low.boot = mean(tip)-qnorm(0.975)*sd(mean.boot)
up.boot = mean(tip)+qnorm(0.975)*sd(mean.boot)
print(c(low.boot,up.boot),digits=3)

# Beregner 95% konfidensintervall etter persentilmetoden
mean.boot.sort=sort(mean.boot)
low.pers=mean.boot.sort[0.025*B]
up.pers=mean.boot.sort[0.975*B]
print(c(low.pers,up.pers),digits=3)

# R-pakken "boot" har prosedyrer for bootstrap-beregninger, deriblant BCa-intervallet
library(boot)

# Trekker bootstrap-utvalg. Merk at vi foerst maa definere en funksjon 
# som brukes til aa beregne bootstrap-estimatene (her gjennomsnitt)
my.mean = function(x, indices) 
{ 
  mean(x[indices])
}
mean.boot2= boot(tip, my.mean, R=1000)

# "mean.boot2" er et objekt med mange attibutter; disse kan en se ved aa skrive
names(mean.boot2)

# For eksempel er "mean.boot2$t0" estimatet for de 
# opprinnelige dataene, mens "mean.boot2$t" gir bootstrap-estimatene
mean.boot2$t0
mean.boot2$t

# BCa-konfidensintervallet:
boot.ci(mean.boot2,type=c("perc", "bca"))

