### Eksempler Kap. 12

## CO2-konsentrasjon og vekst av traer

# Registrerer dataene
CO2 = c(408,408,554,554,680,680,812,812)
mass = c(1.1,1.3,1.6,2.5,3.0,4.3,4.2,4.7)

# Plotter sammenhengen mellom masse og CO2:
plot(CO2,mass,lwd=2,cex=2,col='red')

# Finner minste kvadraters estimater:
beta.1.hat = sum((mass-mean(mass))*(CO2-mean(CO2)))/sum((CO2-mean(CO2))^2)
beta.0.hat = mean(mass)-beta.1.hat*mean(CO2)
c(beta.0.hat,beta.1.hat)
fit.CO2 = lm(mass~CO2) # Gjoer det samme og mer
print(fit.CO2)

# Plotter den tilpassede linja::
abline(fit.CO2,lwd=2,col='blue')

# Minste kvadraters estimater m.m
summary(fit.CO2)

## Foedselsvekt for gutter

# Leser dataene
fvekt = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/fvekt.txt",header=TRUE)

# Trekker ut data for guttene:
gutt = fvekt[fvekt$kjonn==1,]

# Plotter vekt mot varighet av svangerskapet
plot(gutt$varighet,gutt$vekt,xlab="varighet",ylab="vekt")

# Finner minste kvadraters estimater:
fit.gutt = lm(vekt~varighet,data=gutt)
print(fit.gutt)

#Vi plotter den tilpassede linja:
abline(fit.gutt,lwd=2,col='blue')

# Minste kvadraters estimater m.m
summary(fit.gutt)

## Sammenheng mellom vekt og stoerrelse paa hjernen

# Leser dataene
hjerne = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/cerebellum.body.weight.txt",header=TRUE)

# Plotter dataene:
plot(hjerne$body,hjerne$cerebellum,xlab="kropsvekt",ylab="hjernevekt")

# Det er bedre aa plotte med log-skala
plot(hjerne$body,hjerne$cerebellum,log='xy',xlab="kropsvekt",ylab="hjernevekt")
# Dette gir det samme som 
# plot(log(hjerne$body),log(hjerne$cerebellum),xlab="kropsvekt",ylab="hjernevekt"),
# medn med opprinnelige x- og-y-verdier langs aksene

# Tilpasser rett linje til logaritmen av vektene
# Vi finner minste kvadraters estimater:
fit.hjerne = lm(log(cerebellum)~log(body),data=hjerne)
print(fit.hjerne)

#Vi plotter log-vektene og den tilpassede linja:
plot(log(hjerne$body),log(hjerne$cerebellum),xlab="log(kropsvekt)",ylab="log(hjernevekt)")
abline(fit.hjerne,lwd=2,col='blue')

# Tilpasser lineaer regresjon der sentrerer varigheten
# ved aa trekke fra 40 uker, slik at konstantledded svarer
# til forventet foedselsvekt ved 40 uker
fit.gutt.s = lm(vekt~I(varighet-40),data=gutt)
summary(fit.gutt.s)
summary(fit.gutt)

## CO2-konsentrasjon og vekst av traer (forts.)

# Konfidensintervall for beta-ene:
confint(fit.CO2)

# Variananalysetabell:
summary(fit.CO2)
anova(fit.CO2)

# Vi estimerer forventet respons for CO2=500 ppm og bestemmer 
# 95% konfidensintervall for forventet respons (se side 656 i laereboka):
new.CO2 = data.frame(CO2=500)
predict(fit.CO2,new.CO2,interval="confidence")
Y.hat = fit.CO2$coeff[1]+fit.CO2$coeff[2]*500
Y.hat
n = length(CO2)
S = sqrt(sum((mass-fit.CO2$fitted)^2)/(n-2))
Sxx = sum((CO2-mean(CO2))^2)
c(Y.hat-qt(0.025,n-2,lower.tail=FALSE)*S*sqrt(1/n+(500-mean(CO2))^2/Sxx),Y.hat+qt(0.025,n-2,lower.tail=FALSE)*S*sqrt(1/n+(500-mean(CO2))^2/Sxx))

# Vi bestemmer 95% prediksjonsintervall for en 
# ny respons ved CO2=500 ppm (se side 659 i laereboka):
predict(fit.CO2,new.CO2,interval="prediction")
c(Y.hat-qt(0.025,n-2,lower.tail=FALSE)*S*sqrt(1+1/n+(500-mean(CO2))^2/Sxx),Y.hat+qt(0.025,n-2,lower.tail=FALSE)*S*sqrt(1+1/n+(500-mean(CO2))^2/Sxx))

# Vi legger inn 95% konfidensintervall i plottet av dataene med den tilpassede linja:
new.CO2 = data.frame(CO2=seq(400,820,10))
confint = predict(fit.CO2,new.CO2,interval="confidence")
plot(CO2,mass,lwd=2,cex=2,col='red')
lines(new.CO2$CO2,confint[,2],lty=2,lwd=2,col='blue')
lines(new.CO2$CO2,confint[,3],lty=2,lwd=2,col='blue')

# Vi legger inn 95% prediksjonsintervall i plottet:
predint = predict(fit.CO2,new.CO2,interval="prediction")
lines(new.CO2$CO2,predint[,2],lty=3,lwd=3,col='blue')
lines(new.CO2$CO2,predint[,3],lty=3,lwd=3,col='blue')

## Foedselsvekt for gutter (forts.)

# Konfidensintervall for beta-ene:
confint(fit.gutt)

# Variananalysetabell:
summary(fit.gutt)
anova(fit.gutt)

# Vi estimerer forventet foedselsvekt for varighet 40 uker 
# og bestemmer 95% konfidensintervall for forventet foedselsvekt
new.gutt = data.frame(varighet=40)
predict(fit.gutt,new.gutt,interval="confidence")

# Vi bestemmer 95% prediksjonsintervall for foedselsvekten til
# en gutt det varugheten av svangerskapet er 40 uker:
predict(fit.gutt,new.gutt,interval="prediction")

# Vi legger inn 95% konfidensintervall i plottet av dataene med den tilpassede linja:
plot(gutt$varighet,gutt$vekt,xlab="varighet",ylab="vekt")
new.gutt = data.frame(varighet=seq(29,44,1))
confint = predict(fit.gutt,new.gutt,interval="confidence")
lines(new.gutt$varighet,confint[,2],lty=2,lwd=2,col='blue')
lines(new.gutt$varighet,confint[,3],lty=2,lwd=2,col='blue')

# Vi legger inn 95% prediksjonsintervall i plottet:
predint = predict(fit.gutt,new.gutt,interval="prediction")
lines(new.gutt$varighet,predint[,2],lty=3,lwd=3,col='blue')
lines(new.gutt$varighet,predint[,3],lty=3,lwd=3,col='blue')

# Andel observasjoner utafor prediksjonsintervallene
varighet = sort(unique(gutt$varighet))
n.varighet = rep(0,length(varighet))
in.pred.int = rep(0,length(varighet))
for(i in 1:length(varighet))
{
  ind = which(gutt$varighet == varighet[i])
  n.varighet[i] = length(ind)
  in.pred.int[i] = mean(as.numeric((gutt$vekt[ind] >= predint[i,2])&(gutt$vekt[ind] <= predint[i,3]))) 
}
cbind(varighet,in.pred.int,n.varighet)

## CO2-konsentrasjon og vekst av traer (forts.)

# Leser inn dataene:
CO2 = c(408,408,554,554,680,680,812,812)
mass = c(1.1,1.3,1.6,2.5,3.0,4.3,4.2,4.7)

# Finner minste kvadraters estimater:
fit.CO2 = lm(mass~CO2)
summary(fit.CO2)

# Beregner residualer og plotter dem mot CO2
res = residuals(fit.CO2)
plot(CO2,res)

# Bergner standardiserte residualer
n = length(CO2)
y = mass
y.hat = fit.CO2$fitted.values
x = CO2
S.xx = sum((x-mean(x))^2)
s = summary(fit.CO2)$sigma
res.stand = (y-y.hat)/(s*sqrt(1-1/n-(x-mean(x))^2/S.xx))
res.stand

# Standadisert residualene kan oppnaas direkte med kommandoen
rstandard(fit.CO2)

# Plott av standardiserte residualer mot tilpassede verdier
plot(y.hat,res.stand)

# Normalfordelingsplott av standardiserte residualer
qqnorm(res.stand)
qqline(res.stand)

## Eksempel: Volum, hoeyde og diameter av traer

# Leser dataene
trees = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/trees.txt",header=TRUE)

# Ser foerst paa modell med volum som respons (y) og diameter som forklaringsvariabel (x)
plot(trees$diameter,trees$volume)
fit.diam = lm(volume~diameter,data=trees)
summary(fit.diam)
abline(fit.diam)

# Plotter residualer mot diameter
res = residuals(fit.diam)
plot(trees$diameter,res)

# Plott av standardiserte residualer mot tilpassede verdier
y.hat = fit.diam$fitted.values
res.stand = rstandard(fit.diam)
plot(y.hat,res.stand)

# Normalfordelingsplott av standardiserte residualer
qqnorm(res.stand)
qqline(res.stand)        

# R har "innebygd" noen plott for modellsjekk:
plot(fit.diam)

# Ser saa paa modell med volum som respons (y) og hoeyde som forklaringsvariabel (x)
plot(trees$height,trees$volume)
fit.height = lm(volume~height,data=trees)
summary(fit.height)
abline(fit.height)

# Plotter residualer mot diameter
res = residuals(fit.height)
plot(trees$height,res)

# Plott av standardiserte residualer mot tilpassede verdier
y.hat = fit.height$fitted.values
res.stand = rstandard(fit.height)
plot(y.hat,res.stand)

# Normalfordelingsplott av standardiserte residualer
qqnorm(res.stand)
qqline(res.stand)        

# R har "innebygd" noen plott for modellsjekk:
plot(fit.height)

## Eksempel: Foedselsvekt for nyfoedte
# Variabler:
# * alder = mors alder (aar)
# * varighet = varigheten av svangerskapet (uker)
# * kjonn = barnets kjoenn (1=gutt, 2=jente)
# * paritet = barnets paritet (1=foerste barn, 2=andre barn, osv.)
# * vekt = foedselsvekt (gram)

# Vi leser dataene
fvekt = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/fvekt.txt",header=TRUE)

# Vi begrenser oss til aa se paa "fullgaatte svangerskap",
# dvs. med varighet minst 37 uker. Vi ser ogsaa bort fra
# svangerskap som varte mere enn 42 uker.
ind = (fvekt$varighet >= 37)&(fvekt$varighet <= 42)
fvekt = fvekt[ind,]

# Vi ser paa en lineaer regresjonsmodell med vekt som respons 
# og varighet og kjoenn som forklaringsvariabler:
fit.vekt = lm(vekt~varighet+kjonn,data=fvekt)
summary(fit.vekt)

# For aa faa en fornuftig fortolkning av konstantleddet, trekker vi 40 uker 
# fra varigheten og definerer kjonn som en kategorisk forklaringsvariabel 
# ("factor") som faar veridien 0 for gutt og 1 for jente
fit.vekt = lm(vekt~I(varighet-40)+factor(kjonn),data=fvekt)
summary(fit.vekt)

# Residualplott
plot(fit.vekt,1)
plot(fit.vekt,2)
plot(fit.vekt,3)

# Konfidensintervall for betaene
confint(fit.vekt)

# Variansanalysetabell
anova(fit.vekt)

# Vi estimerer forventet foedselsvekt for gutter og jenter ved
# varighetene 38 uker, 39 uker og 40 uker 
# og bestemmer 95% konfidensintervall for forventet foedselsvekt
nytt.barn = data.frame(varighet=c(38,39,40,38,39,40),kjonn=c(1,1,1,2,2,2))
predict(fit.vekt,nytt.barn,interval="confidence")

# Vi bestemmer ogsaa 95% prediksjonsintervall for en gutt og en jente ved
# varighetene 38 uker, 39 uker og 40 uker
predict(fit.vekt,nytt.barn,interval="prediction")

# Modellen vi har sett paa, forusetter at gutter og jenter 
# vokser like fort. For aa undersoeke om dette er rimelig, 
# kan vi tilpasse en modell med interaksjon (samspill)
fit.vekt.int = lm(vekt~I(varighet-40)+factor(kjonn)+I(varighet-40):factor(kjonn),data=fvekt)
summary(fit.vekt.int)

# Vi vil naa se paa en modell som ogsaa inkluderer paritet 
# Vi skaffer oss foerst oversikt over fordelingen av paritet
table(fvekt$paritet)

# Siden det er forholdsvis faa med paritet stoerre enn fire, definerer vi en ny 
# variabel for paritet, der vi ikke skiller mellom paritet 4 eller hoeyere
fvekt$nyparitet = ifelse(fvekt$paritet<=4,fvekt$paritet,4)

# For aa faa en fornuftig fortolkning av konstantleddet, trekker vi 40 uker  
# fra varigheten og definerer kjonn og nyparitet som kategoriske 
# forklaringsvariabler ("factor") 
fit.vekt = lm(vekt~I(varighet-40)+factor(kjonn)+factor(nyparitet),data=fvekt)
summary(fit.vekt)

# Vi tilpasser ogsaa en modell der vi bare skiller mellom foerstefoedte og senere foedte
fit.vekt1=lm(vekt~I(varighet-40)+factor(kjonn)+I(paritet>=2),data=fvekt)
summary(fit.vekt1)

# Vi sammenligner de to modellene med en F-test 
anova(fit.vekt1,fit.vekt)
 
## Eksempel: Karakterer for computer science studenter
# Variabler:
# id: Student nummer
# kar: Gjennomsnittskarakter for de tre foerste semestrene.
# hsm: Gjennomsnittskarakter i matematikk fra high school.
# hse: Gjennomsnittskarakter i engelsk fra high school.
# satm: poeng fra matematikkdelen av SAT-testen
# satv: poeng fra den verbale delen av SAT-testen

# Leser inn dataene
karakterer = read.table("http://www.uio.no/studier/emner/matnat/math/STK1110/data/karakterer.txt",header=TRUE)

# Matriseplott viser dataene:
plot(karakterer[,-1])

# Tilpasser lineaer regresjonsmodell
fit.all = lm(kar~hsm+hse+satm+satv,data=karakterer)
summary(fit.all)

# Residualplott:
plot(fit.all,1)
plot(fit.all,2)
plot(fit.all,3)

# Vi tester om SAT-resultatene har betydning gitt high school karakterene
fit.hs = lm(kar~hsm+hse,data=karakterer)
anova(fit.hs,fit.all)

## Eksempel: Volum, hoeyde og diameter av traer (forts.)

# Leser dataene
trees = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/trees.txt",header=TRUE)

# Matriseplott viser sammenheng mellom alle variablene
plot(trees)

# Tilpasser modell med volum som respons og 
# diameter og h?yde som forklaringsvariabler
fit.both = lm(volume~diameter+height,data=trees)
summary(fit.both)

# Plott for modellsjekk
plot(fit.both,1)
plot(fit.both,2)
plot(fit.both,3)

# Plott av residualene mot diameter og hoeyde
res = residuals(fit.both)
plot(trees$diameter,res)
plot(trees$height,res)

# Tilpasser modell med volum som respons og 
# diameter, diameter^2 og hoeyde som forklaringsvariabler
fit.both2 = lm(volume~diameter+I(diameter^2)+height,data=trees)
summary(fit.both2)

# Plott for modellsjekk
plot(fit.both2,1)
plot(fit.both2,2)
plot(fit.both2,3)

# Formelen for et sylinder indikerer at volumet
# er proporsjonalt med hoeyde*diameter^2
# Det motiverer aa tilpasse en modell med log-transformeter variabler
fit.log = lm(log(volume)~log(diameter)+log(height),data=trees)
summary(fit.log)

# Plott for modellsjekk
plot(fit.log,1)
plot(fit.log,2)
plot(fit.log,3)

## Effekt av etanol paa soen hos rotter

# Leser inn dataene
exmp11.5 = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/exmp11-05.txt",header=TRUE,sep=',')

# Enveis variansanalyse:
fit.aov = aov(sleep~factor(treat),data=exmp11.5)
summary(fit.aov)

# For aa estimere alpha-ene maa vi tilpasse modellen
# som en lineaer regresjonsmodell

# Vil bruke restriksjonen at alfa-ene summerer til null:
options(contrasts=c("contr.sum","contr.poly"))

# Tilpasser regresjonsmodellen:
fit.lm = lm(sleep~factor(treat),data=exmp11.5)
summary(fit.lm)

# Variansanalysetabellen (fra tilpasset regresjonsmodell):
anova(fit.lm)

# Vi vil saa bruker restriksjonen at alfa1=0 (default):
options(contrasts=c("contr.treatment","contr.poly"))

# Vi tilpasser en lineaer regresjonsmodell med denne parameteriseringen
fit.lm=lm(sleep~factor(treat),data=exmp11.5)
summary(fit.lm)
anova(fit.lm)

## Eksempel: Antall foedsler og antall hekkende par av storker
## i Vest-Tyskland i perioden 1965-1980
## Fra graf i notis i Nature 1988, vol 332, side 495

# Leser dataene (antall tusen storker, antall milioner foedte)
storker = c(1.9,1.3,1.1,1.0,1.04,1.0,0.9)
barn = c(1.05,0.78,0.63,0.59,0.57,0.57,0.63)

# Plotter dataene
plot(storker,barn,cex=1.5,col='red',lwd=2)

# Tilpasser lineaer regresjon
fit.stork = lm(barn~storker)
abline(fit.stork)
summary(fit.stork)

## Eksempel: Karakterer for computer science studenter

# leser inn dataene
karakterer = read.table("http://www.uio.no/studier/emner/matnat/math/STK1110/data/karakterer.txt",header=TRUE)

# Vi vil her konsentere oss om de to forklaringsvariabelene satm og hsm

# Vi ser foerst paa sammenhengen mellom karakter og satm
plot(karakterer$satm,karakterer$kar)

# Vi tilpasser en lineaer regresjonsmodell med satm som eneste forklaringsvariabel
fit.sat = lm(kar~satm,data=karakterer)
abline(fit.sat)
summary(fit.sat)

# Vi tilpasser saa en lineaer regresjonsmodell med satm og hsm som forklaringsvariabler
fit.begge = lm(kar~satm+hsm,data=karakterer)
summary(fit.begge)

# Merk at vi faar estimatet i modellen med bare satm ved foelgende beregninger
n = dim(karakterer)[1]
x1 = karakterer$satm
x2 = karakterer$hsm
beta1.hat = fit.begge$coefficients[2]
beta2.hat = fit.begge$coefficients[3]
s11 = (n-1)*var(x1)
s22 = (n-1)*var(x2)
r12 = cor(x1,x2)
gamma1.hat = beta1.hat+beta2.hat*r12*sqrt(s22/s11)
gamma1.hat

## Eks. 12.30 (styrken til betong, prosentdel kalkstein og 
## forholdet mellom vann og sement)

# Vi leser inn dataene
sement = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/exmp12-30.txt",header=TRUE)

# Vi tilpasser en lineaer regresjon med de to forklaringsvariabelene:
fit1 = lm(CompStr~x1+x2, data=sement)
summary(fit1)

# Vi tilpasser ogsaa en modell med interaksjon:
fit2 = lm(CompStr~x1+x2+x1:x2, data=sement)
summary(fit2)

# Vi sammenligner modellene
anova(fit1,fit2)

# Vi gaar videre med modellen med interaksjon
# Vi vil illustrere matriseformelen med denne modellen

# Modellmatrisen
X = model.matrix(fit2)
X

# Minste kvadraters estimater
y = sement$CompStr
beta.hat = solve(t(X)%*%X)%*%t(X)%*%y
beta.hat

# Hattmatrisen, predikerte verdier og residualer
H = X%*%solve(t(X)%*%X)%*%t(X)
y.hat = H%*%y
e = y-y.hat
cbind(y.hat,fitted(fit2))
cbind(e,residuals(fit2))

# Kvadratsummer, residualvarians og forklart variasjon
n = length(y)
k = dim(X)[2]-1
y.mean = matrix(mean(y),n,1)
SST = t(y-y.mean)%*%(y-y.mean)
SSR = t(y.hat-y.mean)%*%(y.hat-y.mean)
SSE = t(y-y.hat)%*%(y-y.hat)
S2 = SSE/(n-(k+1))
R2 = SSR/SST
R2adj = 1-(SSE/(n-(k+1)))/(SST/(n-1))
sqrt(S2)
R2
R2adj
  
# Estimert kovariansmatrise for beta.hat-ene
# (Siden R oppfatter S2 beregnet ovenfor som en 1x1 matrise maa vi 
# gjoere den om til en dimensjonsloes skalar for de videre beregningene)
# (Vi kan faa matrisen C direkte ved kommandoen "summary(fit2)$cov.unscaled")
S2 = as.numeric(S2)
C = solve(t(X)%*%X)
cov.beta.hat = S2*C
cov.beta.hat

# Estimert varians og standardfeil for estimatene
var.beta.hat = diag(cov.beta.hat)
se.beta.hat = sqrt(var.beta.hat)
var.beta.hat
se.beta.hat

# Vi ser saa paa standardfeilene til de predikerte verdiene

# Ved aa bruke de "innebygde" komandoene i R finner vi
pred.fit2 = predict(fit2,se.fit=TRUE)
pred.fit2$se.fit

# Vi regner saa direkte med matriser:
cov.y.hat = S2*H
var.y.hat = diag(cov.y.hat)
se.y.hat = sqrt(var.y.hat)
se.y.hat

# Vi ser saa paa de standardiserte residualene

# Ved aa bruke matriseregning faar 
# vi de standardiserte residualene ved
id = diag(1,n,n)
cov.e = S2*(id-H)
var.e = diag(cov.e)
se.e = sqrt(var.e)
stand.res = e/se.e
stand.res

# Ved aa bruke de "innebygde" komandoene i R 
# faar vi de standardiserte residualene ved
rstandard(fit2)

## Eks. 12.12 (SAT-skore og fullfoeringsgrad)
## Vi vil bruke eksempelet til aa illustrere begrepet "leverage"

# Vi leser inn dataene
grad = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/exmp12-12.txt",header=TRUE)

# Vi plotter fullfoeringsgrad mot SAT-skore
plot(grad$SAT,grad$GradRate,xlab="SAT-skore",ylab="Fullfoeringsgrad")

# Vi tilpasser en lineaer modell og tegner inn regresjonslinja i plottet
fit.grad = lm(GradRate~SAT,data=grad)
abline(fit.grad)

# Vi beregner standardiserte residualer og "leverage"
res.st = rstandard(fit.grad)
leverage = hatvalues(fit.grad)
cbind(grad$SAT,grad$GradRate,res.st,leverage)

# Merk at de observasjonene som har stoerst "leverage"
# er de med stoerst eller minst SAT-score (og det er 
# rimelig at det er disse observasjonene som har stoerst
# betydning for den tilpassede regresjonslinja)

# Vi kan plotte standardiserte residualer mot "leverage" ved kommandoen
plot(fit.grad,5)

# Med flere forklaringsvariabler, er det ikke saa
# lett aa se hvilke observasjoner som har stoerst
# betydning for regresjonen 

# Eks. 12.30 (forts.)

# Vi leser inn dataene
sement = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/exmp12-30.txt",header=TRUE)

# Vi tilpasser modell med interaksjon:
fit2 = lm(CompStr~x1+x2+x1:x2, data=sement)
summary(fit2)

# Vi beregner standardiserte residualer og "leverage"
res.st = rstandard(fit2)
leverage = hatvalues(fit2)
cbind(sement$x1,sement$x2,res.st,leverage)

# Vi ser her at den observasjonen som har minst leverage
# er den med (forholdsvis) gjennomsnittlige verdier av baade x1 og x2

## Eks. 12.21 (hoeyde og rekkevidde)

# Vi leser inn dataene
rekkevidde=read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/exmp12-21.txt",header=TRUE)

# Vi plotter rekkevidde mot hoeyde:
plot(rekkevidde$Height,rekkevidde$Wingspan,xlab="Hoeyde",ylab="Rekkevidde")

# Vi beregner korrelsjonen
r = cor(rekkevidde$Height,rekkevidde$Wingspan)
r

# Det er ikke saa interessant aa teste om korrelsajonen
# er lik null i dette eksempelet. Men vi vil se at
# testobservatoren for test om null korrelasjon
# blir den samme som testobservatoren for null effekt
# av av x-variabelen i en lineaer regresjon

# Testobservatoren for null korrelasjon (side 668 i laereboka)
n = dim(rekkevidde)[1]
t.corr = r*sqrt(n-2)/sqrt(1-r^2)
t.corr

# Testobservatoren for lineaer regresjon kan vi lese 
# av summary-kommandoen etter bruk av lm-kommandoen:
fit.lm = lm(Wingspan~Height,data=rekkevidde)
summary(fit.lm)

# Vi ser at testobservatorene blir like

# Vi lager saa et 95% konfidensintervall for korrelasjonen
v = 0.5*log((1+r)/(1-r))
vL = v-qnorm(0.975)/sqrt(n-3)
vU = v+qnorm(0.975)/sqrt(n-3)
korrL = (exp(2*vL)-1)/(exp(2*vL)+1)
korrU = (exp(2*vU)-1)/(exp(2*vU)+1)
c(korrL,korrU)

# Vi kan faa baade testen og konfidensintervallet 
# direkte ved cor.test-kommandoen:
cor.test(rekkevidde$Height,rekkevidde$Wingspan)

## Eksempel 12.14 (Challenger-ulykken)

challenger = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/exmp12-14.txt",header=TRUE)

# Definerer respons og forklaringsvariabel (som temperatur-70 grader F)
y = challenger$fail
x = challenger$temp-70

# Bestemmer maximum likelihood-estimatene 

# Funksjon som beregner log-likelihooden:
loglik = function(b0,b1)
{
  sum(b0*y+b1*x*y-log(1+exp(b0+b1*x)))
}

# Funksjon som beregner den negatve log-likelihooden:
# (siden kommandoen "optim" minimerer, ikke maksimerer)
negloglik = function(beta,x,y)
{
  beta0 = beta[1]
  beta1 = beta[2]
  
  -sum(beta0*y+beta1*x*y-log(1+exp(beta0+beta1*x)))
}

# Maksimerer log-likelihooden (ved aa minimere den negative log-likelihooden numerisk):
ml.est = optim(c(0,0),negloglik,x=x,y=y)  
beta0.hat = ml.est$par[1]
beta1.hat = ml.est$par[2]
c(beta0.hat,beta1.hat)

# Funksjon som beregner (estimert) sannsynlighet for feil som funksjon av v
px = function(x)
{
  exp(beta0.hat+beta1.hat*x)/(1+exp(beta0.hat+beta1.hat*x))
}

# Plotter stimert sannsynlighet for feil som funksjon av temperatur (x+70) 
xx = seq(-15,10,1)
plot(xx+70,px(xx),type='l',ylim=c(0,1),xlab="Grader Farenheit",ylab="Sannsynlighet for feil")

## Eksempel 12.14 (forts.)

# Informasjonsmatrise 
inf.matrise = matrix(0,2,2)
lin = beta0.hat+beta1.hat*x
inf.matrise[1,1] = sum(exp(lin)/(1+exp(lin))^2)
inf.matrise[2,2] = sum(x^2*exp(lin)/(1+exp(lin))^2)
inf.matrise[1,2] = sum(x*exp(lin)/(1+exp(lin))^2)
inf.matrise[2,1] = inf.matrise[1,2]

# Estimert kovariansmatrise og standrardfeil
cov.matrise = solve(inf.matrise)
var.est = diag(cov.matrise)
se.est = sqrt(var.est)
se.est

## Eksempel 12.14 (forts.)

# Vi kan bruke likelihood ratio-testen til aa teste 
# H_0: beta1=0 mot alternativet H_a: beta1 != 0

# Under H_0 avhenger ikke sammsynligheten av forklarinsvariabelen x
# Maksimum likelihood-estimat for den sannsynligheten under H_0 er
p0.hat = sum(y)/length(y)

# Maksimum likelihood-estimatet for den tilsvarende verdi av beta_0 er
beta0.null = log(p0.hat/(1-p0.hat))

# Beregner -2*ln(LR) og P-verdien for likelihood ratio-testen
minus.2lnLR = 2*(loglik(beta0.hat,beta1.hat)-loglik(beta0.null,0))
1-pchisq(minus.2lnLR,1)
