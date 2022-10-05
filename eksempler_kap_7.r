### Eksempler Kap. 7

## Estimering for nedboersdataene  (eks 2 i introduksjonsforelesningen)

# Leser dataene fra fil. De gir nedboer (i mm) hver av de 92 dagene i mai-juli 2016
nedbor.blindern = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/nedbor-blindern.txt",header=T)

# Tar bare med de 54 dagene det regnet:
regn = nedbor.blindern$nedbor[nedbor.blindern$nedbor > 0]


# Beregner momentestimatene

n = length(regn)
mom.alpha = mean(regn)^2/(mean(regn^2)-mean(regn)^2)
mom.beta = (mean(regn^2)-mean(regn)^2)/mean(regn)
c(mom.alpha,mom.beta)

# Plotter log-likelihood-funksjonen

# Funksjon som beregner log-likelihooden:
loglik_gamma = function(alpha,beta,x)
{
  -n*alpha*log(beta)-n*lgamma(alpha)+(alpha-1)*sum(log(x))-sum(x)/beta
}
  
# Plotter log-likelihood-funksjonen:
alpha = seq(0.4,1,0.01)
beta = seq(4,14,0.1)
f = outer(alpha,beta,loglik_gamma,x=regn)
persp(alpha, beta, f, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

# Konturplott av log-likelihooden
contour(alpha, beta, f, nlevels=20)

  
# Bestemmer maximum likelihood estimatene 

# Funksjon som beregner den negatve log-likelihooden:
negloglik_gamma = function(log.par,x)
{
  alpha = exp(log.par[1])
  beta = exp(log.par[2])
  -loglik_gamma(alpha,beta,x)
}

# Maksimerer log-likelihooden (ved aa minimere den negative log-likelihooden numerisk):
ml.est=optim(log(c(mom.alpha,mom.beta)),negloglik_gamma,x=regn)  
ml.alpha = exp(ml.est$par[1])
ml.beta = exp(ml.est$par[2])
c(ml.alpha,ml.beta)  

# Plotter dataene mot gammafordelingene estimert med momentmetoden 
# og maksimum likelihood

hist(regn,prob=TRUE)
lines(seq(0.01,30,0.1),dgamma(seq(0.01,30,0.1),mom.alpha,scale=mom.beta),col=2)
lines(seq(0.01,30,0.1),dgamma(seq(0.01,30,0.1),ml.alpha,scale=ml.beta),col=3)


# Bestemmer bootstrap estimatene for standardfeilene 

B = 1000

# Ikke-parametrisk bootstrap
mom.alpha.star = mom.beta.star = ml.alpha.star = ml.beta.star = rep(0,B)
for (b in 1:B)
{ 
  x.star = sample(regn,n,replace=T)
  mom.alpha.star[b] = mean(x.star)^2/(mean(x.star^2)-mean(x.star)^2)
  mom.beta.star[b] = (mean(x.star^2)-mean(x.star)^2)/mean(x.star)
  ml.est = optim(c(mom.alpha.star[b],mom.beta.star[b]),negloglik_gamma,x=x.star)  
  ml.alpha.star[b] = exp(ml.est$par[1])
  ml.beta.star[b] = exp(ml.est$par[2])
}

c(sd(mom.alpha.star),sd(mom.beta.star))
c(sd(ml.alpha.star),sd(ml.beta.star))

# Parametrisk bootstrap
mom.alpha.star.p = mom.beta.star.p = ml.alpha.star.p = ml.beta.star.p = rep(0,B)
for (b in 1:B)
{ 
  x.star.mom = rgamma(n,mom.alpha,1/mom.beta)
  x.star.ml = rgamma(n,ml.alpha,1/ml.beta)
  mom.alpha.star.p[b] = mean(x.star.mom)^2/(mean(x.star.mom^2)-mean(x.star.mom)^2)
  mom.beta.star.p[b] = (mean(x.star.mom^2)-mean(x.star.mom)^2)/mean(x.star.mom)
  ml.alpha.start = mean(x.star.ml)^2/sum((x.star.ml-mean(x.star.ml))^2/n)
  ml.beta.start = sum((x.star.ml-mean(x.star.ml))^2/n)/mean(x.star.ml)
  ml.est = optim(c(ml.alpha.start,ml.beta.start),negloglik_gamma,x=x.star.ml)  
  ml.alpha.star.p[b] = exp(ml.est$par[1])
  ml.beta.star.p[b] = exp(ml.est$par[2])
}

c(sd(mom.alpha.star.p),sd(mom.beta.star.p))
c(sd(ml.alpha.star.p),sd(ml.beta.star.p))

# Sammenligner med grenseverdi for maksimum likelihood-estimatene
c(sqrt(ml.alpha/(n*(ml.alpha*trigamma(ml.alpha)-1))),sqrt(ml.beta^2*trigamma(ml.alpha)/(n*(ml.alpha*trigamma(ml.alpha)-1))))

