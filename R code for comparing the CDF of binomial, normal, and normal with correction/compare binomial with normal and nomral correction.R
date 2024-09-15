# R code: compare the CDF of binomial, normal, and normal with correction
n=5
p=0.5
yb=pbinom(0:n,n,p)
yn=pnorm(0:n,n*p,n*p*(1-p))
yc=pnorm((0:n)+1/2,n*p,n*p*(1-p))
plot(0:n,yb)
lines(0:n,yn)
lines(0:n,yc,col="red")

# R code: compare the PDF of binomial, normal, and normal with correction
yb=dbinom(0:n,n,p)
yn=dnorm(0:n,n*p,n*p*(1-p))
yc=pnorm((0:n)+1/2,n*p,n*p*(1-p))-pnorm((0:n)-1/2,n*p,n*p*(1-p))
plot(0:n,yb,ylim=c(0,1))
?plot
lines(0:n,yn)
lines(0:n,yc,col="red")
