g0 = c(1.15,0.88,0.9,0.74,1.21)
mean0 = mean(g0)
mean0
var00 = var(g0)
var00
s0 = log(var00)
s0
g1 = g0[c(2,3,4,5)]
mean1 = mean(g1)
mean1
var1 = sum((g1-mean1)^2)/3
var1
var11 = var(g1)
var11
s1 = log(var11)
s1
g2 = g0[c(1,3,4,5)]
mean2 = mean(g2)
mean2
var22 = var(g2)
var22
s2 = log(var22)
s2
g3 = g0[c(1,2,4,5)]
mean3 = mean(g3)
mean3
var33 = var(g3)
var33
s3 = log(var33)
s3
g4 = g0[c(1,2,3,5)]
mean4 = mean(g4)
mean4
var44 = var(g4)
var44
s4 = log(var44)
s4
g5 = g0[c(1,2,3,4)]
mean5 = mean(g5)
mean5
var55 = var(g5)
var55
s5 = log(var55)
s5
A1 = 5*s0-4*s1
A2 = 5*s0-4*s2
A3 = 5*s0-4*s3
A4 = 5*s0-4*s4
A5 = 5*s0-4*s5
Alist = c(A1,A2,A3,A4,A5)
Amean = mean(Alist)
h0 = c(0.8,0.83,1.89,1.04,1.45,1.38,1.91,1.64,0.73,1.46)
Mean0 = mean(h0)
Mean0
Var0 = var(h0)
Var0
t0 = log(Var0)
t0
h1 = h0[c(-1)]
h1
Mean1 = mean(h1)
Mean1
Var1 = var(h1)
Var1
t1 = log(Var1)
t1

h2 = h0[c(-2)]
h2
Mean2 = mean(h2)
Mean2
Var2 = var(h2)
Var2
t2 = log(Var2)
t2

h3 = h0[c(-3)]
h3
Mean3 = mean(h3)
Mean3
Var3 = var(h3)
Var3
t3 = log(Var3)
t3

h4 = h0[c(-4)]
h4
Mean4 = mean(h4)
Mean4
Var4 = var(h4)
Var4
t4 = log(Var4)
t4

h5 = h0[c(-5)]
h5
Mean5 = mean(h5)
Mean5
Var5 = var(h5)
Var5
t5 = log(Var5)
t5

h6 = h0[c(-6)]
h6
Mean6 = mean(h6)
Mean6
Var6= var(h6)
Var6
t6 = log(Var6)
t6

h7 = h0[c(-7)]
h7
Mean7 = mean(h7)
Mean7
Var7= var(h7)
Var7
t7 = log(Var7)
t7

h8 = h0[c(-8)]
h8
Mean8 = mean(h8)
Mean8
Var8= var(h8)
Var8
t8 = log(Var8)
t8

h9 = h0[c(-9)]
h9
Mean9 = mean(h9)
Mean9
Var9= var(h9)
Var9
t9 = log(Var9)
t9

h10 = h0[c(-10)]
h10
Mean10 = mean(h10)
Mean10
Var10= var(h10)
Var10
t10 = log(Var10)
t10

B1 = 10*t0-9*t1
B2 = 10*t0-9*t2
B3 = 10*t0-9*t3
B4 = 10*t0-9*t4
B5 = 10*t0-9*t5
B6 = 10*t0-9*t6
B7 = 10*t0-9*t7
B8 = 10*t0-9*t8
B9 = 10*t0-9*t9
B10 = 10*t0-9*t10
Blist = c(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10)
Bmean = mean(Blist)

U = var(Alist)/5
V = var(Blist)/10

Q = (Amean-Bmean)/(sqrt(U+V))
Q

Y <- c(-0.15,8.6,5,3.71,4.29,7.74,2.48,3.25,-1.15,8.38)
X <- c(2.55,12.07,0.46,0.35,2.69,-0.94,1.73,0.73,-0.35,-0.37)
dat <- data.frame(weight = c(Y,X), 
                  company = rep(c("Y","X"), each=10))
boxplot(weight ~ company, data = dat)
wilcox.test(weight~company,data=dat,alternative="two.sided")
wilcox.test(x=c(2.55,12.07,0.46,0.35,2.69,-0.94,1.73,0.73,-0.35,-0.37),y=c(-0.15,8.6,5,3.71,4.29,7.74,2.48,3.25,-1.15,8.38),alternative ="two.sided")

?ks.test()
### KS Test on Y ~ N(4,9)
ks.test(Y,"pnorm",mean=4,sd=3)
ks.test(c(-0.15,8.6,5,3.71,4.29,7.74,2.48,3.25,-1.15,8.38),"pnorm",mean=4,sd=3)
### KS Test on X and Y if has same distribution
ks.test(x=X,y=Y,alternative = "two.sided")
ks.test(x=c(2.55,12.07,0.46,0.35,2.69,-0.94,1.73,0.73,-0.35,-0.37),y=c(-0.15,8.6,5,3.71,4.29,7.74,2.48,3.25,-1.15,8.38),alternative = "two.sided")