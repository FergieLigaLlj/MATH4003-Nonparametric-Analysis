### Histogram of 1000 Wilcoxon signed rank statistics W
Wl <- vector(mode='list', length=0)
for (t in 1:1000) {
  X = rnorm(50,0,1)
  X_abs = abs(X)
  rank_abs = rank(X_abs)
  sign = X/X_abs
  sign_rank = sign*rank_abs
  W = 0.5*(sum(sign_rank))+637.5
  Wl=append(Wl,W)
}
Wl = as.numeric(Wl)
### Here "Wl" means List of W (no=1000)
hist(Wl)
### normal approximation of wilcoxon signed rank test
n=50
Ew = n*(n+1)/4
Ew
Vw = n*(n+1)*(2*n+1)/24
Vw
X_ = rnorm(1000,Ew,sqrt(Vw))
hist(X_)


### Histogram of 1000 Wilcoxon signed rank statistics W from Exp(1)
Wl_ <- vector(mode='list', length=0)
for (t in 1:1000) {
  X_ = rexp(50,1)
  X_abs_ = abs(X_)
  rank_abs_ = rank(X_abs_)
  sign_ = X_/X_abs_
  sign_rank_ = sign_*rank_abs_
  W_ = 0.5*(sum(sign_rank_))+637.5
  Wl_=append(Wl_,W_)
}
Wl_ = as.numeric(Wl_)
Wl_
### Here "Wl_" means List of W (no=1000) of exp distribution
hist(Wl_)


### It is concluded from the 3 histograms that the distribution Xi affect the result.
### It can be seen that since no negative sign shown in Exp(1) distribution, so all 1000 W = 1275, making histogram useless.
### However, when Xi is generated from N(0,1), it demonstrates similar pattern with Normal Approximation N(Ew,SDw).
### So finally Xi distribution affects the histogram.