# Load required library
library(plotly)
# Generate sample data
set.seed(123)
### Histogram of 1000 Wilcoxon Signed Rank Statistics W from N(50,0,1)
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
### Normal Approximation of Wilcoxon Signed Rank Test of N(50,0,1)
n=50
Ew = n*(n+1)/4
Ew
Vw = n*(n+1)*(2*n+1)/24
Vw
X_ = rnorm(1000,Ew,sqrt(Vw))
# Create Plotly figure with multiple two histograms
fig <- plot_ly() %>%
  add_histogram(x = ~Wl, name = "Wilcoxon Signed Rank from N(0,1)", nbinsx = 30, opacity = 0.7) %>%
  add_histogram(x = ~X_, name = "Its corresponding Normal Approximation", nbinsx = 30, opacity = 0.7) %>%
  layout(title = "Histogram of Wilcoxon Signed Rank Test and its Normal Approximation ~ N(0,1)",
         xaxis = list(title = "Sample W statistics"),
         yaxis = list(title = "Frequency"))
# Render the plot
fig



### Histogram of 1000 Wilcoxon signed rank statistics W from Exp(1) with N = 50
Wl <- vector(mode='list', length=0)
for (t in 1:1000) {
  X = rexp(50,1)
  X = X - log(2)
  X_abs = abs(X)
  rank_abs = rank(X_abs)
  sign = X/X_abs
  sign_rank = sign*rank_abs
  W = 0.5*(sum(sign_rank))+637.5
  Wl=append(Wl,W)
}
Wl = as.numeric(Wl)

### Histogram of Normal Approximation of Wilcoxon Signed Rank Test of EXP(1) with n = 50
n=50
Ew = n*(n+1)/4
Ew
Vw = n*(n+1)*(2*n+1)/24
Vw
X_ = rnorm(1000,Ew,sqrt(Vw))
# Create Plotly figure with multiple histograms
fig <- plot_ly() %>%
  add_histogram(x = ~Wl, name = "Wilcoxon Signed Rank from EXP(1)", nbinsx = 30, opacity = 0.7) %>%
  add_histogram(x = ~X_, name = "Its corresponding Normal Approximation", nbinsx = 30, opacity = 0.7) %>%
  layout(title = "Histogram of Wilcoxon Signed Rank Test and its Normal Approximation ~ EXP(1)",
         xaxis = list(title = "Sample W Statistics"),
         yaxis = list(title = "Frequency"))
# Render the plot
fig


### Answer
### Comparing two combined histogram, it is clear that although they are also following rough normal
### approximation, Wilcoxon Signed Rank Test W statistics from N(0,1) and its normal approximation has very similar distribution.
### But for EXP(1), its mean has bias compared to its approximation, which is to the right of its normal approximation.
### This is because N(0,1) sampling is symmetric but EXP(1) sampling is not symmetric.
### The prior distribution affects the result.




### Normal Approximation of Mann Whitney U Test
### First with 50 sample points from N(50,0,1) for both Set 1 and Set 2
### Histogram of 1000 Mann Whitney U test statistics U2 from X1~N(50,0,1) and x2~N(50,0,1)
Wl2 <- vector(mode='list', length=0)
for (t in 1:1000) {
  X1 = rnorm(50,0,1)
  X2 = rnorm(50,0,1)
  XX = c(X1,X2)
  Xrank = rank(XX)
  X1rank = Xrank[c(1:50)]
  X2rank = Xrank[c(51:100)]
  U1 = sum(X1rank) - 1275
  U2 = sum(X2rank) - 1275
  Wl2 = append(Wl2,U2)
}
Wl2 = as.numeric(Wl2)



### normal approximation of 1000 Mann Whitney U test statistics U2 from X1~N(50,0,1) and x2~N(50,0,1)
n1=50
n2=50
Ew = n1*n2*0.5
Ew
Vw = n1*n2*(0.5*0.5+49/12+49/12)
Vw
X2_ = rnorm(1000,Ew,sqrt(Vw))
# Create Plotly figure with multiple two histograms
fig <- plot_ly() %>%
  add_histogram(x = ~Wl2, name = "Mann Whitney U test from N(0,1)", nbinsx = 30, opacity = 0.7) %>%
  add_histogram(x = ~X2_, name = "Its corresponding Normal Approximation", nbinsx = 30, opacity = 0.7) %>%
  layout(title = "Histogram of Mann Whitney U Test and its Normal Approximation ~ N(0,1)",
         xaxis = list(title = "Sample U Statistics"),
         yaxis = list(title = "Frequency"))
# Render the plot
fig

### Second with 50 sample points from exp(50,1) for both Set 1 and Set 2
### Histogram of 1000 Mann Whitney U test statistics U2 from X1~exp(50,1) and x2~exp(50,1)
Wl2 <- vector(mode='list', length=0)
for (t in 1:1000) {
  X1 = rexp(50,1)
  X2 = rexp(50,1)
  XX = c(X1,X2)
  Xrank = rank(XX)
  X1rank = Xrank[c(1:50)]
  X2rank = Xrank[c(51:100)]
  U1 = sum(X1rank) - 1275
  U2 = sum(X2rank) - 1275
  Wl2 = append(Wl2,U2)
}
Wl2 = as.numeric(Wl2)



### normal approximation of 1000 Mann Whitney U test statistics U2 from X1~exp(50,1) and x2~exp(50,1)
n1=50
n2=50
Ew = n1*n2*0.5
Ew
Vw = n1*n2*(0.5*0.5+49/12+49/12)
Vw
X2_ = rnorm(1000,Ew,sqrt(Vw))

# Create Plotly figure with multiple two histograms
fig <- plot_ly() %>%
  add_histogram(x = ~Wl2, name = "Mann Whitney U test from EXP(1)", nbinsx = 30, opacity = 0.7) %>%
  add_histogram(x = ~X2_, name = "Its corresponding Normal Approximation", nbinsx = 30, opacity = 0.7) %>%
  layout(title = "Histogram of Mann Whitney U Test and its Normal Approximation ~ EXP(1)",
         xaxis = list(title = "Sample U Statistics"),
         yaxis = list(title = "Frequency"))
# Render the plot
fig
### Answer
### From the Two combined graph it is clear that both N(0,1) and EXP(1) have rough normal distribution along with their 
### normal approximation
### Also their mean is very close and their variance is similar, so the prior distribution doesn't affect the reslt of U Test.