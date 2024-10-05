All = c(13,9,11,16,1,14,6.5,16,18,5,12,2,10,3,6.5,16,8,4)
All = All - 9.5
All = All*All
sum=sum(All)
mean = c(32/3,119/12,95/12)
mean = mean - 9.5
mean = mean* mean
mean = sum(mean)
mean = mean*6
x1 = c(13,9,11,16,1,14)
x2 = c(6.5,16,18,5,12,2)
x3 = c(10,3,6.5,16,8,4)
x1 = x1 - 32/3
x1 = x1*x1
su1=sum(x1)
x2 = x2 - 119/12
x2 = x2*x2
su2=sum(x2)
x3 = x3 - 95/12
x3 = x3*x3
su3=sum(x3)
summ = su1+su2+su3

### suppose the significant level is alpha = 0.05
### Question (2)
n_5 = c(1:5)
set.seed(39)
data1 = 0.5+0.3*0.1*n_5+rnorm(5,mean=0,sd=1)
set.seed(2)
data2 = 1+0*0.2*n_5+rnorm(5,mean=0,sd=1)
### data 1 a): standard t - test
x1 = 0.1*n_5
fit1=lm(data1~x1)
summary(fit1)
inter1=fit1$coefficients[1]
slope1=fit1$coefficients[2]
t1 = (sqrt(3)*slope1*sqrt(sum((x1-mean(x1))^2)))/(sqrt(sum((data1-inter1-slope1*x1)^2)))
t1
t_left = qt(p=.025,df=3)
t_right = qt(p=.975,df=3)
library("dplyr")
output = between(t1,t_left,t_right)
print(output)
### Since t1 is not greater than t_0.975 and t1 is not smaller than t_0.025, we conclude that we do not Reject H0.
### data 1 b): rank-based test with exact distribution
R = rank(data1)
D = sum((R-n_5)^2)
E_D = (5^3-5)/6
library(combinat)
per=permn(5)
per=do.call(rbind,per)
find_distribution_probability <- function(k){
  num = 0
  for (i in 1:120){
    ite = per[i,]
    result = sum((ite-n_5)^2)
    if (result == k){
      num = num+1
    }
  }
  p = num/120
  return (p)
}
test_sta = abs(D-E_D)
D_up = test_sta + E_D
D_lower = E_D - test_sta
iterr1 = c(c(1:D_lower),c(D_up:40))
pp1 = 0
for (i in iterr1){
  pp1 = pp1 + find_distribution_probability(i)
}
print(pp1<=0.05)
### Since the probability is larger than 0.05 = alpha, we do not reject H0.
### data 1 c): rank-based test with large sample approximation
E_U = (5*(5+1)*mean(x1))/2
Var_U = (5*(5+1)*sum((x1-mean(x1))^2))/12
U = sum(x1*R)
z_app = (U-E_U)/(sqrt(Var_U))
z_app
output = between(z_app,-1.96,1.96)
print(output)
### Since approximated z score is in the range of [z_0.025,z_0.975], we conclude that we do not reject H0.




















### data 2 a): standard t - test
x2 = 0.2*n_5
fit2=lm(data2~x2)
summary(fit2)
inter2=fit2$coefficients[1]
slope2=fit2$coefficients[2]
t2 = (sqrt(3)*slope2*sqrt(sum((x2-mean(x2))^2)))/(sqrt(sum((data2-inter2-slope2*x2)^2)))
t2
t_left = qt(p=.025,df=3)
t_right = qt(p=.975,df=3)
library("dplyr")
output = between(t2,t_left,t_right)
print(output)
### Since t2 is not greater than t_0.975 and t2 is not smaller than t_0.025, we conclude that we do not Reject H0.
### data 2 b): rank-based test with exact distribution
R = rank(data2)
D = sum((R-n_5)^2)
E_D = (5^3-5)/6
test_sta = abs(D-E_D)
D_up = test_sta + E_D
D_lower = E_D - test_sta
iterr2 = c(c(1:D_lower),c(D_up:40))
pp2 = 0
for (i in iterr2){
  pp2 = pp2 + find_distribution_probability(i)
}
print(pp2<=0.05)
### Since the probability is larger than 0.05 = alpha, we do not reject H0.
### data 2 c): rank-based test with large sample approximation
E_U = (5*(5+1)*mean(x2))/2
Var_U = (5*(5+1)*sum((x2-mean(x2))^2))/12
U = sum(x2*R)
z_app = (U-E_U)/(sqrt(Var_U))
z_app
output = between(z_app,-1.96,1.96)
print(output)
### Since approximated z score is in the range of [z_0.025,z_0.975], we conclude that we do not reject H0.






### Question (3)
### Again alpha = 0.05
### set up two raw data
set.seed(32)
X1 = rnorm(5)
Z1 = rnorm(5)
Y1 = 0.5*X1 + sqrt(1-0.5^2)*Z1
data_1 = cbind(X1,Y1)
set.seed(888)
X2 = rnorm(5)
Z2 = rnorm(5)
Y2 = 0*X2 + sqrt(1-0^2)*Z2
data_2 = cbind(X2,Y2)
### data 1 a): classical test
Rx1_y1 = cor(X1,Y1)
test_statistic1 = (sqrt(5)*Rx1_y1)/(1-(Rx1_y1)^2)
test_statistic1
alpha = .05  
z.half.alpha = qnorm(1-alpha/2)
output = between(test_statistic1,-z.half.alpha,z.half.alpha)
print(output)
### Since calculated classic statistic is not in the range of [z_0.025,z_0.975], we reject H0.
### data 1 b): Spearman's rank correlation
sort_data1= data_1[order(data_1[, 1]), ]
R = rank(sort_data1[,2])
D = sum((R-n_5)^2)
E_D = (5^3-5)/6
test_sta = abs(D-E_D)
D_up = test_sta + E_D
D_lower = E_D - test_sta
iterr2 = c(c(1:D_lower),c(D_up:40))
pp2 = 0
for (i in iterr2){
  pp2 = pp2 + find_distribution_probability(i)
}
print(pp2<=0.05)
### Since calculated probability in permutation is larger than 0.05, we do not reject H0.
### data 2 a): classical test
Rx2_y2 = cor(X2,Y2)
test_statistic2 = (sqrt(5)*Rx2_y2)/(1-(Rx2_y2)^2)
test_statistic2
alpha = .05  
z.half.alpha = qnorm(1-alpha/2)
output = between(test_statistic2,-z.half.alpha,z.half.alpha)
print(output)
### Since calculated classic statistic is in the range of [z_0.025,z_0.975], we do not reject H0.
### data 2 b): Spearman's rank correlation
sort_data2= data_2[order(data_2[, 1]), ]
R = rank(sort_data2[,2])
D = sum((R-n_5)^2)
E_D = (5^3-5)/6
test_sta = abs(D-E_D)
D_up = test_sta + E_D
D_lower = E_D - test_sta
iterr2 = c(c(1:D_lower),c(D_up:40))
pp2 = 0
for (i in iterr2){
  pp2 = pp2 + find_distribution_probability(i)
}
print(pp2<=0.05)
### Since calculated probability in permutation is larger than 0.05, we do not reject H0.