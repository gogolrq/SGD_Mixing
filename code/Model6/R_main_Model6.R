# Model 6
rm(list = ls())
library(here)
library(MASS)
here::i_am("Model6/R_main_Model6.R")
source(here::here("myfun.R"))

n = as.integer(n)
a = as.numeric(a)
RandomSeed = as.integer(RandomSeed)
set.seed(RandomSeed)

# burn-in sample for time series, ignore it for iid
offset=5000
n=n+offset
I=c(0.1,0,0,
    0,0.5,0,
    0,0,1)
I=matrix(I, ncol=3)
epsilon=rnorm(2,sd=1)
mnum=20
for (i in 3:n){
  z=0.5*epsilon[i-1]+0.4*epsilon[i-2]+rnorm(1,sd=1)
  epsilon=c(epsilon,z)
}

epsilon=epsilon/3
n=n-offset
epsilon=tail(epsilon,n)
alpha=c(1,1,-2)
X=mvrnorm(n,mu=alpha, Sigma=I)
X=matrix(X, ncol=3)
X=cbind(X[,1:3])
b0=c(-0.2,0.3,0.1)
Y=X%*%b0+epsilon # generate sample

 
p=3
b1=rnorm(p,sd=0.1)

mod=SGDOLS(X,Y, a=a, b1)
abstar=mod$abstar # bootstrap
ab=mod$ab # point estimate
t=mod$t # number of loops
Bt.sim=mod$Bt.sim # batch sizes

ci1=quantile(abstar[,1],c(0.025,0.975))
ci2=quantile(abstar[,2],c(0.025,0.975))
ci3=quantile(abstar[,3],c(0.025,0.975))
ci4=c(0,0) # useless variable, we only have three slopes
v1=mean((abstar[,1]-ab[1])^2)*t^2/sum(1/Bt.sim)
v2=mean((abstar[,2]-ab[2])^2)*t^2/sum(1/Bt.sim)
v3=mean((abstar[,3]-ab[3])^2)*t^2/sum(1/Bt.sim)
v4=0 # useless variable, we only have three slopes

result=c(n,a,ab,ci1,ci2,ci3,ci4,v1,v2,v3,v4) # collect result
df=data.frame()
df=rbind(df,result)
colnames(df)=NULL
print(df)

filename = here("Model6", "data", paste0("Model6_n", n, ".txt"))

write.table(
  df,
  file = filename,
  append = TRUE,
  quote = TRUE,
  sep = " ",
  col.names = FALSE
)