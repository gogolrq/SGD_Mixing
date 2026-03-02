rm(list=ls())
library(here)
library(MASS)
here::i_am("Additional_Simulation/Model1/R_main_Add_Model1.R")
source(here::here("myfun.R"))

n = as.integer(n)
a = as.numeric(a)
B = as.integer(B)
RandomSeed = as.integer(RandomSeed)
set.seed(RandomSeed)

# burn-in sample for time series, ignore it for iid
offset=5000
n=n+offset
e0=rnorm(n,sd=sqrt(0.5))
n=n-offset
epsilon=tail(e0,n)
X=rep(1,n)
b0=c(0)
Y=X*b0+epsilon # generate sample


p=1
b1=rnorm(p,sd=0.01) # initial value

mod = SGDMean(Y, a = a, b1, B)
abstar = mod$abstar # bootstrap
ab = mod$ab # point estimate
t = mod$t # number of loops
ci1 = quantile(abstar[, 1], c(0.025, 0.975)) # ci
v1 = mean((abstar[, 1] - ab[1])^2) * t # no use, ignore it
tv1 = (ab[1] - b0[1])^2 * t # rmse


result = c(n,a,B,ab,ci1,v1,tv1) # collect result
df = data.frame()
df = rbind(df, result)
colnames(df) = NULL
print(df)


filename = here("Additional_Simulation","Model1", "data", paste0("Model1_n", n, ".txt"))

write.table(
  df,
  file = filename,
  append = TRUE,
  quote = TRUE,
  sep = " ",
  col.names = FALSE
)