rm(list = ls())
library(here)
library("lubridate")
library(hms)
here::i_am("Realdata_lad/R_main_lad.R")
source(here::here("myfun.R"))
filename = here("Realdata_lad", "household_power_consumption.txt")
df = read.csv(filename, sep = ";", stringsAsFactors = FALSE)

a = 0.33
num = as.integer(num) # correspond to models 1-4.
RandomSeed =  1
set.seed(RandomSeed)


index = which(df$Global_active_power == "?")
ddf = df[-index, ]
y = as.numeric(ddf$Global_active_power)
yy = y * 1000 / 60
x1 = as.numeric(ddf$Sub_metering_1)
x2 = as.numeric(ddf$Sub_metering_2)
x3 = as.numeric(ddf$Sub_metering_3)

time = hms::as_hms(ddf$Time)
time = hour(time)
morning = as.numeric(time >= 0 & time <= 11)
afternoon = as.numeric(time >= 12 & time <= 17)
evening = as.numeric(time >= 18 & time <= 23)
ydata = cbind(y, x1, x2, x3)


Y = ydata[, num]
X = cbind(morning, afternoon, evening)

n = length(Y)
print(n)

p = 3
b1 = rnorm(p, sd = 0.1)

mod = SGDLAD(X, Y, a = a, b1)
abstar = mod$abstar # bootstrap
ab = mod$ab # point estimate
t = mod$t # number of loops
Bt.sim = mod$Bt.sim # batch sizes


result = rbind(ab, abstar)
df = data.frame()
df = rbind(df, result)
df$id = 0:500
print(df)

filename = here("Realdata_lad",
                "data",
                paste0("model", num, "_lad_a", a, ".txt"))
write.table(
  df,
  file = filename,
  append = TRUE,
  quote = TRUE,
  sep = " ",
  col.names = FALSE
)