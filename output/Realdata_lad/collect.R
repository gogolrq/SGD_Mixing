rm(list = ls())
library(here)
library(MASS)
library(ggplot2)
library(latex2exp)
library(hrbrthemes)
here::i_am("Realdata_lad/collect.R")
a.sim=c(0.33)

for (k in 1:4) {
  output = c()
  result = c() 
  filename = here("Realdata_lad",
                  "data",
                  paste0("Model", k, "_lad_a",
                         0.33, ".txt"))
  if (file.exists(filename)) {
    tryCatch({
      
      df = read.table(filename, sep = " ")
      df = df[,-1]
      result = rbind(result, df)
    }, error = function(e) {
      
    })
  }
  colnames(result)=c("b1","b2","b3","id")
  result=result[order(result$id),]
  
  for (i in a.sim) {
    df=result
    bhat=df[which(df$id==0),][1:3]
    df=df[which(df$id>0),][1:3]
    ci1=quantile(df[,1],c(0.025,0.975))
    ci2=quantile(df[,2],c(0.025,0.975))
    ci3=quantile(df[,3],c(0.025,0.975))
    output=rbind(ci1,ci2,ci3)
    output=cbind(t(bhat),output)
    print(paste("a=",i,sep=""))
    print(output)
  }
}

