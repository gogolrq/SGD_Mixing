rm(list=ls())
library(here)
library(ggplot2)
here::i_am("Realdata_ols/hist_bootstrap.R")


for (num in 1:4){
  output = c()
  result = c() 
  filename = here("Realdata_ols",
                  "data",
                  paste0("Model", num, "_ols_a",
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
  df=result
  df=df[which(df$id>0),][1:3]
  title.sim=c("Morning","Afternoon","Evening")
  for (j in 1:3){
    x=df[,j]
    data=data.frame(x)
    colnames(data)=c("b")
    ps.options(horizontal = F)
    ps.options(height=4, width=4)
    filename = here("Figure", paste0("hist_ols_model",num,"_",title.sim[j],".eps"))
    postscript(filename)
    par(mar=c(2,2,3,2))
    p1=ggplot(data, aes(x=b, y=after_stat(density)))+
      geom_histogram(color="darkblue", fill="lightblue")+
      geom_density(alpha=.2, fill="#FF6666")+
      theme(text = element_text(size=15))+
      ylab("")+
      xlab("")+
      ggtitle(paste("Model ",num," - ",title.sim[j],sep=""))+
      theme(plot.title = element_text(hjust = 0.5,face="bold",size=15),axis.text = element_text(face="bold",size=9),legend.text =element_text(size=15) )
    
    print(p1)
    dev.off()
  }
  
  
}
