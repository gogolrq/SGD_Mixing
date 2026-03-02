rm(list = ls())
library(here)
library(MASS)
library(ggplot2)
library(latex2exp)
library(hrbrthemes)
here::i_am("Additional_Simulation/Model4/collect.R")
n.sim = c(20000, 50000, 100000)
b0 = 0
output = c()
result = c()
for (n in n.sim) {
  filename = here("Additional_Simulation",
                  "Model4",
                  "data",
                  paste0("Model4_n", n, ".txt"))
  if (file.exists(filename)) {
    tryCatch({
      df = read.table(filename, sep = " ")
      df = df[, -1]
      result = rbind(result, df)
    }, error = function(e) {
      
    })
  }
}

colnames(result) = c("n", "a", "B", "b1", "l1", "u1", "v1", "tv1")
result$phi1 = (b0[1] >= result$l1 & b0[1] <= result$u1)
result$len1 = result$u1 - result$l1
result$error = abs(b0[1] - result$b1)
result$count = 1
result$atv1 = ave(result$tv1, result$n)
output = aggregate(cbind(result$phi1, result$len1, result$error),
                   list(result$n, result$B),
                   FUN = mean)
output2 = aggregate(result$count, list(result$n, result$B), FUN = sum)
output$count = output2$x
colnames(output) = c("n", "N", "CP", "len1", "error", "count")
print(output)

data = output
data$N = factor(data$N)
title = "Model 4"
xlab = c(20, 50, 100)
ps.options(horizontal = F)
ps.options(height = 4, width = 5)
filename = here("Figure", "Addition_Pro1_CP4.eps")
postscript(filename)
par(mar = c(2, 4, 2, 2))
p1 = ggplot(data = data,
            mapping = aes(
              x = n,
              y = CP,
              group = N,
              color = N,
              pch = N
            )) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 2) +
  labs("test") +
  xlab("n") +
  ylab("") +
  ylim(0.75, 1) +
  theme(text = element_text(size = 15)) +
  ggtitle(title) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_x_continuous(labels = paste0(xlab, "k"), breaks = 10^3 * xlab)

print(p1)
dev.off()
