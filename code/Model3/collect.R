rm(list = ls())
library(here)
library(MASS)
library(ggplot2)
library(latex2exp)
library(hrbrthemes)
here::i_am("Model3/collect.R")
n.sim = c(20000, 50000, 100000)
b0 = c(0)
output = c()
result = c()
for (n in n.sim) {
  filename = here("Model3", "data", paste0("Model3_n", n, ".txt"))
  if (file.exists(filename)) {
    tryCatch({
      df = read.table(filename, sep = " ")
      df = df[, -1]
      result = rbind(result, df)
    }, error = function(e) {
      
    })
  }
}
colnames(result) = c("n", "a", "b1", "l1", "u1", "v1", "tv1")
result$phi1 = (b0[1] >= result$l1 & b0[1] <= result$u1)
result$len1 = result$u1 - result$l1
result$error = abs(b0[1] - result$b1)
result$count = 1
result$atv1 = ave(result$tv1, result$n)
output = aggregate(cbind(result$phi1, result$len1, result$error),
                   list(result$n, result$a),
                   FUN = mean)
output2 = aggregate(result$count, list(result$n, result$a), FUN = sum)
output$count = output2$x
colnames(output) = c("n", "a", "CP", "Len", "MSE", "count")
print(output)

# Draw RMSE
title = "Model 3"
data = output
data$a = factor(data$a)
xlab = c(20, 50, 100)
ps.options(horizontal = F)
ps.options(height = 4, width = 5)
filename = here("Figure", "Pro1_MSE3.eps")
postscript(filename)
par(mar = c(2, 4, 2, 2))
p1 = ggplot(data = data,
            mapping = aes(
              x = n,
              y = (MSE),
              group = a,
              color = a,
              pch = a
            )) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 2) +
  labs("test") +
  xlab("n") +
  ylab("") +
  theme(text = element_text(size = 15)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15)
  ) +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = paste0(xlab, "k"), breaks = 10^3 * xlab) +
  labs(color = TeX("  $\\beta$  "), pch = TeX("  $\\beta$  ")) +
  theme(text = element_text(size = 15))

print(p1)
dev.off()

# Draw CP
xlab = c(20, 50, 100)
ps.options(horizontal = F)
ps.options(height = 4, width = 5)
filename = here("Figure", "Pro1_CP3.eps")
postscript(filename)
par(mar = c(2, 4, 2, 2))
p1 = ggplot(data = data,
            mapping = aes(
              x = n,
              y = CP,
              group = a,
              color = a,
              pch = a
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
  scale_x_continuous(labels = paste0(xlab, "k"), breaks = 10^3 * xlab) +
  labs(color = TeX("  $\\beta$  "), pch = TeX("  $\\beta$  "))

print(p1)
dev.off()