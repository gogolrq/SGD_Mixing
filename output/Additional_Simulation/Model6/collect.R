rm(list = ls())
library(here)
library(MASS)
library(ggplot2)
library(latex2exp)
library(hrbrthemes)
here::i_am("Additional_Simulation/Model6/collect.R")
n.sim = c(20000, 50000, 100000)
b0 = c(-0.2, 0.3, 0.1)
output = c()
result = c()
for (n in n.sim) {
  filename = here("Additional_Simulation",
                  "Model6",
                  "data",
                  paste0("Model6_n", n, ".txt"))
  if (file.exists(filename)) {
    tryCatch({
      df = read.table(filename, sep = " ")
      df = df[, -1]
      result = rbind(result, df)
    }, error = function(e) {
      
    })
  }
}

colnames(result) = c(
  "n",
  "a",
  "B",
  "b1",
  "b2",
  "b3",
  "l1",
  "u1",
  "l2",
  "u2",
  "l3",
  "u3",
  "l4",
  "u4",
  "v1",
  "v2",
  "v3",
  "v4"
)

result$phi1 = (b0[1] >= result$l1 & b0[1] <= result$u1)
result$phi2 = (b0[2] >= result$l2 & b0[2] <= result$u2)
result$phi3 = (b0[3] >= result$l3 & b0[3] <= result$u3)
result$phi4 = (b0[4] >= result$l4 & b0[4] <= result$u4)
result$error = (b0[1] - result$b1)^2 + (b0[2] - result$b2)^2 + (b0[3] -
                                                                  result$b3)^2
result$error = sqrt(result$error)
result$count = 1
result$len1 = result$u1 - result$l1
result$len2 = result$u2 - result$l2
result$len3 = result$u3 - result$l3
result$len4 = result$u4 - result$l4


output = aggregate(
  cbind(
    result$phi1,
    result$len1,
    result$phi2,
    result$len2,
    result$phi3,
    result$len3,
    result$phi4,
    result$len4,
    result$error
  ),
  list(result$n, result$B),
  FUN = mean
)
output2 = aggregate(result$count, list(result$n, result$B), FUN = sum)
output$count = output2$x
colnames(output) = c("n",
                     "N",
                     "CP1",
                     "len1",
                     "CP2",
                     "len2",
                     "CP3",
                     "len3",
                     "phi4",
                     "len4",
                     "error",
                     "count")
output$error = sqrt(output$error)
print(output)

data = output
data$N = factor(data$N)

# Draw CPs
xlab = c(20, 50, 100)
ps.options(horizontal = F)
ps.options(height = 5, width = 4)
filename = here("Figure", "Addition_CP2_1.eps")
postscript(filename)
par(mar = c(2, 4, 2, 2))
p1 = ggplot(data = data,
            mapping = aes(
              x = n,
              y = CP1,
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
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15)
  ) +
  ggtitle(TeX("$\\theta_1$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = paste0(xlab, "k"), breaks = 10^3 * xlab) +
  theme(legend.position = "bottom") #+

print(p1)
dev.off()

xlab = c(20, 50, 100)
ps.options(horizontal = F)
ps.options(height = 5, width = 4)
filename = here("Figure", "Addition_CP2_2.eps")
postscript(filename)
par(mar = c(2, 4, 2, 2))
p2 = ggplot(data = data,
            mapping = aes(
              x = n,
              y = CP2,
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
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15)
  ) +
  ggtitle(TeX("$\\theta_2$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = paste0(xlab, "k"), breaks = 10^3 * xlab) +
  theme(legend.position = "bottom") #+

print(p2)
dev.off()


xlab = c(20, 50, 100)
ps.options(horizontal = F)
ps.options(height = 5, width = 4)
filename = here("Figure", "Addition_CP2_3.eps")
postscript(filename)
par(mar = c(2, 4, 2, 2))
p3 = ggplot(data = data,
            mapping = aes(
              x = n,
              y = CP3,
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
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15)
  ) +
  ggtitle(TeX("$\\theta_3$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = paste0(xlab, "k"), breaks = 10^3 * xlab) +
  theme(legend.position = "bottom") #+

print(p3)
dev.off()
