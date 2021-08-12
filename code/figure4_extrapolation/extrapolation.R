source("code/theme.R")


set.seed(12345678)
n = 100
ngrid = 11
x1 = rexp(n, rate = 1)
x2 = x1 + rnorm(n, mean(x1), sd = 0.5*sd(x1)) #rexp(1000, rate = 100)
#x2 = sqrt(x1)*abs(x2)
plot(density(x1))
plot(x1, x2)

par(mfrow = c(1, 3), mar = c(4,4,2,0))
# grid
grid.x1 = seq(min(x1), max(x1), length = ngrid+1)
grid.x2 = seq(min(x2), max(x2), length = ngrid+1)
data1 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data1$method = "equidistant grid"
plot(data1$x1, data1$x2, pch = 4, main = "equidistant grid", xlim = range(x1), ylim = range(x2), col = "#000000A0")
points(x1, x2, col = "#FF000040", pch = 19)

# subsample
grid.x1 = sample(x1, size = ngrid+1)
grid.x2 = sample(x2, size = ngrid+1)
data2 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data2$method = "sub-sampled grid"
plot(data2$x1, data2$x2, pch = 4, main = "sub-sampled grid", xlim = range(x1), ylim = range(x2), col = "#000000A0")
points(x1, x2, col = "#FF000040", pch = 19)

# quantile
grid.x1 = quantile(x1, 0:(ngrid)/(ngrid), type = 1)
grid.x2 = quantile(x2, 0:(ngrid)/(ngrid), type = 1)
data3 = expand.grid(x1 = grid.x1, x2 = grid.x2)
data3$method = "quantile grid"
plot(data3$x1, data3$x2, pch = 4, main = "quantile grid", xlim = range(x1), ylim = range(x2), col = "#000000A0")
points(x1, x2, col = "#FF000040", pch = 19)

data = rbind(data1, data2, data3)
data$method = factor(data$method, levels = c("equidistant grid", "sub-sampled grid", "quantile grid"))

p = ggplot(data = data, aes(x1, x2)) +
  geom_point(data = data.frame(x1 = x1, x2 = x2), aes(x1, x2), alpha = 0.5) +
  facet_grid(~ method) +
  geom_point(shape = 4, alpha = 0.5, col = "red") +
  xlab("Feature"~X[1]) +
  ylab("Feature"~X[2])

pdf(file = "./figures/sampling.pdf", height = 3, width = 9)
print(p)
dev.off()
