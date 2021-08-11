library("dHSIC")
#library("ggplot2")
# Simulate data with dinosaur package
source("code/theme.R")

datasets = read.csv("./data/DatasaurusDozen.tsv", sep = "\t")
print(unique(datasets$dataset))
dat = datasets[datasets$dataset == "slant_down",]

# Measure correlation

# Measure HSIC
cr = cor(dat$x, dat$y)
hsic = dhsic.test(dat[c("x", "y")])
cr = cor.test(dat$x, dat$y)
cr
print(dhsic.test(dat[c("x", "y")]))
print(dhsic(dat[c("x", "y")]))
cr
hsic
label = sprintf("Independence Tests \n PCC:  p-value = %.3f \n HSIC: p-value = %.3f", cr$p.value, hsic$p.value)
# Plot data and print both measures
p = ggplot(dat) + geom_point(aes(x = x, y = y)) +
  geom_label(x = 85, y = 86, label = label) +
  scale_x_continuous("Feature"~X[1]) +
  scale_y_continuous("Feature"~X[2])

pdf(file = "./figures/association.pdf", height = 3, width = 6)
print(p)
dev.off()
# Maybe alsoshow that PDP and PFI would extrapolate
