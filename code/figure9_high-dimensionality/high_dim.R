##########################
#--- PIMP -----
##########################
library("vita") # PIMP
library("randomForest")
library("ggplot2")

# Setup
# n = 100, p = dims differ
nsim = 100
dims = c(2, 5, 10, 15, 30, 50, 100, 250, 500, 750, 1000)

get_pval <- function(dim) {
  print(dim)
  # Create training data
  X.train = replicate(dim, rnorm(nsim))
  X.train = data.frame(X.train)
  y.train = with(X.train, 2*X1 + 2*X2^2 + rnorm(nsim))
  
  # Train rf on training data
  reg.rf = randomForest(X.train, y.train, ntree = 500, importance = TRUE)
  
  # Create test data 
  X.test = replicate(dim, rnorm(nsim))
  X.test = data.frame(X.test)
  y.test = with(X.test, 2*X1 + 2*X2^2 + rnorm(nsim))
  
  # Compute PIMP on test data
  pimp.varImp = PIMP(X.test, y.test, reg.rf, S = 100, parallel = TRUE, ncores=3)
  pval = PimpTest(pimp.varImp)$pvalue
  nr.sig = sum(pval < 0.05)
  nr.bonf = sum(pval < 0.05/dim)
  
  return(c(dim = dim, nr.sig = nr.sig, nr.bonf = nr.bonf, 
    x1.p = pval[1], x2.p = pval[2]))
}

# Run above's function for each p
set.seed(1234L)
res = sapply(dims, get_pval)

# Create plot 
df = data.frame(t(res)) 
dfmelt = reshape(df, direction = "long", varying = c("nr.sig", "nr.bonf"),  v.names = "nr", timevar = "Correction", 
  times = c("No", "Yes"))
dfmelt$Correction = factor(dfmelt$Correction, levels = c("No", "Yes"))

ggplot(dfmelt, 
  aes(x = dim, y = nr, colour = Correction)) + 
  geom_line() + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "blue")) +
  ylab("Number of significant features") +
  xlab("Number of features (p)") +
  theme_bw() + 
  scale_y_continuous(breaks = c(seq(5, 50, 10)))

# ggsave(filename = "bonferroni.pdf", height = 2.5, width = 5)  

