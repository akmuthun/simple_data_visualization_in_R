
# The Old Faithful geyser data 
d <- density(faithful$eruptions, bw = "sj")
d 
plot(d)

y <- subset(x=iris, Species=="virginica")[, 1:4] 
mu <- colMeans(y)
Sigma <- cov(y)

mu

Sigma

rmvn.Choleski <- function(n, mu, Sigma) { 
  # generate n random vectors from MVN(mu, Sigma) # dimension is inferred from mu and Sigma 
  d <- length(mu) 
  Q <- chol(Sigma) # Choleski factorization of Sigma (Q is the upper triangular matrix of choleski dico)
  Z <- matrix(rnorm(n*d), nrow=n, ncol=d) 
  X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE) 
  X }

#now generate MVN data with this mean and covariance 
X <- rmvn.Choleski(200, mu, Sigma) 

dim(X)

pairs(X,  panel = panel.smooth)

dim(volcano)

#contour plot with labels 
contour(volcano, asp = 0.5, labcex = 2)

#another version from lattice package ]
library(lattice) 
contourplot(volcano) #similar to above

install.packages("rgl")

library(rgl)

## volcano  ## 87 x 61 matrix
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))

library(MASS) 
library(lattice) 
#trellis.device(color = FALSE) #black and white display 
x <- iris[seq(1, 150, 5), ] #get every fifth obs. 
parallelplot(~x[1:4] | Species, x)
