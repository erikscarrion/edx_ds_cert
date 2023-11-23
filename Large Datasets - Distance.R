## Large Datasets - Distance ====

library(tidyverse)
library(ggthemes)
library(ggrepel)
library(caret)
library(dslabs)
library(matrixStats)
library(MASS)

if(!exists("mnist")){mnist = read_mnist()}

## Distance ====
## 
## 2 Types of Distance: 
##  1. Euclidean Distance  :  d = sqrt((x1 - x2)^2 +(y1-y2)^2)
##  2. Manhattan Distance  :  d = |x1 - x2| + |y1 - y2|
##  3. Mahalanobis Distance:  d = D(p = point, Q = distribution)

## Predictor Space ====
##    - The set of all predictors that should be considered for the particular
##    challenge at hand
##    - Distance calculations are an essential component of the predictor space
##    
##    - Calculate the distance between all 784 predictors in the mnist trains set
##    by taking the distance of the transpose - dist(x) measures distance rowwise
##    
##    - This will result in a 784 x 784 matrix

# take a small sample first
set.seed(1999) # The Slim Shady LP is released
idx <- sample(which(mnist$train$labels %in% c(2,7)), 200)
x.small <- mnist$train$images[idx,]
y.small <- factor(mnist$train$labels[idx], levels = c(2,7), ordered = T)

start = Sys.time()
d.preds <- as.matrix(dist(t(x.small)))
print(Sys.time() - start)

## Visualize the distances
image(d.preds[order(y.small), order(y.small)])

## Preserving Distance ====
## 

n = 100
mu.adults = c(69, 69)
mu.teens = c(55,55)
Sigma = matrix(c(9, 9*.9, 9*.92, 9),2,2)

set.seed(1988, sample.kind = "Rounding") # Gabe is born
x = rbind(mvrnorm(n/2, mu.adults, Sigma),
          mvrnorm(n/2, mu.teens , Sigma))
## Create tibble to visualize
heights <-  
  tibble(X1 = x[,1],
         X2 = x[,2],
         Group = factor(rep(c("Adult", "Teen"), each = 50)))

line.df = data.frame(x = c(x[3,1], x[75,1]),
                     y = c(x[3,2], x[75,2]))

heights |>
  ggplot(aes(X1, X2)) + 
  geom_point(aes(color = Group)) +
  # add line of length d between a point in the 
  # adults group and a point in the teens group
  geom_line(data = line.df,
            aes(x, y), 
            col = "red") +
  labs(title = "Twin Heights - Scatterplot",
       caption = "Data: Simulated Multivariate Normal Data",
       x = "Height - Twin 1",
       y = "Height - Twin 2") +
  theme(plot.caption.position = "plot")


## Suppose we want to capture the majority of the information
## found in the above scatter plot, but instead of 2 dimensions
## we only want 1

## What are the original distances
dx = dist(x) |> as.matrix()

## Suppose we use just 1 dimension to calculate distances
z = x[,1]
dz = dist(z) |> as.matrix()

# Plot the actual vs. approximated distance and regress
# Z against on X
plot(dx, dz,
     main = "Approximate:Actual Distance")
abline(lm(dx ~ dz), col = "red", lwd = 2)

## Based on our approximation, we are underestimating the actual distance
## So we need to make a correction
## 
## Because we divide the sum of the squared differences by 2 we can divide
## all of X by sqrt(2) then calculate the distances to arrive at the corrected line

plot(dx/sqrt(2), dz,
     main = "Corrected Est. vs. Actual Distance")
abline(lm((dx/sqrt(2))~dz), col = "red", lwd = 2)

## Suppose we want to plot the differences against the average
z1 = cbind( ((z+x[,2])/ sqrt(2)), (z-x[,2])/sqrt(2))

plot(z1[,1], z1[,2])
abline(lm(z1[,2] ~ z1[,1])$coef, col = 'red')

summary(lm(z1[,2]~ z1[,1] ))

## based on the plot, we see that the majority of the information
## is found in the first column of the transformed matrix

dz1 <- dist(z1[,1])|> as.matrix()
plot(dx, dz1, 
     main = "Approx vs. Actual Distance", 
     sub = "Z Transformed to use Average",
     xlab = "D(x)",
     ylab = "D(mean height)")
abline(lm(dz1 ~ dist(x)), col = "red", lwd = 3)

# Using this transformed vector allows us to reduce the dimensionality
# And gain even greater precision


## Orthogonal Transformations ====
z2 = matrix(rep(0,200), 100,2)
z2[,1] <- (x[,1]+x[,2])/sqrt(2)
z2[,2] <- (x[,2]-x[,1]) /sqrt(2)

max(dist(x) - dist(z2))
sd(dist(x) - dist(z2[,1]))

## Z is the orthogonal rotation of X 

plot(z2[,1], z2[,2])
abline(lm(z2[,2] ~ z2[,1]), col = "red")

## Again, the majority of the information has been captured by the first dimension which can 
## be seen when we take the column std. devs.
colSds(z2)
colSds(x)

## Compared to the original feature space, which has near equal variances in both columns, 
## the transformed matrix has much, much higher variance along the first dimension. 
## 
## In this Case, z[,1] is the first principal component of X
colnames(z2) = c("Mean", "Diff")
as_tibble(z2) |> 
  ggplot(aes(Mean)) +
  # Histogram of the first dim of Z clearly shows 2 groups
  geom_histogram(color = "black", fill = "orange") +
  labs(title = "Histogram of Z",
       x = "Z", ylab = "count")

## Did the transformation affect total variability?

## We assume X is centered
sum(colMeans(x^2))
sum(colMeans(z2^2))

## As we can see, total variability has not been affected and yet we are able to reduce 
## the overall dimensionality. 

## MNIST - Principal Components
## 

idx = sample(1:nrow(mnist$train$images), 1000)
x = mnist$train$images[idx, ]
y = mnist$train$labels[idx]


# PCA with stats::prcomp()
pca.mnist = stats::prcomp(x)

# PCA with ade4::dudi.pca()
pca.mnist.ade = ade4::dudi.pca(x)

# Principal Components are the new design matrix
d.mat = pca.mnist.ade$li
d.mat$y = factor(y)

# Use the principal components to predict the label using various procedures

## Logistic Regression
log.reg   = glm(y ~ ., family = binomial(link = "probit"), data = d.mat)
log.reg.1 = glm(y ~ ., family = binomial(link = "logit"),  data = d.mat)


## Take a random 500 unit sample to test your model
space = 1:nrow(mnist$train$images)
space = space[-idx]
set.seed(1983, sample.kind = "Rounding")
idx   = sample(space, 500)

x.test = mnist$train$images[idx, ]
y.test = mnist$train$labels[idx]

## grab the principal components
x.test.pca = ade4::dudi.pca(x.test,scannf = F, nf = 15)
x.test.comps = x.test.pca$li

log.reg.preds = predict(log.reg, newdata = x.test.comps, type = "response")
log.reg.preds.2 = predict(log.reg.1, newdata = x.test.comps)

preds = 
  tibble(response = y.test,
         preds.link = log.reg.preds.2,
         preds.response = predict(log.reg.1, x.test.comps, type = "response"),
         preds.terms  = predict(log.reg.1, x.test.comps, type = "terms"))
head(preds)
predict(log.reg.1)


##
### Exercises ====
## 

## Exercises: 33.4====

data("tissue_gene_expression")


x.gene <- tissue_gene_expression$x
y.gene <- tissue_gene_expression$y


# compute the distance between each observation:
d.observations.gene <- dist(x.gene)
d <- as.matrix(d.observations.gene)

# Compare the distance between the 39th and 40th
# and the distance between the 73rd and 74th
knitr::kable(
  tibble(D = c("D(1,2)" , "D(39,40)", "D(73,74)",
               "D(1,39)", "D(2,40)" , "D(1,74)"),
         Distance = c(round(d[1,2], 2), round(d[39,40], 2), round(d[73,74], 2),
                      round(d[1,39], 2), round(d[2,40], 2), round(d[1,74], 2)),
         `Tissue 1` = c(y.gene[1], y.gene[39], y.gene[73],
                        y.gene[1], y.gene[2], y.gene[1]), 
         `Tissue 2` = c(y.gene[2], y.gene[40], y.gene[74],
                        y.gene[39], y.gene[40], y.gene[74]))
)

image(1:190, 1:190, d[order(y.gene), order(y.gene)],
      main = "Distance b/t Observations",
      sub = "Data: Tissue Gene Expression",
      xlab = "", ylab = "")