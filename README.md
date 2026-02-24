
# funcle

<!-- badges: start -->
<!-- badges: end -->

With funcle you can create powerful kernel-based classification ensembles using functional data. The package is based on the paper [Classification ensembles for multivariate functional data with application to mouse movements in web surveys](https://arxiv.org/abs/2205.13380).

## Installation

You can install the development version of funcle from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("twistuba-hu/funcle")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(funcle)
library(mlr)

dataApp4 <- kuric_minigame
cl <- "classif.mclassiKernel"
task <- makeClassifTask(data = dataApp4, target = "sex")
id <- c("L1", "globMin", "L2")
knn <- NULL # list(c(1:2), c(1:2), c(1:2))
kernel <- "Ker.norm"
gridsearch.vals <- list(c(100, 500, 2000), c(2, 3, 4, 5, 6))
super.learner <- "randomForest"
par.vals <- list(
  list(metric = "L1", h = 5, predict.type = "prob"),
  list(metric = "globMin", h = 5, predict.type = "prob"),
  list(metric = "L2", h = 5, predict.type = "prob")
)

weight <- c(0.70, 0.30)

cv <- 10
asupremum3 <- mclassiOnested(
  cl = cl,
  task = task,
  knn = knn,
  kernel = kernel,
  id = id,
  par.vals = par.vals,
  hyperparams.vals = gridsearch.vals,
  super.learner = super.learner,
  cv = cv,
  M = 100,
  weight = weight
)

asupremum3
```

