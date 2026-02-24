
# funcle

<!-- badges: start -->
<!-- badges: end -->

With funcle you can create powerful kernel-based classification ensembles using functional data. The package is based on the paper [Classification ensembles for multivariate functional data with application to mouse movements in web surveys](https://arxiv.org/abs/2205.13380).

## Installation

You can install the development version of funcle from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("twistuba-hu/funcle")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(funcle)

dataApp4 <- read.csv("dataApp4.csv", header = TRUE, sep = ",")
dataApp4
as.factor(dataApp4$condition)
ncol(dataApp4)
dataApp4[, -ncol(dataApp4)] <- apply(dataApp4[, -ncol(dataApp4)], 2, as.numeric)
head(dataApp4)
dataApp4[, ncol(dataApp4)] <- as.factor(dataApp4[, ncol(dataApp4)])
head(dataApp4)
classes <- dataApp4[, ncol(dataApp4)]
classes

fdata <- dataApp4[, -ncol(dataApp4)]
d <- unique(fdata[, "dim"])
fdata <- t(fdata[, -which(names(fdata) %in% c("id", "dim"))])
fdata <- plyr::alply(
    cbind(1:d),
    1,
    function(i) {
        data.frame(dim = rep(1:d, each = nrow(fdata) / d), fdata)[
            data.frame(dim = rep(1:d, each = nrow(fdata) / d), fdata)[
                ,
                "dim"
            ] ==
                i,
            -1
        ]
    }
)
fdata <- lapply(fdata, as.matrix)


cl <- "classif.mclassiKernel"
task <- makeClassifTask(data = dataApp4, target = "condition")
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

