
# featureImportance: Model-agnostic permutation feature importance with the [`mlr`](https://github.com/mlr-org/mlr) package

[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/featureImportance)](http://cran.r-project.org/web/packages/featureImportance)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/featureImportance)](http://cran.rstudio.com/web/packages/featureImportance/index.html)
[![Build
Status](https://travis-ci.org/giuseppec/featureImportance.svg?branch=master)](https://travis-ci.org/giuseppec/featureImportance)
[![codecov](https://codecov.io/gh/giuseppec/featureImportance/branch/master/graph/badge.svg?token=2w8ISxXGMc)](https://codecov.io/gh/giuseppec/featureImportance)

## Results of the article [“Visualizing the Feature Importance for Black Box Models”](https://arxiv.org/abs/1804.06620)

This R package was developed as a part of the article [“Visualizing the
Feature Importance for Black Box
Models”](https://arxiv.org/abs/1804.06620) accepted at the ECML-PKDD
2018 conference track. The results of the application section of this
article can be reproduced with the code provided
[here](https://github.com/giuseppec/featureImportance/blob/master/ecml-demo/application_results.md).

## Installation of the package

Install the development version from GitHub (using `devtools`)

``` r
install.packages("devtools")
devtools::install_github("giuseppec/featureImportance")
```

## Introduction

The `featureImportance` package is an extension for the
[`mlr`](https://github.com/mlr-org/mlr) package and allows to compute
the permutation feature importance in a model-agnostic manner. The focus
is on performance-based feature importance measures:

  - **Model reliance** and **algorithm reliance**, which is a
    model-agnostic version of [breiman’s permutation
    importance](https://www.stat.berkeley.edu/~breiman/randomforest2001.pdf)
    introduced in the article
    [arXiv:1801.01489v3](https://arxiv.org/abs/1801.01489).
  - **SFIMP** (Shapley Feature Importance)
  - PIMP

## Simple usecase

``` r
library(mlr)
library(mlbench)
library(featureImportance)
set.seed(2018)

# Look at the data
data(PimaIndiansDiabetes, package = "mlbench")
str(PimaIndiansDiabetes)
```

    ## 'data.frame':    768 obs. of  9 variables:
    ##  $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
    ##  $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
    ##  $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
    ##  $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
    ##  $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
    ##  $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
    ##  $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
    ##  $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
    ##  $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...

``` r
# Make mlr classification task from data
pid.task = makeClassifTask(data = PimaIndiansDiabetes, target = "diabetes")
pid.task
```

    ## Supervised task: PimaIndiansDiabetes
    ## Type: classif
    ## Target: diabetes
    ## Observations: 768
    ## Features:
    ##    numerics     factors     ordered functionals 
    ##           8           0           0           0 
    ## Missings: FALSE
    ## Has weights: FALSE
    ## Has blocking: FALSE
    ## Has coordinates: FALSE
    ## Classes: 2
    ## neg pos 
    ## 500 268 
    ## Positive class: neg

``` r
# Choose machine learning algorithm 
lrn = makeLearner("classif.randomForest", ntree = 100)

# Create indices for train and test data
n = getTaskSize(pid.task)
train.ind = sample(n, size = 0.6*n)
test.ind = setdiff(1:n, train.ind)

# Fit model on train data
mod = train(lrn, pid.task, subset = train.ind)

# Measure feature importance on test data
test = getTaskData(pid.task, subset = train.ind)
imp = featureImportance(mod, data = test)
imp
```

    ## $importance
    ##      n.feat.perm features       mmce
    ##   1:           1 pregnant 0.02608696
    ##   2:           1  glucose 0.20869565
    ##   3:           1 pressure 0.01304348
    ##   4:           1  triceps 0.01956522
    ##   5:           1  insulin 0.04565217
    ##  ---                                
    ## 396:          50  triceps 0.02608696
    ## 397:          50  insulin 0.02608696
    ## 398:          50     mass 0.08260870
    ## 399:          50 pedigree 0.07391304
    ## 400:          50      age 0.06521739
    ## 
    ## $resample
    ## NULL
    ## 
    ## $measures
    ## $measures$mmce
    ## Name: Mean misclassification error
    ## Performance measure: mmce
    ## Properties: classif,classif.multi,req.pred,req.truth
    ## Minimize: TRUE
    ## Best: 0; Worst: 1
    ## Aggregated by: test.mean
    ## Arguments: 
    ## Note: Defined as: mean(response != truth)
    ## 
    ## 
    ## attr(,"class")
    ## [1] "featureImportance"
