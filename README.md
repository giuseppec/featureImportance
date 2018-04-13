---
output: github_document
---



[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/featureImportance)](http://cran.r-project.org/web/packages/featureImportance)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/featureImportance)](http://cran.rstudio.com/web/packages/featureImportance/index.html)
[![Build Status](https://travis-ci.com/giuseppec/featureImportance.svg?token=P4o4Hs3rFaP4ygx5oTzm&branch=master)](https://travis-ci.com/giuseppec/featureImportance)
[![codecov](https://codecov.io/gh/giuseppec/featureImportance/branch/master/graph/badge.svg?token=2w8ISxXGMc)](https://codecov.io/gh/giuseppec/featureImportance)

# featureImportance: An R package to assess the importance of features for any machine learning model

Add general info

# Installation of the package

Install the development version from GitHub (using `devtools`)


```r
install.packages("devtools")
devtools::install_github("giuseppec/featureImportance")
```

# Usecase


```r
library(mlr)
library(mlbench)
set.seed(2018)

# Look at the data
data(PimaIndiansDiabetes, package = "mlbench")
str(PimaIndiansDiabetes)
```

```
## 'data.frame':	768 obs. of  9 variables:
##  $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
##  $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
##  $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
##  $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
##  $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
##  $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
##  $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
##  $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
##  $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...
```

```r
# Make classification task from data
pid.task = makeClassifTask(data = PimaIndiansDiabetes, target = "diabetes")
pid.task
```

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
```

```r
# Choose machine learning algorithm 
lrn = makeLearner("classif.randomForest", ntree = 100)
lrn
```

```
## Learner classif.randomForest from package randomForest
## Type: classif
## Name: Random Forest; Short name: rf
## Class: classif.randomForest
## Properties: twoclass,multiclass,numerics,factors,ordered,prob,class.weights,oobpreds,featimp
## Predict-Type: response
## Hyperparameters: ntree=100
```

```r
# Create indices for train and test data
n = getTaskSize(pid.task)
train.ind = sample(n, size = 0.6*n)
test.ind = setdiff(1:n, train.ind)

# Fit model on train data
mod = train(lrn, pid.task, subset = train.ind)

# Measure feature importance on test data
test = getTaskData(pid.task, subset = train.ind)
featureImportance(mod, data = test)
```

```
## Object of class 'featureImportance'
## Aggregated importance:
##    features       mmce
## 1: pregnant 0.03182609
## 2:  glucose 0.19434783
## 3: pressure 0.02930435
## 4:  triceps 0.01752174
## 5:  insulin 0.02221739
## 6:     mass 0.10169565
## 7: pedigree 0.06773913
## 8:      age 0.06682609
```




