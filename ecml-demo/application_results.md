Simulations and Application
================

# Introduction

In this short tutorial, we provide the code that reproduces the results
of the application section of our article entitled “Visualizing the
Feature Importance for Black Box Models”. We used
[`batchtools`](https://github.com/mllg/batchtools) to run our
experiments. The files
[`application_pi_simulation.R`](https://github.com/giuseppec/featureImportance/blob/master/ecml-demo/application_pi_simulation.R),
[`application_shapley_simulation.R`](https://github.com/giuseppec/featureImportance/blob/master/ecml-demo/application_shapley_simulation.R)
and
[`application_importance_realdata.R`](https://github.com/giuseppec/featureImportance/blob/master/ecml-demo/application_importance_realdata.R)
contain the `batchtools` code to reproduce the expermients and can be
found in this
[directory](https://github.com/giuseppec/featureImportance/tree/master/ecml-demo).
The directory also includes the results of both files in an `.Rds` file
which is used in the code below to produce the figures and tables.

# Load Packages

``` r
# load required packages
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(xtable)
library(BBmisc)
library(knitr)
library(mlr)
source("helper/functions.R")
```

# Simulation

## PI and ICI Plots

``` r
res = readRDS("application_pi_simulation.Rds")

imp.global = rbindlist(lapply(res, function(x) {
  getImpTable(x, mid = "mse", sort = FALSE)
}))

imp1 = rbindlist(lapply(res, function(x) {
  remove0 = unique(x$replace.id[x$features == "V3" & x$feature.value == 0])
  getImpTable(x, subset.ind = remove0, mid = "mse", sort = FALSE)
}))

imp0 = rbindlist(lapply(res, function(x) {
  remove1 = unique(x$replace.id[x$features == "V3" & x$feature.value == 1])
  getImpTable(x, subset.ind = remove1, mid = "mse", sort = FALSE)
}))

tab = rbind(
  imp.global[, lapply(.SD, pasteMeanSd)],
  imp0[, lapply(.SD, pasteMeanSd)],
  imp1[, lapply(.SD, pasteMeanSd)]
)
kable(tab)
```

| V1             | V2             | V3           |
| :------------- | :------------- | :----------- |
| 77.98 (14.15)  | 76.76 (13.89)  | 62.76 (7.82) |
| 152.49 (26.06) | 1.43 (1.32)    | 0            |
| 1.26 (1.03)    | 151.49 (24.69) | 0            |

``` r
mid = "mse"
pfi = res[[1]]

# Get index of observations for V3 = 0 and V3 = 1
bin0 = subset(pfi, features == "V3" & feature.value == 0)
bin0 = unique(bin0$replace.id)
bin1 = subset(pfi, features == "V3" & feature.value == 1)
bin1 = unique(bin1$replace.id)

var = "V1"
pi = conditionalPFI(pfi, var, bin0, bin1, group.var = "V3")
ici = subset(pfi, features == var)
ici$V3 = as.factor(as.numeric(ici$row.id %in% bin1))

iciV1 = plotPartialImportance(pfi, feat = var,
  mid = mid, individual = FALSE, hline = FALSE, rug = FALSE) +
  xlab(var)
iciV1 = iciV1 + 
  geom_line(data = pi, aes_string(x = "feature.value", y = mid, color = "V3")) +
  geom_point(data = pi, aes_string(x = "feature.value", y = mid, color = "V3")) +
  labs(y = bquote(Delta~L ~ "based on" ~ .(toupper(mid))), x = bquote(X[1]), color = bquote(X[3])) +
  ylim(c(-10, 300))

var = "V2"
pi = conditionalPFI(pfi, var, bin0, bin1, group.var = "V3")
ici = subset(pfi, features == var)
ici$V3 = as.factor(as.numeric(ici$row.id %in% bin1))

iciV2 = plotPartialImportance(pfi, feat = var,
  mid = mid, individual = FALSE, hline = FALSE, rug = FALSE) +
  xlab(var)

iciV2 = iciV2 + 
  geom_line(data = pi, aes_string(x = "feature.value", y = mid, color = "V3")) +
  geom_point(data = pi, aes_string(x = "feature.value", y = mid, color = "V3")) +
  labs(y = bquote(Delta~L ~ "based on" ~ .(toupper(mid))), x = bquote(X[2]), color = bquote(X[3])) +
  ylim(c(-10, 300))

gridExtra::grid.arrange(iciV1, iciV2, nrow = 1)
```

![PI curves of \(X_1\) and \(X_2\) calculated using all observations
(black line) and conditional on \(X_3 = 0\) (red line) and \(X_3 = 1\)
(green line). The points plotted on the lines refer to the observed
feature values that were used as grid points to produce the
corresponding PI
curves.](application_results_files/figure-gfm/conditional-1.png)

## Shapley Feature Importance

``` r
### Plot Example
res.ex = readRDS("application_shapley_simulation.Rds")
# select one single run of the simulation with 500 repetitions to plot the example
res.ex = subset(res.ex, repl == 2 & method %in% c("shapley", "geP"), select = c("learner", "feature", "mse", "method"))
# specify colour and legend text
feat.order = c("V3", "V2", "V1", "geP")
col = c(gray.colors(3, start = 0.5, end = .9), hcl(h = 195, l = 65, c = 100))
col = setNames(col, feat.order)
legend.lab = c(
  "V1" = bquote(phi[1]),
  "V2" = bquote(phi[2]),
  "V3" = bquote(phi[3]),
  "geP" = bquote(widehat(GE)[P])
)
# change sign
res.ex[, mse := ifelse(method != "geP", -mse, mse)]
# reorder features for plotting
res.ex$feature = factor(res.ex$feature, levels = feat.order)
# shorten string for learner
res.ex$learner = factor(gsub("regr.", "", res.ex$learner))
# add column containing proportion of explained importance
res.ex[, perc := ifelse(feature == "geP", NA, mse[feature != "geP"]/sum(mse[feature != "geP"])*100), by = c("learner")]
# add column containing drop in MSE + proportion of explained importance
res.ex[, label := ifelse(feature == "geP", round(mse, 2), paste0(round(mse, 2), " (", round(perc, 0), "%)"))]

plot.ex = ggplot(res.ex, aes(x = learner, y = mse, fill = feature))
plot.ex = plot.ex + geom_bar(stat = "identity", colour = "white", pos = "stack") + coord_flip()
plot.ex = plot.ex + geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5)
plot.ex = plot.ex +
  ylab("performance (MSE)") +
  xlab("") +
  ggtitle("(a) Comparing the model performance and SFIMP values across different models") +
  scale_fill_manual(values = col, name = " performance \n explained by", labels = legend.lab) +
  theme_minimal()



### Plot Simulation Results
res.cleanup = readRDS("application_shapley_simulation.Rds")
# compute ratio of the importance values w.r.t feature "V3"
res.cleanup[, ratio := mse/mse[feature == "V3"], by = c("method", "learner", "repl")]
res.cleanup = subset(res.cleanup, method %in% c("pfi.diff", "pfi.ratio", "shapley") & feature != "V3")

lab.lrn = function(lrn) gsub("regr.", "", lrn)
new.names = setNames(expression(X[1] / X[3], X[2] / X[3]), c("V1", "V2"))

pp = ggplot(data = res.cleanup, aes(x = feature, y = ratio))
pp = pp + geom_boxplot(aes(fill = method), lwd = 0.25, outlier.size = 0.75)
pp = pp + facet_grid(. ~ learner, scales = "free", labeller = labeller(learner = lab.lrn))
pp = pp +
  scale_fill_discrete(name = "method",  labels = c("PFI (Diff.)", "PFI (Ratio)", "SFIMP"))  +
  scale_fill_grey(start = 0.4, end = .95) +
  theme_minimal()
pp = pp + scale_x_discrete(labels = new.names) +
  ylab("Value of the ratio") + xlab("Features involved to compute the ratio") +
  ggtitle("(b) Simulation with 500 repetitions")
grid.arrange(plot.ex, pp, heights = c(1.5, 2.5))
```

![Panel (a) shows the results of a single run, consisting of sampling
test data and computing the importance on the previously fitted models.
The first numbers on the left refer to the model performance (MSE) using
all features. The other numbers are the SFIMP values which sum up to the
total explainable performance. The percentages refer to the proportion
of explained importance. Panel (b) shows the results of 500 repetitions
of the experiment. The plots display the distribution of ratios of the
importance values for \(X_1\) and \(X_2\) with respect to \(X_3\)
computed by SFIMP, by the difference-based PFI and by the ratio-based
PFI.](application_results_files/figure-gfm/shapley-1.png)

# Application on Real Data

## Produce Table

``` r
pfi = readRDS("application_importance_realdata.Rds")
mid = "mse"

# get index for LSTAT > 10 in order to remove those observations
pi.ind = unique(pfi$replace.id[pfi$features == "LSTAT" & pfi$feature.value > 10])
# compute integral of each ICI curve and select observations with negative ICI integral
ici = subset(pfi, features == "LSTAT")
ici.integral = ici[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = "row.id"]
ici.ind = which(ici.integral[[mid]] <= 0)

# produce table
imp = getImpTable(pfi)
imp.pi = getImpTable(pfi, pi.ind)
imp.ici = getImpTable(pfi, ici.ind)
kable(rbind(imp, imp.pi, imp.ici))
```

| LSTAT |   RM | NOX | DIS | CRIM | PTRATIO | AGE | INDUS | TAX | RAD |   B |  ZN | CHAS |
| ----: | ---: | --: | --: | ---: | ------: | --: | ----: | --: | --: | --: | --: | ---: |
|  32.0 | 15.6 | 3.9 | 2.7 |  2.6 |     2.2 | 1.2 |   1.0 | 1.0 | 0.8 | 0.8 | 0.1 |  0.1 |
|  10.4 | 29.6 | 1.5 | 3.3 |  0.8 |     2.3 | 0.8 |   0.5 | 1.2 | 1.1 | 0.6 | 0.2 |  0.2 |
|  35.3 | 17.0 | 4.3 | 2.4 |  2.5 |     2.5 | 1.1 |   1.2 | 0.8 | 0.9 | 0.8 | 0.1 |  0.1 |

## Produce PI and ICI plots

``` r
mid = "mse"
features = c("LSTAT", "RM")

pp = lapply(features, function(feat) {
  ici = subset(pfi, features == feat)
  ici.integral = ici[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = "row.id"]
  ind = c(which.min(ici.integral[[mid]]), which.max(ici.integral[[mid]]))
  ici.obs = subset(ici, row.id %in% ici.integral$row.id[ind])

  # PI plot
  pi.plot = plotPartialImportance(pfi, feat, mid, rug = FALSE)
  pi.plot = pi.plot +
      labs(title = "PI plot", y = bquote(Delta~L ~ "based on" ~ .(toupper(mid)))) +
      xlab(feat) +
      theme(legend.position = "none", title = element_text(size = 8), axis.title = element_text(size = 8))
  
  # ICI plot
  ici.plot = plotPartialImportance(pfi, feat, mid,
    individual = TRUE, grid.points = FALSE, rug = FALSE, hline = FALSE)
  ici.plot = ici.plot +
    geom_line(data = ici.obs, aes_string(x = "feature.value", y = mid, color = "factor(row.id)", group = "row.id")) +
    labs(title = "ICI plot", y = bquote(Delta~L ~ "based on" ~ .(toupper(mid)))) +
    xlab(feat) +
    theme(legend.position = "none", title = element_text(size = 8), axis.title = element_text(size = 8))
  
  list(
    ggMarginal(pi.plot, type = "histogram", fill = "transparent", margins = "x"),
    ici.plot
    #ggMarginal(ici.plot, type = "histogram", fill = "transparent", margins = "y")
  )
})
pp = unlist(pp, recursive = FALSE)

do.call(grid.arrange, pp[c(1,3,2,4)])
```

![PI and ICI plots for a random forest and the two most important
features of the Boston housing data (LSTAT and RM). The horizontal lines
in the PI plots represent the value of the global PFI (i.e. the integral
of the PI curve). Marginal distribution histograms for features are
added to the PI and ICI plot margins. The ICI curve with the largest
integral is highlighted in green and the curve with the smallest
integral in red.](application_results_files/figure-gfm/piplot-1.png)
