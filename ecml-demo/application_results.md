Application Results
================

Load Packages
=============

``` r
# load packages
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(xtable)
library(BBmisc)
library(knitr)
```

Produce Table
=============

``` r
pfi = readRDS("application_importance_realdata.Rds")
mid = "mse"

# function for recomputing feature importance after removing observations indexed by subset.ind
getImpTable = function(pfi, subset.ind = NULL, learner = "regr.randomForest") {
  if (!is.null(subset.ind))
    pfi = subset(pfi, row.id %nin% subset.ind & replace.id %nin% subset.ind)
  imp = pfi[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("mse"),
    by = c("features", "learner")]
  imp = split(imp, imp$learner)[[learner]]
  imp$mse = round(imp$mse, 1)
  imp = sortByCol(imp[, -"learner"], "mse", asc = FALSE)
  setColNames(as.data.frame(rbind(imp$mse)), imp$features)
}

# get index for LSTAT > 10 in order to remove those observations
pi.ind = unique(pfi$replace.id[pfi$features == "LSTAT" & pfi$feature.value > 10])
ici = subset(pfi, learner == "regr.randomForest" & features == "LSTAT")
ici.integral = ici[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = "row.id"]
ici.ind = which(ici.integral[[mid]] <= 0)

# produce table
imp = getImpTable(pfi)
imp.pi = getImpTable(pfi, pi.ind)
imp.ici = getImpTable(pfi, ici.ind)
kable(rbind(imp, imp.pi, imp.ici))
```

|  LSTAT|    RM|  NOX|  DIS|  CRIM|  PTRATIO|  AGE|  INDUS|  TAX|  RAD|    B|   ZN|  CHAS|
|------:|-----:|----:|----:|-----:|--------:|----:|------:|----:|----:|----:|----:|-----:|
|   32.0|  15.6|  3.9|  2.7|   2.6|      2.2|  1.2|    1.0|  1.0|  0.8|  0.8|  0.1|   0.1|
|   10.4|  29.6|  1.5|  3.3|   0.8|      2.3|  0.8|    0.5|  1.2|  1.1|  0.6|  0.2|   0.2|
|   35.3|  17.0|  4.3|  2.4|   2.5|      2.5|  1.1|    1.2|  0.8|  0.9|  0.8|  0.1|   0.1|

Produce PI and ICI plots
========================

``` r
plotPartialImportance = function(pfi, feat, learner.id, mid,
  marginal = FALSE, individual = FALSE, rug = TRUE, hline = TRUE, 
  grid.points = TRUE, subset.observation.index = NULL, subset.replaced.index = NULL) {
  d = copy(subset(pfi, learner == learner.id & features == feat))
  d$feature.value = as.numeric(as.character(d$feature.value))
  if (!is.null(subset.observation.index))
    d = subset(d, row.id %in% subset.observation.index)
  if (!is.null(subset.replaced.index))
    d = subset(d, replace.id %in% subset.replaced.index)

  pi = d[, lapply(.SD, mean, na.rm = TRUE),
    .SDcols = c(mid), by = c("replace.id", "features", "learner", "feature.value")]

  pp = ggplot(data = as.data.frame(pi), aes_string(x = "feature.value", y = mid))
  if (grid.points)
    pp = pp + geom_point()
  if (individual) {
    pp = pp + geom_point(shape = NA) + geom_line(data = as.data.frame(na.omit(d)),
      aes_string(x = "feature.value", y = mid, group = "row.id"), color = "gray")
  }
  pp = pp + geom_line(data = as.data.frame(pi), aes_string(x = "feature.value", y = mid))
  if (rug)
    pp = pp + geom_rug()
  if (hline)
    pp = pp + geom_hline(yintercept = mean(pi[[mid]]))
  if (marginal)
    ggMarginal(pp, type = "histogram", fill = "transparent") else
      pp
}

learner.id = "regr.randomForest"
mid = "mse"
features = c("LSTAT", "RM")

pp = lapply(features, function(feat) {
  ici = subset(pfi, learner == learner.id & features == feat)
  ici.integral = ici[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = "row.id"]
  ind = c(which.min(ici.integral[[mid]]), which.max(ici.integral[[mid]])) # unlist(ind[[feat]])
  ici.obs = subset(ici, row.id %in% ici.integral$row.id[ind])

  # PI plots
  pi.plot = plotPartialImportance(pfi, feat, learner.id, mid, rug = FALSE)
  pi.plot = pi.plot +
      labs(title = "PI plot", y = bquote(Delta~L ~ "based on" ~ .(toupper(mid)))) +
      xlab(feat) +
      theme(legend.position = "none", title = element_text(size = 8), axis.title = element_text(size = 8))
  
  # ICI plots
  ici.plot = plotPartialImportance(pfi, feat, learner.id, mid,
    individual = TRUE, grid.points = FALSE, rug = FALSE, hline = FALSE)
  ici.plot = ici.plot +
    geom_line(data = ici.obs, aes_string(x = "feature.value", y = mid,
      color = "factor(row.id)",
      group = "row.id")) +
    labs(title = "ICI plot", y = bquote(Delta~L ~ "based on" ~ .(toupper(mid)))) +
    xlab(feat) +
    theme(legend.position = "none", title = element_text(size = 8), axis.title = element_text(size = 8))
  
  list(
    pi.plot,
    ici.plot
  )
})
pp = unlist(pp, recursive = FALSE)

pp[c(1,3)] = lapply(pp[c(1,3)], function(p)
  ggMarginal(p, type = "histogram", fill = "transparent"))
pp[c(2,4)] = lapply(pp[c(2,4)], function(p)
  ggMarginal(p, type = "histogram", fill = "transparent", margins = "y"))
do.call(grid.arrange, pp[c(1,3,2,4)])
```

![PI and ICI plots for a random forest and the two most important features of the Boston housing data (LSTAT and RM). The horizontal lines in the PI plots represent the value of the global PFI (i.e. the integral of the PI curve). Marginal distribution histograms for features and partial importances are added in the PI and ICI plot margins, respectively. The ICI curve with the largest integral is highlighted in green and the curve with the smallest integral in red.](application_results_files/figure-markdown_github/piplot-1.png)

Produce Shapley Feature Importance Plots
========================================

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
res.cleanup = subset(res.cleanup, method %in% c("pfi.diff", "pfi.ratio", "shapley"))
# compute percentages for 3 features
res.cleanup[, ratio := mse/mse[feature == "V3"], by = c("method", "learner", "repl")]
res.cleanup = subset(res.cleanup, feature != "V3")

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

![Panel (a) shows the results of a single run, consisting of sampling test data and computing the importance on the previously fitted models. The first numbers on the left refer to the model performance (MSE) using all features. The other numbers are the SFIMP values which sum up to the total explainable performance. The percentages refer to the proportion of explained importance. Panel (b) shows the results of 500 repetitions of the experiment. The plots display the distribution of ratios of the importance values for *X*<sub>1</sub> and *X*<sub>2</sub> with respect to *X*<sub>3</sub> computed by SFIMP, by the difference-based PFI and by the ratio-based PFI.](application_results_files/figure-markdown_github/shapley-1.png)
