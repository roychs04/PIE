## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(comment="..",warning=FALSE,
                      message=FALSE)
options(digits=2)


## ------------------------------------------------------------------------
# required libraries
library(clinpredict)


library(ggplot2)
library(survival)
library(tidyverse)
library(mvtnorm)
library(survminer)

options(digits=2)

# Simulation sample size for example
# 1000 sample is too small: default is 10000
n.pred = 2000


## ---- eval=TRUE----------------------------------------------------------

# Example data file
data("CM141.r")
ds = CM141

ds = data(CM141)
ds = CM141


C.label = "standard of care"
T.label = "nivolumab"

yC.cond = 1:15
gain.cutoffs = -1:12

gain.label = "survival gain in months (nivolumab-standard)"

out1 = pie_surv(
  data = ds,
  n.pred = n.pred,
  yC.cond = yC.cond,
  pie.gain.cutoffs = gain.cutoffs,
  time.unit = "months",
  gain.label = gain.label,
  C.label = C.label,
  T.label = T.label
)




## ---- eval=TRUE----------------------------------------------------------
# output object has the following elements...
names(out1)

# 1) main results
out1$pie
out1$pie.gain

# conditional pie (for fixed value control times)
out1$pie.cond


## ---- eval=TRUE----------------------------------------------------------
# interval boundaries
out1$int
# Cox and log-rank results
out1$coxPH
out1$logrank

# summaries  of marginal predictive distributions
out1$marg
 
# results from pwexp analysis for control and treatment 
# out1$pwexp

# simulated predictive values
head(out1$sim)



## ---- eval=TRUE----------------------------------------------------------

# knitr tables: out1$tables_knitr
names(out1$tables_knitr)

out1$tables_knitr$marg
out1$tables_knitr$pie
out1$tables_knitr$pie.gain

# ggplot figures: out1$figures
names(out1$figures)

fig1.km = out1$figures$km
fig1.km.pwexp = out1$figures$km.pwexp
fig1.gain = out1$figures$pie.gain
fig1.cond = out1$figures$pie.cond

pdf("appfig1km.pdf",onefile=FALSE)
fig1.km
dev.off()

pdf("appfig1kmpwexp.pdf",onefile=FALSE)
fig1.km.pwexp
dev.off()

pdf("appfig1gain.pdf",onefile=FALSE)
fig1.gain
dev.off()

pdf("appfig1cond.pdf",onefile=FALSE)
fig1.cond
dev.off()



## ---- eval=TRUE----------------------------------------------------------
data(CM057)
ds = CM057

# biomarker: group variable

# biomarker-negative
ds0 = ds[ds$group == 0, ]
# biomarker-positive
ds1 = ds[ds$group == 1, ]

C.label = "docetaxel"
T.label = "nivolumab"

gain.cutoffs = seq(-3, 24, by = 3)
gain.label = "survival gain in months (nivolumab-docetaxel)"

out2.0 = pie_surv(
  ds0,
  n.pred = n.pred,
  C.label = C.label,
  T.label = T.label,
  gain.label = gain.label,
  pie.gain.cutoffs = gain.cutoffs
)

out2.1 = pie_surv(
  ds1,
  n.pred = n.pred,
  C.label = C.label,
  T.label = T.label,
  gain.label = gain.label,
  pie.gain.cutoffs = gain.cutoffs
)

labels = c("biomarker-negative","biomarker-positive")

out.pie = rbind(out2.0$pie,out2.1$pie)
rownames(out.pie) = labels

out.piegain = rbind(out2.0$pie.gain,out2.1$pie.gain)
rownames(out.piegain) = labels

cap = "Summary of predictive individual effect distribution"
knitr::kable(out.pie,cap=cap)

cap = "Probabilities of survival gains for nivolumab vs. docetaxel"
knitr::kable(rbind(out.piegain),cap=cap)


fig20.km = out2.0$figures$km
fig21.km = out2.1$figures$km

fig20.gain = out2.0$figures$pie.gain
fig21.gain = out2.1$figures$pie.gain


dev.off()

pdf("appfig20km.pdf",onefile=FALSE)
fig20.km
dev.off()

pdf("appfig21km.pdf",onefile=FALSE)
fig21.km
dev.off()


pdf("appfig20gain.pdf",onefile=FALSE)
fig20.gain
dev.off()

pdf("appfig21gain.pdf",onefile=FALSE)
fig21.gain
dev.off()



## ------------------------------------------------------------------------
sessionInfo()

