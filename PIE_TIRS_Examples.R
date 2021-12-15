rm(list=ls())
library(clinpredict)
library(survival)
library(ggplot2)
library(tidyverse)
library(survminer)

options(digits = 3)

set.seed(20211206)
# very large number of simulations for final analysis
# n.pred = 1000
n.pred = 50000

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
  # C.col = C.col,
  # T.col = T.col,
  C.label = C.label,
  T.label = T.label
)

out1$figures$pie.cond = 
  plot_estlowup(x=yC.cond,
               est = out1$pie.cond[,4],
               low = out1$pie.cond[,3],
               up = out1$pie.cond[,5],
               xlim=c(0,16),xbreaks=seq(0,16,by=4),
               ylim=c(-3,21),ybreaks=seq(-3,21,by=3),
               ylab=gain.label,
               xlab="standard of care survival time (months)"
  )


out1$figures$pie.cond
out1$figures$pie.gain

dput(out1, "revised-ex1-out.dmp")


ds = data(IM)
ds = IM

yC.cond = seq(2,24,by=2)
gain.cutoffs = -1:12

C.label = "chemotherapy"
T.label = "atezolizumab"

gain.label = "survival gain in months (atezolizumab-chemo)"
out2 = pie_surv(
  data = ds,
  n.pred = n.pred,
  yC.cond = yC.cond,
  pie.gain.cutoffs = gain.cutoffs,
  time.unit = "months",
  gain.label = gain.label,
  # C.col = C.col,
  # T.col = T.col,
  C.label = C.label,
  T.label = T.label
)


out2$figures$pie.cond = 
  plot_estlowup(x=yC.cond,
               est = out2$pie.cond[,4],
               low = out2$pie.cond[,3],
               up = out2$pie.cond[,5],
               xlim=c(0,25),xbreaks=seq(0,25,by=5),
               ylim=c(-3,18),ybreaks=seq(-3,18,by=3),
               ylab=gain.label,
               xlab="chemotherapy survival time (months)"
  )


out2$figures$pie.cond
out2$figures$pie.gain

dput(out2, "revised-ex2-out.dmp")

ds = data(PA3)
ds = PA3
head(ds)

yC.cond = seq(2,20,by=2)
gain.cutoffs = seq(-1, 3, by = 0.5)

C.label = "gemcitabine"
T.label = "gemcitabine+erlotinib"

gain.label = "survival gain in months (combo-mono)"
out3 = pie_surv(
  data = ds,
  n.pred = n.pred,
  yC.cond = yC.cond,
  pie.gain.cutoffs = gain.cutoffs,
  time.unit = "months",
  gain.label = gain.label,
  # C.col = C.col,
  # T.col = T.col,
  C.label = C.label,
  T.label = T.label
)


out3$figures$pie.cond = 
  plot_estlowup(x=yC.cond,
               est = out3$pie.cond[,4],
               low = out3$pie.cond[,3],
               up = out3$pie.cond[,5],
               xlim=c(0,25),xbreaks=seq(0,25,by=5),
               ylim=c(-2,16),ybreaks=seq(-2,16,by=2),
               ylab=gain.label,
               xlab="monotherapy survival time (months)"
  )

out3$figures$pie.cond
out3$figures$pie.gain

dput(out3, "revised-ex3-out.dmp")


ds = data(ASPIRE)
ds = ASPIRE
head(ds)

yC.cond = seq(4,80,by=4)
gain.cutoffs = seq(0,18,by=2)

C.label = "Rd"
T.label = "KRd"

out4 = pie_surv(
  data = ds,
  n.pred = n.pred,
  yC.cond = yC.cond,
  pie.gain.cutoffs = gain.cutoffs,
  time.unit = "months",
  gain.label = gain.label,
  # C.col = C.col,
  # T.col = T.col,
C.label = C.label,
T.label = T.label
)

out4$figures$pie.cond = 
  plot_estlowup(x=yC.cond,
               est = out4$pie.cond[,4],
               low = out4$pie.cond[,3],
               up = out4$pie.cond[,5],
               xlim=c(0,80),xbreaks=seq(0,80,by=20),
               ylim=c(-3,30),ybreaks=seq(-3,30,by=3),
               ylab=gain.label,
               xlab="Rd survival time (months)"
               # C.col = C.col,
               # T.col = T.col
  )

out4$figures$pie.cond
out4$figures$pie.gain

dput(out4, "revised-ex4-out.dmp")


data(CM057)
ds = CM057

ds0 = ds[ds$group == 0, ]
ds1 = ds[ds$group == 1, ]

C.label = "docetaxel"
T.label = "nivolumab"

gain.cutoffs = seq(-3, 24, by = 3)
gain.label = "survival gain in months (nivolumab-docetaxel)"

out5.0 = pie_surv(
  ds0,
  n.pred = n.pred,
  C.label = C.label,
  T.label = T.label,
  # C.col = C.col,
  # T.col = T.col,
  gain.label = gain.label,
  pie.gain.cutoffs = gain.cutoffs
)

out5.0$figures$pie.gain

out5.1 = pie_surv(
  ds1,
  n.pred = n.pred,
  C.label = C.label,
  T.label = T.label,
  # C.col = C.col,
  # T.col = T.col,
  gain.label = gain.label,
  pie.gain.cutoffs = gain.cutoffs
)

out5.1$figures$pie.gain


dput(out5.0, "revised-ex50-out.dmp")
dput(out5.1, "revised-ex51-out.dmp")


pie.sum = rbind(out1$pie,
                out2$pie,
                out3$pie,
                out4$pie,
                out5.0$pie,
                out5.1$pie)
pie.sum = pie.sum[, c(1, 4, 3, 5, 7)]
rownames(pie.sum) = c("CM141",
                      "Imvigor211",
                      "NCIC CTG",
                      "ASPIRE",
                      "CM057 (negative)",
                      "CM057 (positive)")
colnames(pie.sum) = c("mean", "median", "2.5%", "97.5%", "pr(test>control)")

options(digits = 2)
pie.sum

dput(pie.sum, "revised-piesum-out.dmp")




fig2A = out1$figures$km 
fig2B = out1$figures$pie.gain 
fig2C = out1$figures$pie.cond

fig3A = out2$figures$km 
fig3B = out2$figures$pie.gain 
fig3C = out2$figures$pie.cond

fig4A = out3$figures$km 
fig4B = out3$figures$pie.gain 
fig4C = out3$figures$pie.cond

fig5A = out4$figures$km 
fig5B = out4$figures$pie.gain 
fig5C = out4$figures$pie.cond

fig6.11 = out5.0$figures$km
fig6.21 = out5.1$figures$km 
fig6.12 = out5.0$figures$pie.gain 
fig6.22 = out5.1$figures$pie.gain 




# fig2B = fig2B + theme_classic()
# fig2C = fig2C + theme_classic()
# 
# fig3B = fig3B + theme_classic()
# fig3C = fig3C + theme_classic()
# 
# fig4B = fig4B + theme_classic()
# fig4C = fig4C + theme_classic()
# 
# fig5B = fig5B + theme_classic()
# fig5C = fig5C + theme_classic()
# 
# fig6.12 = fig6.12 + theme_classic()
# fig6.22 = fig6.22 + theme_classic()


dev.off()

pdf("fig2A.pdf",onefile=FALSE)
fig2A
dev.off()
pdf("fig2B.pdf")
fig2B
dev.off()
pdf("fig2C.pdf")
fig2C
dev.off()

pdf("fig3A.pdf",onefile=FALSE)
fig3A
dev.off()
pdf("fig3B.pdf")
fig3B
dev.off()
pdf("fig3C.pdf")
fig3C
dev.off()


pdf("fig4A.pdf",onefile=FALSE)
fig4A
dev.off()
pdf("fig4B.pdf")
fig4B
dev.off()
pdf("fig4C.pdf")
fig4C
dev.off()


pdf("fig5A.pdf",onefile=FALSE)
fig5A
dev.off()
pdf("fig5B.pdf")
fig5B
dev.off()
pdf("fig5C.pdf")
fig5C
dev.off()



pdf("fig6-11.pdf",onefile=FALSE)
fig6.11
dev.off()
pdf("fig6-21.pdf",onefile=FALSE)
fig6.21
dev.off()
pdf("fig6-12.pdf",onefile=FALSE)
fig6.12
dev.off()
pdf("fig6-22.pdf",onefile=FALSE)
fig6.22
dev.off()


