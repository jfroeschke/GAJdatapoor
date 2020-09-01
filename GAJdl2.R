### fit data limited model to generate biomass index for use in ices model
### GAJ using datalimited package
#devtools::install_github("datalimited/datalimited")
options(scipen=999)
library(datalimited)
set.seed(1)
#setwd("X:/Data_John/council meetings/June 2018/DataPoor")
setwd("X:/Data_John/council meetings/August 2018/WFH06212018/DataPoor")
#setwd('C:/Users/johnf/Documents/WFH06212018/DataPoor')

gl <- read.csv("gajlandings.csv")
#gl <- subset(gl, year>=1981) ##not sensitive to this
x <- cmsy(gl$year, ct = gl$total, reps = 20000,
          start_r=resilience("medium"))
Biomass <- apply(x$biomass, 2, median)[-1]
preds <- x$bbmsy

### Summary plots
names(x)
#> [1] "theta"       "biomass"     "bmsy"        "msy"         "mean_ln_msy"
#> [6] "bbmsy"
par(mfrow = c(2, 2))
plot(gl$year, gl$total, type = "o", xlab = "Year", 
     ylab = "Catch (lbs)")
plot(gl$year,  apply(x$biomass, 2, median)[-1], type = "o",
     ylab = "Estimated biomass", xlab = "Year")
hist(x$bmsy)
plot(x$theta$r, x$theta$k, col = "#00000030")


library("ggplot2")
ggplot(x$bbmsy, aes(year, bbmsy_q50)) + geom_line()  +
  geom_ribbon(aes(ymin = bbmsy_q25, ymax = bbmsy_q75), alpha = 0.2) +
  geom_ribbon(aes(ymin = bbmsy_q2.5, ymax = bbmsy_q97.5), alpha = 0.1) +
  geom_hline(yintercept = 1, lty = 2) + theme_light()
ggsave(filename="U:/presentation/DD/figures/biomass.png",
       dpi=600)
### try biomass dynamic model
#devtools::install_github("mawp/spict/spict")
## help: plotspict.catch(res)

library(spict)
inp2 <- check.inp(list(obsC=x$bbmsy$catch,
                       obsI=x$bbmsy$bbmsy_q50))
## use this one to include year labels
inp3 <- check.inp(list(obsC=x$bbmsy$catch,
                       obsI=x$bbmsy$bbmsy_q50,
                       timeC=1963:2017,
                       timeI=1963:2017))
# res3 <- fit.spict(inp3)
res3 <- fit.spict(inp3)
##Plot catch and index data
plotspict.data(inp3, qlegend=FALSE)

## relative biomass
plotspict.bbmsy(res3, qlegend=FALSE)
plotspict.f(res3, main='', qlegend=FALSE, rel.axes=FALSE, rel.ci=FALSE)
plotspict.ffmsy(res, main='', qlegend=FALSE)
plotspict.catch(res3, qlegend=FALSE)
plotspict.fb(res3)

png(filename="U:/presentation/DD/figures/summary.png", width=(3.91*2),
    height=(3.19*2), units="in", res=600)
    par(mfrow=c(2, 2))
plotspict.catch(res3, qlegend=FALSE)
plotspict.bbmsy(res3, qlegend=FALSE)
plotspict.f(res3, qlegend=FALSE)
plotspict.fb(res3)
dev.off()
##time to rebuild

par(mfrow=c(1,1))
png(filename="U:/presentation/DD/figures/rebuildtim.png", width=(3.91*2),
    height=(3.19*2), units="in", res=600)
plotspict.tc(res3)
dev.off()


plotspict.ci(inp3)

# Plots observed catch and predictions using 
# the current F and Fmsy. The plot also
# contains the equilibrium catch if the current F is maintained.
png(filename="U:/presentation/DD/figures/catch.png", width=3.91,
    height=3.19, units="in", res=600)
dev.off()
plotspict.catch(res3, qlegend=FALSE)


plotspict.tc(res3) #time to rebuild
dev.off()

png(filename="U:/presentation/DD/figures/ffmsy.png", width=3.91,
    height=3.19, units="in", res=600)
plotspict.ffmsy(res3, qlegend=FALSE)
dev.off()

png(filename="U:/presentation/DD/figures/kobe.png", width=3.91,
    height=3.19, units="in", res=600)
plotspict.fb(res3)
dev.off()
##retrospectives
retrores3 <- retro(res3)
plotspict.retro(retrores3)


## see this for summary info
sumpredout3 <- sumspict.predictions(res3)

### 
save.image("GAJdatapoor.RData")
### see datalimited about how biomass is estimated
