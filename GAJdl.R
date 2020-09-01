### GAJ using datalimited package
library(datalimited)
set.seed(1)
setwd("X:/Data_John/council meetings/June 2018/DataPoor")

gl <- read.csv("gajlandings.csv")
#gl <- subset(gl, year>=1981) ##not sensitive to this
gl2 <- data.frame(year=2018:2021, com=-9999, rec=-9999, total=Landings)
gl <- rbind(gl, gl2)
##resilience: http://www.fishbase.us/manual/English/Key%20Facts.htm#resilience
Landings <- rpois(4, 810043)


x <- cmsy(gl$year, ct = gl$total, reps = 20000,
          start_r=resilience("medium"))
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