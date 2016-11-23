win <- read.table("orgWind.txt", na.strings = "///")
save(win, file = "wind.rda")
load("wind.rda")
names(win) <- c("No","year","mon","day","type","d21","t21","d22","t22","d23","t23",
                "d00","t00","d01","t01","d02","t02","d03","t03","d04","t04","d05","t05",
                "d06","t06","d07","t07","d08","t08","d09","t09","d10","t10","d11","t11",
                "d12","t12","d13","t13","d14","t14","d15","t15","d16","t16","d17","t17",
                "d18","t18","d19","t19","d20","t20","tavg")
library(outliers)
library(dplyr)
ws <- win %>% select(No:type,t21,t22,t23,t00,t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,
                     t12,t13,t14,t15,t16,t17,t18,t19,t20,tavg)
ws[ws==32766] <- NA
save(ws, file = "ws.rda") #WindSpeed no QC
#=======================================
#求显著性差异
load("dataSets/ws.rda")
wst02 <- ws %>% select(No, year, mon, day,type, t02)
wst02$tt <- wst02$t02/10
wst08 <- ws %>% select(No, year, mon, day,type, t08)
wst08$tt <- wst08$t08/10
wst14 <- ws %>% select(No, year, mon, day,type, t14)
wst14$tt <- wst14$t14/10
wst20 <- ws %>% select(No, year, mon, day,type, t20)
wst20$tt <- wst20$t20/10
wstavg <- ws %>% select(No, year, mon, day,type, tavg)
wstavg$tt <- wstavg$tavg/10
save(wst02,wst08,wst14,wst20,wstavg, file = "dataSets/wst.rda")

t02data <- wst02 %>% group_by(No)  %>% do(te=t.test(tt~type,data=.)[[3]])
t02data$pValue <-do.call(c,t02data$te)

#tmpavg <- wst02 %>% filter(No!=54646,No!=57351,No!=57352,No!=57353,No!=57453)
tmpavg <- wst02 %>% filter(No!=54646,No!=57351,No!=57352,No!=57353,No!=57453)
tavgdata <- tmpavg %>% group_by(No)  %>% do(te=t.test(tt~type,data=.)[[3]])
tavgdata$pValue <-do.call(c,tavgdata$te)
nrow(tavgdata[tavgdata$pValue<0.05,])
#=======================================
aws <- ws %>% filter(type=="AWS")   %>% select(No, year, mon, day, t02)
hum <- ws %>% filter(type=="HUMAN") %>% select(No, year, mon, day, t02)
difftem <- data.frame((as.matrix(hum) - as.matrix(aws))/10)
difftem$No <- aws$No
difftem$year <- aws$year
difftem$mon <- aws$mon
difftem$day <- aws$day

tmp <- difftem %>% group_by(No) %>% summarise(total=sum(is.na(t02)))
write.csv(tmp, file = "dataSets/tmp.csv")
#p Value break
#======================================= 
#diff analysis
aws <- ws %>% filter(type=="AWS")   %>% select(No, year, mon, day, tavg)
hum <- ws %>% filter(type=="HUMAN") %>% select(No, year, mon, day, tavg)
difftem <- data.frame((as.matrix(hum) - as.matrix(aws))/10)
difftem$No <- aws$No
difftem$year <- aws$year
difftem$mon <- aws$mon
difftem$day <- aws$day

save(difftem, file = "dataSets/wsdiff.rda") #diff 
load("dataSets/wsdiff.rda")
library(outliers)
library(dplyr)
#extreme value filter
difftem$tavg[abs(difftem$tavg)>5] <- NA
#3 sigma filter
gSigma <- function(dataqc) {
  t <- dataqc
  t[!is.na(t)][scores(na.omit(t), prob =.997)] <- NA
  return(t)
}

difftem$tavg <- (difftem %>% group_by(No) %>% mutate(tmp = gSigma(tavg)))$tmp
library(tidyverse)
difftem <- as_tibble(difftem)
save(difftem, file = "dataSets/qcwsdiff.rda")
#
load("dataSets/qcwsdiff.rda")
# proTime <- function(z) {
#   pdf( "qcws.pdf")
#   z <- na.omit(z)
#   hist(z, freq = FALSE,
#        xlab = paste("Time dtr"),
#        main = "Difference")
#   rug(jitter(z), col = "brown")
#   curve(dnorm(x, mean=mean(z), sd=sd(z)),
#         add = TRUE, col = "blue", lwd = 2)
#   lines(density(z,width = 0.2),
#         col = "red", lwd = 2, lty =2)
#   dev.off()
# }
# proTime(difftem$tavg)

######################
library(ggplot2)
b <- ggplot(difftem, aes(x=tavg))
b <- b+geom_histogram(binwidth = 0.05,na.rm = TRUE)+scale_x_continuous(breaks = seq(-3,3,1))
ggsave("images/wsTwo.png",b,width = 5,height = 4,dpi = 600)

########################
library(outliers)
library(dplyr)

qtem <- ws %>% select(No:type,t02,t08,t14,t20)
qtem[qtem==32766] <- NA
aws <- qtem %>% filter(type=="AWS")   %>% select(No, year, mon, day, t02:t20)
hum <- qtem %>% filter(type=="HUMAN") %>% select(No, year, mon, day, t02:t20)
difftem <- data.frame((as.matrix(hum) - as.matrix(aws))/10)
difftem$No <- aws$No
difftem$year <- aws$year
difftem$mon <- aws$mon
difftem$day <- aws$day

# Extreme Value Processing

difftem$t02[abs(difftem$t02)>5] <- NA
difftem$t08[abs(difftem$t08)>5] <- NA
difftem$t14[abs(difftem$t14)>5] <- NA
difftem$t20[abs(difftem$t20)>5] <- NA

# 3sigma QC Processing
gSigma <- function(dataqc) {
  t <- dataqc
  t[!is.na(t)][scores(na.omit(t), prob =.997)] <- NA
  return(t)
}


difftem$t02 <- (difftem %>% group_by(No) %>% mutate(tmp = gSigma(t02)))$tmp
difftem$t08 <- (difftem %>% group_by(No) %>% mutate(tmp = gSigma(t08)))$tmp
difftem$t14 <- (difftem %>% group_by(No) %>% mutate(tmp = gSigma(t14)))$tmp
difftem$t20 <- (difftem %>% group_by(No) %>% mutate(tmp = gSigma(t20)))$tmp

library(ggplot2)
library(tidyr)
aa <- fourDiff %>% select(No, t02, t08, t14, t20) %>% gather(type, val, -No)
bb <- aa %>% filter(!is.na(val))
p <- ggplot(bb, aes(factor(type),val))+xlab("Time")+ylab("Wind_2_min difference")
p <- p + geom_boxplot(outlier.colour = "red",outlier.shape = 1)+theme_bw()
#+stat_summary(fun.y = mean,geom = "line",aes(group=1))
ggsave("images/TD51.png",p,width = 5, height = 4, dpi = 600)
##############
vt <- fourDiff
z <- na.omit(vt$t08)
ylims <- range(density(z,width = 1)$y)
png( "images/ntxx.png",width = 750, height = 1000)
opar <- par(no.readonly = TRUE)
par(mfrow=c(4,1))
p <- na.omit(vt$t02)
hist(p, probability = T, xlab ="", ylab = "Density", breaks =60 , main = "Wind difference", ylim = ylims, xaxt="n",cex.axis=2,cex.lab=1.45, cex.main=2)
axis(1,at=seq(-4,4,1),cex.axis=2)
mtext("t02", side = 4, cex = 2)
abline(h=c(.5,1,1.5),lwd=1.5, lty=2, col="grey")
abline(v=c(mean(p)),lwd=3, lty=6, col="brown")
curve(dnorm(x, mean=mean(p), sd=sd(p)), add = TRUE, col = "blue", lwd = 2)
lines(density(p,width = 0.5), col = "red", lwd = 2, lty =2)
p <- na.omit(vt$t08)
hist(p, probability = T, xlab ="", ylab = "Density", breaks =60, main = "", ylim = ylims, xaxt="n",cex.axis=2,cex.lab=1.45, cex.main=2)
axis(1,at=seq(-4,4,1),cex.axis=2)
mtext("t08", side = 4, cex = 2)
abline(h=c(.5,1,1.5),lwd=1.5, lty=2, col="grey")
abline(v=c(mean(p)),lwd=3, lty=6, col="brown")
curve(dnorm(x, mean=mean(p), sd=sd(p)), add = TRUE, col = "blue", lwd = 2)
lines(density(p,width = 0.5), col = "red", lwd = 2, lty =2)
p <- na.omit(vt$t14)
hist(p, probability = T, xlab ="", ylab = "Density", breaks =60, main="", ylim = ylims, xaxt="n",cex.axis=2,cex.lab=1.45, cex.main=2)
axis(1,at=seq(-4,4,1),cex.axis=2)
mtext("t14", side = 4, cex = 2)
abline(h=c(.5,1,1.5),lwd=1.5, lty=2, col="grey")
abline(v=c(mean(p)),lwd=3, lty=6, col="brown")
curve(dnorm(x, mean=mean(p), sd=sd(p)), add = TRUE, col = "blue", lwd = 2)
lines(density(p,width = 0.5), col = "red", lwd = 2, lty =2)
p <- na.omit(vt$t20)
hist(p, probability = T, xlab ="", ylab = "Density", breaks =60, main = "", ylim = ylims, xaxt="n",cex.axis=2,cex.lab=1.45, cex.main=2)
axis(1,at=seq(-4,4,1),cex.axis=2)
mtext("t20", side = 4, cex = 2)
abline(h=c(.5,1,1.5),lwd=1.5, lty=2, col="grey")
abline(v=c(mean(p)),lwd=3, lty=6, col="brown")
curve(dnorm(x, mean=mean(p), sd=sd(p)), add = TRUE, col = "blue", lwd = 2)
lines(density(p,width = 0.5), col = "red", lwd = 2, lty =2)
par(opar)
dev.off()
