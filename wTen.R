setwd("d:/lab/R")
winTen <- read.table("dataSets/morgW10.txt", na.strings = "///")
save(winTen, file = "dataSets/winTen.rda")
load("dataSets/winTen.rda")
names(winTen) <- c("No","year","mon","day","type","d21","t21","d22","t22","d23","t23",
                "d00","t00","d01","t01","d02","t02","d03","t03","d04","t04","d05","t05",
                "d06","t06","d07","t07","d08","t08","d09","t09","d10","t10","d11","t11",
                "d12","t12","d13","t13","d14","t14","d15","t15","d16","t16","d17","t17",
                "d18","t18","d19","t19","d20","t20","wTen")
library(outliers)
library(dplyr)
wsTen <- winTen %>% select(No:type,t21,t22,t23,t00,t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,
                     t12,t13,t14,t15,t16,t17,t18,t19,t20,wTen)
save(wsTen, file = "dataSets/wsTen.rda")
wsTen[wsTen==32766] <- NA
##################

