setwd("d:/lab/R")
winMax <- read.table("dataSets/morgWMax.txt", na.strings = "///")
save(winMax, file = "dataSets/winMax.rda")
load("dataSets/winMax.rda")
names(winMax) <- c("No","year","mon","day","type","maxD" ,"wMax")
winMax[winMax==32766] <- NA
