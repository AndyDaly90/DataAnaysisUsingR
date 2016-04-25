Uni2016$world_rank <- str_replace_all(top1002016$world_rank, "[^[:alnum:]]", "")
install.packages("rworldmap")
install.packages("plyr")
library(rworldmap)
library(plyr)


subsetYear <- subset(UniData, year==2016)
averageResearch <- ddply(subsetYear,.(country), summarize, value=mean(research))
colnames(averageResearch) <- c("country","average")
worldMap <- joinCountryData2Map(averageResearch, nameJoinColumn = "country", joinCode = "NAME")
mapCountryData(worldMap, nameColumnToPlot = 'average', catMethod = 'fixedWidth',
               numCats = 20, mapTitle = paste("Research in 2016"))
