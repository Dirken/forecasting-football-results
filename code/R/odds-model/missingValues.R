##########################################
#EDA:
##########################################
#do we have a lot of NA's?
numberOfTables <- length(tables)
for(i in 1:numberOfTables){
  
  fileName <- paste0("images/missingValues/md-pattern", i, ".jpeg")
  jpeg(file=fileName, width = 1000, height = 1000)
  md.pattern(tables[[i]])
  dev.off()
  
  fileName <- paste0("images/missingValues/matrixplot", i, ".jpeg")
  jpeg(file=fileName, width = 1000, height = 1000)
  matrixplot(tables[[3]]) #◘tables[[3]][86:115] to do matrixplot of betting odds
  dev.off()
  
  fileName <- paste0("images/missingValues/scattmatrixMiss", i, ".jpeg")
  jpeg(file=fileName, width = 1000, height = 1000)
  scattmatrixMiss(tables[[i]])
  dev.off()
}

#Need to interpret this plots better.
par(resetPar())
fileName <- paste0("images/missingValues/aggrMatches", ".jpeg")
jpeg(file=fileName, width = 5000, height = 5000)
aggr(match, numbers =T, prop=T, cex.axis=3,cex.numbers=3) #more readable but not the rhs
dev.off()
fileName <- paste0("images/missingValues/aggrTeams", ".jpeg")
jpeg(file=fileName, width = 5000, height = 5000)
aggr(team, numbers =T, prop=T, cex.axis=3,cex.numbers=3)
dev.off()

