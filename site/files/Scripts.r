#PARA FAZER:
#- boxcox
###################################################################################################################################
# round numbers: 2, 3; any different number corresponds to rounds 2 and 3 together
# task valid numbers: 1 and 2
# SD occurs only for task 2, where we fix the data by adding the SD to the wrong answers
###################################################################################################################################
main <- function(round, task, SD=F) {
  
  #root path.
  #importPath = "/Users/marcinho/Dropbox/Tese/Experimento/"
  importPath = "/Users/marcioribeiro/Dropbox/Tese/Experimento/"
  
  #export path
  #exportPath = "/Users/marcinho/R-script-results/"
  exportPath = "/Users/marcioribeiro/R-script-results/"
  
  #Defines the fileName according to the task
  if (task == 1) {
    timeFileName = "Time-M1-All"
    errorsFileName = "Time-Errors-All"
    taskName = "M1"
  } else {
    timeFileName = "Time-M2-All"
    #if (rmOutlier) {
      #timeFileName = "Time-M2-All-Outlier"
    #}
    taskName = "M2"
  }
  
  #Defines the round root directory
  if (round == 2) {
    roundRootDir = "Round 2"
    roundName = "R2"
  } else if (round == 3) {
    roundRootDir = "Round 3"
    roundName = "R3"
  } else {
    roundRootDir = "Rounds-2and3"
    roundName = "R2andR3"
  }
  
  #Defines the directory name in terms of the round and the task
  directoryName = paste(roundName, "-", taskName, sep="")
  if (SD) {
    directoryName = paste(directoryName, "-SD", sep="")
  }

  #Loads the data according to the round
  timeData = read.table(file=paste(importPath, roundRootDir, "/Dados/", timeFileName, ".dat", sep=""), header=T)
  
  #Adds the standard deviation to the wrong answers
  timeDataWithoutSD = timeData
  if (SD) {
    timeData = addStandardDeviationToWrongAnswers(timeData)
  }
  
  #Puts the letters for Dotplot
  dataDot = putLettersForDotplot(timeData)
  
  #Updates the export path to contain the round and task names
  exportPath = paste(exportPath, directoryName, "/", sep="")
  
  #HTML file
  htmlFile = paste(exportPath, directoryName, ".html", sep="")
  
  #Delete all graphics and the HTML file
  deleteAllFiles(exportPath, htmlFile)

  #Creates the directory if it does not exist
  if (!file.exists(exportPath)) {
    dir.create(file.path(exportPath))
    setwd(file.path(exportPath))
  }
  
  attach(timeData)
  
  replic = factor(Replic)
  subject = factor(Subject)
  spl = factor(SPL)
  technique = factor(Technique)
  
  #ANOVA
  if (round == 2) {
    anova.ql = aov(time~replic+subject:replic+spl+technique)
  } else {
    #For Rounds 3 and 4, we adjust the model to make it additive according to Tukey test.
    anova.ql = aov(time^0.5~replic+subject:replic+spl+technique)
  }
  
  #Boxcox
  library(MASS)
  #png(paste(exportPath, "boxcox.png", sep=""))
  #boxcox(anova.ql, lambda = seq(-3, 5, 1/10))
  #dev.off()
  
  #Boxplot
  boxplotPDFFileName = paste("Boxplot-", directoryName, ".png", sep="")
  #png(paste(exportPath, boxplotPDFFileName, sep=""), width=6, height=6)
  png(paste(exportPath, boxplotPDFFileName, sep=""))
  plot(time~technique, col=c("gray"), xlab="Technique", ylab="Time (Seconds)")
  dev.off()
  
  #Beanplot
  library(beanplot)
  beanplotPDFFileName = paste("Beanplot-", directoryName, ".png", sep="")
  #png(paste(exportPath, beanplotPDFFileName, sep=""), width=6, height=6)
  png(paste(exportPath, beanplotPDFFileName, sep=""))
  beanplot(time~technique, col=c("black","white","blue", "red"), ylab="Time (Seconds)", xlab="Technique")
  dev.off()
  
  #Dotplot
  library(ggplot2)
  
  attach(dataDot)
  replicDot = factor(Replic)
  subjectDot = factor(Subject)
  splDot = factor(SPL)
  techniqueDot = factor(Technique)
  
  dotplotPDFFileName = paste("Dotplot-", directoryName, ".png", sep="")
  #png(paste(exportPath, dotplotPDFFileName, sep=""), width=8, height=4)
  png(paste(exportPath, dotplotPDFFileName, sep=""))
  p = qplot(subjectDot, time, colour=splDot, shape = factor(techniqueDot), xlab="Subjects", ylab="Time (Seconds)")
  print(p + geom_point(size=3) + labs(colour="SPL", shape="Technique"))
  dev.off()
  
  eiTime = getEIData(timeData)
  vsocTime = getVSoCData(timeData)
  meanDifference = difference(eiTime, vsocTime, mean)
  medianDifference = difference(eiTime, vsocTime, median)
  variance = varianceConstant(eiTime, vsocTime)
  
  vsocBestlapMean = getMeanForTechniqueAndSPL(timeData, round, "No-EIs", "Bestlap")
  vsocMobileMediaMean = getMeanForTechniqueAndSPL(timeData, round, "No-EIs", "MobileMedia")
  eiBestlapMean = getMeanForTechniqueAndSPL(timeData, round, "EIs", "Bestlap")
  eiMobileMediaMean = getMeanForTechniqueAndSPL(timeData, round, "EIs", "MobileMedia")
  
  #HTML code
  library(R2HTML)
  
  if (task == 1) {
    #title = paste("<hr><h1>Results for ", roundRootDir, " - New requirement task</h1>", sep="") 
    title = paste("<hr><h1>Results for Round 2 - New requirement task</h1>", sep="")
  } else {
    #title = paste("<hr><h1>Results for ", roundRootDir, " - Unused variable task</h1>", sep="")
    title = paste("<hr><h1>Results for Round 2 - Unused variable task</h1>", sep="")
  }
  
  HTML.title(title, file=htmlFile, append=TRUE)
  
  HTML("<hr><h2>Time Data</h2>", file=htmlFile, append=TRUE)
  HTML(timeDataWithoutSD, file=htmlFile, append=TRUE)
  
  if (SD) {
    HTML(paste("<hr><h2>Time Data with Standard Deviation for Wrong Answers; SD = ", round(sd(timeDataWithoutSD[,5])), "</h2>", sep=""), file=htmlFile, append=TRUE)
    HTML(timeData, file=htmlFile, append=TRUE)  
  }
  
  HTML("<hr><h2>Graphics</h2>", file=htmlFile, append=TRUE)
  HTMLInsertGraph(file=htmlFile, GraphFileName=boxplotPDFFileName, Align="center", append=TRUE)
  HTMLInsertGraph(file=htmlFile, GraphFileName=beanplotPDFFileName, Align="center", append=TRUE)
  HTMLInsertGraph(file=htmlFile, GraphFileName=dotplotPDFFileName, Align="center", append=TRUE)
  #HTMLInsertGraph(file=htmlFile, GraphFileName="boxcox.png", Align="center", append=TRUE)
  
  #Barplot - Errors
  if (task == 1) {
    errorsData = read.table(file=paste(importPath, roundRootDir, "/Dados/", errorsFileName, ".dat", sep=""), header=T)
    
    HTML("<hr><h2>Errors Data</h2>", file=htmlFile, append=TRUE)
    HTML(errorsData, file=htmlFile, append=TRUE)
    
    barplotPDFFileName = paste("Barplot-Errors-", directoryName, ".png", sep="")
    #png(paste(exportPath, barplotPDFFileName, sep=""), width=8, height=4)
    png(paste(exportPath, barplotPDFFileName, sep=""))
    data = tapply(errorsData$errors, list(errorsData$Technique, errorsData$Subject), sum)
    barplot(data, beside=T, col=c("blue","red"), main="Number of Errors (NE) per Subject", xlab="Subjects", ylab="Number of Errors (NE)")
    legend("topright", rownames(data), fill=c("blue","red"))
    dev.off()
    
    HTMLInsertGraph(file=htmlFile, GraphFileName=barplotPDFFileName, Align="center", append=TRUE)
  }
  
  HTML("<hr><h2>ANOVA summary</h2>", file=htmlFile, append=TRUE)
  HTML(summary(anova.ql), file=htmlFile, append=TRUE)
  
  HTML("<hr><h2>Differences between both approaches</h2>", file=htmlFile, append=TRUE)
  
  #Standard deviation
  HTML(paste("<hr><b>Standard deviation time without Emergent Interfaces: </b>", sd(vsocTime), sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<b>Standard deviation time with Emergent Interfaces: </b>", sd(eiTime), sep=""), file=htmlFile, append=TRUE)
  
  if (task == 2) {
    allTimes = c(eiTime, vsocTime)
    HTML(paste("<hr><b>Standard deviation (all times): </b>", sd(allTimes), sep=""), file=htmlFile, append=TRUE)
  }
  
  #Mean
  HTML(paste("<hr><b>Mean time without Emergent Interfaces: </b>", mean(vsocTime), sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<b>Mean time with Emergent Interfaces: </b>", mean(eiTime), sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<b>without / with: </b>", meanDifference, sep=""), file=htmlFile, append=TRUE)
  
  #Median
  HTML(paste("<hr><b>Median time without Emergent Interfaces: </b>", median(vsocTime), sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<b>Median time with Emergent Interfaces: </b>", median(eiTime), sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<b>without / with: </b>", medianDifference, sep=""), file=htmlFile, append=TRUE)
  
  #Differences
  HTML("<hr><b>Time results without Emergent Interfaces: </b>", file=htmlFile, append=TRUE)
  HTML(vsocTime, file=htmlFile, append=TRUE)
  HTML("<b>Time results with Emergent Interfaces: </b>", file=htmlFile, append=TRUE)
  HTML(eiTime, file=htmlFile, append=TRUE)
  HTML("<b>Differences: </b>", file=htmlFile, append=TRUE)
  HTML(vsocTime - eiTime, file=htmlFile, append=TRUE)
  
  #Mean per Technique and Product Line
  HTML(paste("<hr><b>Mean(No-EIs, Bestlap) = </b>", vsocBestlapMean, sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<hr><b>Mean(No-EIs, MobileMedia) = </b>", vsocMobileMediaMean, sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<hr><b>Mean(EIs, Bestlap) = </b>", eiBestlapMean, sep=""), file=htmlFile, append=TRUE)
  HTML(paste("<hr><b>Mean(EIs, MobileMedia) = </b>", eiMobileMediaMean, sep=""), file=htmlFile, append=TRUE)
  
  #Tukey
  HTML("<hr><h2>Tukey test for additivity</h2>", file=htmlFile, append=TRUE)
  HTML(tukeyAdditive(anova.ql), file=htmlFile, append=TRUE)
  
  #Bartlett
  HTML("<hr>", file=htmlFile, append=TRUE)
  HTML(variance, file=htmlFile, append=TRUE)
}

putLettersForDotplot <- function(data) {
  letters = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  
  numberOfRows = length(data[,1])
  
  j = 1
  for (i in seq(1, numberOfRows, by=2)) {
    data[i,2]   = letters[j]
    data[i+1,2] = letters[j]
    j = j + 1
  }

  data
}

addStandardDeviationToWrongAnswers <- function(data) {
  #Position 5 = times
  #Position 6 = right

  #"N" stands for "no"; both variables are wrong
  #"H" stand for "half"; only one variable is wrong
  
  sd = round(sd(data[,5]))

  numberOfRows = length(data[,1])

  for (i in 1:numberOfRows) {
    if (data[i,6] == "N") {
      data[i,5] = data[i,5] + sd
    } else if (data[i,6] == "H") {
      data[i,5] = data[i,5] + round(sd/2)
    }
  }

  data
}

getMeanForTechniqueAndSPL <- function(data, round, technique, spl) {
  #Position 3 = SPL
  #Position 4 = Technique
  #Position 5 = Time
  
  numberOfRows = length(data[,1])
  
  sum = 0;
  
  for (i in 1:numberOfRows) {
    if (data[i,3] == spl && data[i,4] == technique) {
      sum = sum + data[i,5]
    }
  }
  
  sum / (numberOfRows / 4)
}

getEIData <- function(data) {
  eiVector = data[data[,4] == "EIs",]
  eiVector[,5]
}

getVSoCData <- function(data) {
  vsocVector = data[data[,4] == "No-EIs",]
  vsocVector[,5]
}

difference <- function(eiTime, vsocTime, f) {
  ei = f(eiTime)
  vsoc = f(vsocTime)
  vsoc / ei
}

varianceConstant <- function(eiTime, vsocTime) {
  group = as.factor(c(rep(1, length(vsocTime)), rep(2, length(eiTime))))
  y = c(sqrt(vsocTime), sqrt(eiTime))
  bartlett.test(y, group)
}

tukeyAdditive <- function(object1) {
  y1<-NULL
  y2<-NULL
  y1<- fitted(object1)
  y2<- y1^2
  object2<- aov(y2 ~ object1[13]$model[,2] +
    object1[13]$model[,3]:object1[13]$model[,2]
                + object1[13]$model[,4]+ object1[13]$model[,5])
  ynew <- resid(object1)
  xnew <- resid(object2)
  object3 <- lm(ynew ~ xnew)
  M <- anova(object3)
  MSN <- M[1,3]
  MSErr <- M[2,2]/(object1[8]$df.residual-1)
  F0 <- MSN/MSErr
  p.val <- 1 - pf(F0, 1,object1[8]$df.residual-1)
  p.val
}

deleteAllFiles <- function(exportPath, htmlFile) {
  if (file.exists(htmlFile)) {
    file.remove(htmlFile)
  }
  
  filesToRemove = c("boxplot.png", "beanplot.png", "barplot.png", "dotplot.png")
  for (i in 1:length(filesToRemove)) {
    fileToRemove = paste(exportPath, filesToRemove[i], sep="")
    if (file.exists(fileToRemove)) {
      file.remove(fileToRemove)
    }
  }
}

beanplots <- function(SD=T) {
  #importPath = "/Users/marcinho/Dropbox/Tese/Experimento/"
  importPath = "/Users/marcioribeiro/Dropbox/Tese/Experimento/"

  round2m1 = read.table(file=paste(importPath, "Round 2/Dados/Time-M1-All.dat", sep=""), header=T)
  round2m2 = read.table(file=paste(importPath, "Round 2/Dados/Time-M2-All.dat", sep=""), header=T)
  #round3m1 = read.table(file=paste(importPath, "Round 3/Dados/Time-M1-All.dat", sep=""), header=T)
  #round3m2 = read.table(file=paste(importPath, "Round 3/Dados/Time-M2-All.dat", sep=""), header=T)
  
  attach(round2m1)
  attach(round2m2)
  #attach(round3m1)
  #attach(round3m2)
  
  if (SD) {
    round2m2 = addStandardDeviationToWrongAnswers(round2m2)
    #round3m2 = addStandardDeviationToWrongAnswers(round3m2)
  }
  
  technique = factor(Technique)
  
  library(beanplot)

  eiDataM1 = getEIData(round2m1)
  vsocDataM1 = getVSoCData(round2m1)
  eiDataM2 = getEIData(round2m2)
  vsocDataM2 = getVSoCData(round2m2)
  
  #eiDataM1 = getEIData(round3m1)
  #vsocDataM1 = getVSoCData(round3m1)
  #eiDataM2 = getEIData(round3m2)
  #vsocDataM2 = getVSoCData(round3m2)

  print(c(eiDataM1, eiDataM2, vsocDataM1, vsocDataM2))
  
  taskMax = max(c(eiDataM1, eiDataM2, vsocDataM1, vsocDataM2))
  print(taskMax)
  
  beanplot(time ~ technique, round2m1,
  #beanplot(time ~ technique, round3m1,
          ## 6 eh a quantidade de conjuntos de boxplots q eu tenho, no caso eu tenho 6 conjuntos de 3 box plots (random, meta-h e union)
          at = 1:2 - 0.2, 
          boxwex = 0.25, 
          col = "darkgray",
          main = "Round 1",
          xlab = "Technique",
          ylab = "Time (seconds)",
          #ylim = c(30, taskMax * 2.4),
          yaxs = "i",
          cex.lab=0.8,
          cex.axis=0.7)
  
  beanplot(time ~ technique, round2m2,
  #beanplot(time ~ technique, round3m2,
          boxwex = 0.25,
          at = 1:2 + 0.1,
          add = TRUE,
          col = "white",
          cex.lab=0.8,
          cex.axis=0.7)
  
  #x = locator()
  #print(x)
  par(xpd=TRUE);
  legend("topright", c("New R.", "Unused"), cex=.7, fill = c("darkgray", "white"))
}

beanplots2 <- function(SD=T) {
  #importPath = "/Users/marcinho/Dropbox/Tese/Experimento/"
  importPath = "/Users/marcioribeiro/Dropbox/Tese/Experimento/"
  
  #round2m1 = read.table(file=paste(importPath, "Round 2/Dados/Time-M1-All.dat", sep=""), header=T)
  round2m2 = read.table(file=paste(importPath, "Round 2/Dados/Time-M2-All.dat", sep=""), header=T)
  #round3m1 = read.table(file=paste(importPath, "Round 3/Dados/Time-M1-All.dat", sep=""), header=T)
  round3m2 = read.table(file=paste(importPath, "Round 3/Dados/Time-M2-All.dat", sep=""), header=T)
  
  #attach(round2m1)
  attach(round2m2)
  #technique = factor(round2m1[,4])
  technique = factor(round2m2[,4])
  
  #attach(round3m1)
  attach(round3m2)
  #technique2 = factor(round3m1[,4])
  technique2 = factor(round3m2[,4])
  
  if (SD) {
    round2m2 = addStandardDeviationToWrongAnswers(round2m2)
    round3m2 = addStandardDeviationToWrongAnswers(round3m2)
  }

  library(beanplot)
  
  #eiDataR2 = getEIData(round2m1)
  #vsocDataR2 = getVSoCData(round2m1)
  eiDataR2 = getEIData(round2m2)
  vsocDataR2 = getVSoCData(round2m2)
  
  #eiDataR3 = getEIData(round3m1)
  #vsocDataR3 = getVSoCData(round3m1)
  eiDataR3 = getEIData(round3m2)
  vsocDataR3 = getVSoCData(round3m2)
  
  print(c(eiDataR2, eiDataR3, vsocDataR2, vsocDataR3))
  
  taskMax = max(c(eiDataR2, eiDataR3, vsocDataR2, vsocDataR3))
  print(taskMax)
  
  #beanplot(time ~ technique, round2m1,
  beanplot(time ~ technique, round2m2,
           ## 6 eh a quantidade de conjuntos de boxplots q eu tenho, no caso eu tenho 6 conjuntos de 3 box plots (random, meta-h e union)
           at = 1:2 - 0.2, 
           boxwex = 0.25, 
           col = "darkgray",
           #main = "New requirement task",
           main = "Unused variable task",
           xlab = "Technique",
           ylab = "Time (seconds)",
           #NR
           #ylim = c(10, taskMax * 1.2),
           #UV
           ylim = c(40, taskMax * 1.3),
           yaxs = "i",
           cex.lab=0.8,
           cex.axis=0.7)
  
  #beanplot(time ~ technique2, round3m1,
  beanplot(time ~ technique2, round3m2,
           boxwex = 0.25,
           at = 1:2 + 0.1,
           add = TRUE,
           col = "white",
           cex.lab=0.8,
           cex.axis=0.7)
  
  #x = locator()
  #print(x)
  par(xpd=TRUE);
  legend("topright", c("Round 1", "Round 2"), cex=.7, fill = c("darkgray", "white"))
}

barplots <- function() {
  #importPath = "/Users/marcinho/Dropbox/Tese/Experimento/"
  importPath = "/Users/marcioribeiro/Dropbox/Tese/Experimento/"

  errorsData = read.table(file=paste(importPath, "Rounds-2and3/Dados/Time-Errors-All.dat", sep=""), header=T)

  data = tapply(errorsData$errors, list(errorsData$Technique, errorsData$Subject), sum)

  barplot(data, beside=T, col=c("gray","white"), main="Number of Errors (NE) per Participant", xlab="Participants", ylab="Number of Errors (NE)", cex.names=0.7, cex.lab=0.8, cex.axis=0.8)
  legend("topright", rownames(data), fill=c("gray","white"), cex=.7)
}