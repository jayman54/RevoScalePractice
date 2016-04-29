#6.6
#pg 41

dataDir <- getwd()

inDataFile <- file.path(dataDir,"mortDefault","mortDefault2000.csv")
outFile <- "myMortData.xdf"
outFile2 <- "myMortData2.xdf"

mortData <- rxImport(inData = inDataFile, outFile = outFile,overwrite = TRUE)

rxGetInfo(data = mortData, getVarInfo = TRUE, numRows = 10)

rows <- dim(mortData)[1]

mortDataNew <- rxDataStep(
  inData = mortData,
  outFile = outFile2,
  overwrite = TRUE,
  varsToDrop = c("year"),
  rowSelection = creditScore < 850,
  transforms = list(
    catDebt = cut(ccDebt, breaks = c(0,6500,13000),
                  labels = c("Low Debt","High Debt")),
    lowScore = creditScore < 625,
    rnd = dim(inData)[1]
  )
  
)

rxHistogram(~creditScore, data = mortDataNew)

myCube = rxCube(~F(creditScore):catDebt, data = mortDataNew)

rxLinePlot(Counts~creditScore|catDebt, data = rxResultsDF(myCube))

rxGetInfo(data = mortDataNew, getVarInfo = TRUE, numRows = 10)

myLogit <- rxLogit(formula = default ~ creditScore + houseAge + yearsEmploy
                   + ccDebt, data=mortDataNew)
summary(myLogit)


runif(1)

