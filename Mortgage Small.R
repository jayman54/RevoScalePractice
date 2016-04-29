
inDataFile <- file.path(rxGetOption("sampleDataDir"),"mortDefaultSmall2000.csv")

mortData <- rxImport(inData = inDataFile)

rxGetInfo(data = mortData,getVarInfo = TRUE, numRows = 3)

mortDataNew <- rxDataStep(inData = mortData,
                          # outFile = outDataFile, 
                          varsToDrop = c("year"),rowSelection = creditScore<850,
                          transforms = list(
                            catDebt = cut(ccDebt, breaks = c(0,6500,13000),
                                          labels = c("Low Debt","High Debt")),
                            lowScore = creditScore < 625))

rxHistogram(~credmyLogit <- rxLogit(default~ccDebt+yearsEmploy , data=mortDataNew) 
            summary(myLogit)itScore, data = mortDataNew)
qqnorm(mortDataNew$creditScore)

mortCube <- rxCube(data = mortDataNew,~F(creditScore):catDebt)

rxLinePlot(formula = Counts~creditScore|catDebt, data = rxResultsDF(mortCube))