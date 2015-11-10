
userId <- "langestrst01"
passWd <- "Devt0314"

Rdir <- "~/PROJECTS/Thought Leadership/TL_Data"

source(file.path(Rdir,"libraries.R"))

datain <- "~/PROJECTS/Thought Leadership"

setwd(datain)

rd_CCI <- function(filename,nrRows,nrCols) {
  
  #filename <- "Q3 2015 CCI AP REGION.xls"; nrRows <- 10000; nrCols <- 16
  #rm(filename,infile,dataIn,rowstart,rowend,jobnr,date,table,index,base,question,question_sub,r,nrRows,nrCols);
  
  dataIn <- loadWorkbook(filename)
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=1, endRow=nrRows, startCol=1, endCol=nrCols, header=F)
  project <- infile[1,"Col1"]
  jobnr <- infile[2,"Col1"]
  date <- infile[3,"Col1"]
  table <- infile[4,"Col1"]
  index <- infile[5,"Col1"]
  infile <- infile[!is.na(infile$Col2),]
  rowstart <- as.numeric(rownames(infile[1,]))
  rowend <- as.numeric(rows[length(rownames(infile))])
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=rowstart, endRow=rowend, startCol=1, endCol=nrCols, header=TRUE)
  names(infile)[1] <- "Response"
  infile <- infile[!is.na(infile$Response),]
  infile$question <- index
  infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$question_sub <- "None"
  infile <- infile[c(1,ncol(infile),3:ncol(infile)-1)]
  infile$base <- infile[1,"Response"]
  infile <- infile[c(1:3,ncol(infile),5:ncol(infile)-1)]
  question <- infile[1,"question"]
  question_sub <- infile[1,"question_sub"]
  base <- infile[1,"base"]
  for (r in 1:nrow(infile)) {
    if (grepl("Base:",infile[r,"Response"]) & !grepl("wtd",infile[r,"Response"])) {
      base <- infile[r,"Response"]
    } else if (grepl("^Q[1,2,3,4,5,6]",infile[r,"Response"])) {
      question <- infile[r,"Response"]
      if (grepl("-",infile[r+1,"Response"])) {
        question_sub <- infile[r+1,"Response"]
      } else {question_sub <- "None"}
    }
    infile[r,"base"] <- base
    infile[r,"question"] <- question
    infile[r,"question_sub"] <- question_sub
  }
  infile <- infile[!is.na(infile$Total),]
  infile$table <- table  ; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$date <- date   ; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$jobnr <- jobnr  ; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$project <- project; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  
  infile
}

fileURL <- "https://intranet.nielsen.com/company/news/newsletters/Consumer%20Confidence%20Concerns%20and%20Spending%20Library/Q3%202015%20CCI%20AP%20REGION.xls"
fileOut <- "Q3 2015 CCI AP REGION.xls"
bindat <- getBinaryURL(fileURL,userpwd=paste(userId,":",passWd,sep=""))
writeBin(bindat,paste(datain,"/",fileOut,,sep=""))
cci1 <- rd_CCI(fileOut,nrRows = 10000, nrCols = 16)

head(cci1,n=5)
write.csv(cci1,paste(datain,"/cci1.csv",sep=""),row.names=F)
