
userId <- "langestrst01"
passWd <- "Devt0314"

Rdir <- "~/PROJECTS/Thought Leadership/TL_Data"

source(file.path(Rdir,"libraries.R"))

datain <- "~/PROJECTS/Thought Leadership"

fileCCIRoot <- "https://intranet.nielsen.com/company/news/newsletters/Consumer%20Confidence%20Concerns%20and%20Spending%20Library/"

rd_CCI <- function(filename,nrRows,nrCols) {
  
  #filename <- "Q3 2015 CCI AP REGION.xls"; nrRows <- 10000; nrCols <- 16
  #rm(filename,infile,dataIn,rowstart,rowend,jobnr,date,table,index,base,question,question_sub,r,nrRows,nrCols);
  
  #setwd(paste(datain,"/Files CCI/",sep=""))
  
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

downloadCCI <- function(subdir,read=F) {
  setwd(paste(datain,"/Files CCI/",subdir,sep=""))
  for (y in 2011:2015) {
    for (q in 1:4) {
      if (!(y==2015 & q==4)) {
        if (subdir == "AP") {
          if (y==2011 & q==1) {
            fileOut <- paste("Q",q," ",y," CCI - ",subdir," REGION.xls",sep="")
          } else if (y==2011 & (q==3|q==4)) {
            fileOut <- paste("Q",q," ",y," CCI RESULTS ",subdir," REGION.xls",sep="")
          } else {
            fileOut <- paste("Q",q," ",y," CCI ",subdir," REGION.xls",sep="")
          }
        } else if (subdir == "SEA") {
          if ((y==2014 & q==4)|(y==2015 & q==2)|(y==2015 & q==3)) {
            fileOut <- paste("Q",q," ",y," CCI ",subdir," REGION.xls",sep="")
          }
        } else {
          if (y==2015 & q==3) {
            fileOut <- "Q3 2015 CCI RESULTS BY COUNTRY with Morocco.xls"
          } else if (y==2011 & q==1) {
            fileOut <- paste("Q",q," ",y," CCI - RESULTS BY COUNTRY.xls",sep="")
          } else if (y==2012 & q==1) {
            fileOut <- paste("Q",q," ",y," CCI RESLUTS BY COUNTRY.xls",sep="")
          } else if (y==2012 & q==3) {
            fileOut <- paste("Q",q," ",y," CCI RESULTS BY COUNTRY REVISED FOR FF Q21.xls",sep="")
          } else {
            fileOut <- paste("Q",q," ",y," CCI RESULTS BY COUNTRY.xls",sep="")
          }
        }
        if (read==F) {
          fileURL <- paste(fileCCIRoot,gsub(" ", "%20", fileOut),sep="")
          bindat <- getBinaryURL(fileURL,userpwd=paste(userId,":",passWd,sep=""))
          writeBin(bindat,fileOut)
        } else if (read==T) {
          cci1 <- rd_CCI(fileOut,nrRows = 10000, nrCols = 16)
        }
      }
    }
  }
}

downloadCCI("AP")
downloadCCI("SEA")
downloadCCI("GLOBAL")

cci1 <- rd_CCI(fileOut,nrRows = 10000, nrCols = 16)

head(cci1,n=5)

write.csv(cci1,paste(datain,"/cci1.csv",sep=""),row.names=F)
