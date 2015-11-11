
userId <- "langestrst01"
passWd <- "Devt0314"

Rdir <- "~/PROJECTS/Thought Leadership/TL_Data"

source(file.path(Rdir,"libraries.R"))

datain <- "~/PROJECTS/Thought Leadership"

fileCCIRoot <- "https://intranet.nielsen.com/company/news/newsletters/Consumer%20Confidence%20Concerns%20and%20Spending%20Library/"

rd_CCI <- function(filename,subdir,nrRows,nrCols) {
  
  #filename <- "Q1 2011 CCI - AP REGION.xls"; nrRows <- 10000; nrCols <- 16; subdir <- "AP"
  #rm(filename,subdir,infile,dataIn,rowstart,rowend,jobnr,date,table,index,base,question,question_sub,r,nrRows,nrCols);
  
  setwd(paste(datain,"/Files CCI/",subdir,sep=""))
  
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
  names(infile)[names(infile)=="Global.Avg"] <- "Total"
  names(infile)[names(infile)=="Indonesia..ID."] <- "ID"
  names(infile)[names(infile)=="Malaysia..MY."] <- "MY"
  names(infile)[names(infile)=="Philippines..PH."] <- "PH"
  names(infile)[names(infile)=="Singapore..SG."] <- "SG"
  names(infile)[names(infile)=="Thailand..TH."] <- "TH"
  names(infile)[names(infile)=="Vietnam..VN."] <- "VN"
  infile <- infile[,names(infile)[!grepl("Col",names(infile))]]
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
  head(infile)
  infile <- infile[!is.na(infile$Total),]
  infile$table <- table  ; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$date <- date   ; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$jobnr <- jobnr  ; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$project <- project; infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  
  infile
}

addCols <- function (infile,sourceFile,year,quarter) {
  infile$quarter <- paste("Q",quarter,sep="")
  infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$year <- year
  infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$sourceFile <- sourceFile
  infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile
}

getFileAP <- function(year,quarter) {
  
  fileOut <- ""; subdir <- "AP"
  
  if (year==2011 & quarter==1) {
    fileOut <- paste("Q",quarter," ",year," CCI - ",subdir," REGION.xls",sep="")
  } else if (year==2011 & quarter==3) {
    fileOut <- paste("Q",quarter," ",year," CCI RESULTS ",subdir," REGION.xls",sep="")
  } else if (year==2011 & quarter==4) {
    fileOut <- "Q4 2011 CCI_Asia Pacific Region.xls"
  } else if (year==2015 & quarter==1) {
  } else {
    fileOut <- paste("Q",quarter," ",year," CCI ",subdir," REGION.xls",sep="")
  }
  
    fileOut
}

getFileSEA <- function(year,quarter) {
  
  fileOut <- ""; subdir <- "SEA"
  
  if ((year==2014 & quarter==4)|(year==2015 & quarter==2)|(year==2015 & quarter==3)) {
    fileOut <- paste("Q",quarter," ",year," CCI ",subdir," REGION.xls",sep="")
  }
  
  fileOut
}

getFileGLOBAL <- function(year,quarter) {
  
  fileOut <- ""; subdir <- "GLOBAL"
  
  if (year==2015 & quarter==3) {
    fileOut <- "Q3 2015 CCI RESULTS BY COUNTRY with Morocco.xls"
  } else if (year==2011 & quarter==1) {
    fileOut <- paste("Q",quarter," ",year," CCI - RESULTS BY COUNTRY.xls",sep="")
  } else if (year==2012 & quarter==1) {
    fileOut <- paste("Q",quarter," ",year," CCI RESLUTS BY COUNTRY.xls",sep="")
  } else if (year==2012 & quarter==3) {
    fileOut <- paste("Q",quarter," ",year," CCI RESULTS BY COUNTRY REVISED FOR FF Q21.xls",sep="")
  } else {
    fileOut <- paste("Q",quarter," ",year," CCI RESULTS BY COUNTRY.xls",sep="")
  }
  
  fileOut
}

downloadCCI <- function(subdir,read=F) {
  #subdir <- "AP"; read <- T
  #rm(subdir,read,cciOut,y,q,fileOut,fileURL,bindat)
  setwd(paste(datain,"/Files CCI/",subdir,sep=""))
  cciOut <- ""
  nrRows <- 10000; nrCols <- 62
  for (y in 2011:2015) {
    #y <- 2011
    for (q in 1:4) {
      #q <- 1
      fileOut <- ""
      if (!(y==2015 & q==4)) {
        if (subdir == "AP") {
          fileOut <- getFileAP(y,q)
        } else if (subdir == "SEA") {
          fileOut <- getFileSEA(y,q)
        } else {
          fileOut <- getFileGLOBAL(y,q)
        }
        if (read==F) {
          fileURL <- paste(fileCCIRoot,gsub(" ", "%20", fileOut),sep="")
          bindat <- getBinaryURL(fileURL,userpwd=paste(userId,":",passWd,sep=""))
          writeBin(bindat,fileOut)
        } else if (read==T & !(fileOut == "")) {
          if (is.data.frame(cciOut)) {
            temp <- rd_CCI(fileOut,subdir,nrRows, nrCols)
            temp <- addCols(temp,fileOut,y,q)
            cciOut <- rbind.fill(cciOut,temp)
          } else {
            cciOut <- rd_CCI(fileOut,subdir,nrRows, nrCols)
            cciOut <- addCols(cciOut,fileOut,y,q)
          }
        }
      }
    }
    #head(cciOut[9:length(cciOut)])
    head(cciOut)
  }
  cciOut$region <- subdir
  cciOut <- cciOut[c(ncol(cciOut),1:ncol(cciOut)-1)]
  cciOut
}

cciOutAP <- downloadCCI("AP",read=T)
cciOutSEA <- downloadCCI("SEA",read=T)
cciOutGLOBAL <- downloadCCI("GLOBAL",read=T)

cciOut <- rbind.fill(cciOutAP,cciOutSEA,cciOutGLOBAL)

write.csv(cciOut,paste(datain,"/Files OUTPUT/cciOut.csv",sep=""),row.names=F)
