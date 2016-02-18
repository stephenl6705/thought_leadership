
################## CCI FUNCTIONS ############################################

addCol <- function(infile,name,value) {
  infile[,name] <- value
  infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile
}

rd_CCI <- function(filename,subdir,nrRows,nrCols) {
  
  #filename <- "Q4 2015 CCI RESULTS BY COUNTRY.xls"; nrRows <- 10000; nrCols <- 62; subdir <- "GLOBAL"
  #rm(filename,subdir,infile,dataIn,rowstart,rowend,jobnr,date,table,base,question,question_sub,r,nrRows,nrCols,sheets,sheetNr,category);
  
  setwd(paste(datain,"/Files CCI/",subdir,sep=""))
  getwd()
  dataIn <- loadWorkbook(filename)
  
  sheets <- getSheets(dataIn)
  
  outfile <- ""
  
  for (sheetNr in 1:length(sheets)) {
    
    #sheetNr <- 1
    
    infile = readWorksheet(dataIn, sheet = sheets[sheetNr],useCachedValues=FALSE,
                           startRow=1, endRow=nrRows, startCol=1, endCol=nrCols, header=F)
    if (sheetNr == 1) {category <- "CCI"} else {category <- sheets[sheetNr]}
    infile <- infile[!is.na(infile$Col1),]
    project <- infile[1,"Col1"]
    jobnr <- infile[2,"Col1"]
    date <- infile[3,"Col1"]
    table <- infile[4,"Col1"]
    question <- infile[5,"Col1"]
    if (grepl("Base",infile[6,"Col1"])) {
      question_sub <- "None"
      base <- infile[6,"Col1"];
    } else {
      question_sub <- infile[6,"Col1"]
      base <- infile[7,"Col1"];
    }
    infile = readWorksheet(dataIn, sheet = sheets[sheetNr],useCachedValues=FALSE,
                           startRow=1, endRow=nrRows, startCol=1, endCol=nrCols, header=F)
    infile <- infile[!is.na(infile$Col2),]
    rowend <- as.numeric(rows[length(rownames(infile))])
    infile <- infile[grepl("Total",infile$Col2)|grepl("Global.Avg",infile$Col2),]
    rowstart <- as.numeric(rownames(infile[1,]))
    infile = readWorksheet(dataIn, sheet = sheets[sheetNr],useCachedValues=FALSE,
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
    
    infile <- addCol(infile,"base",base)
    infile <- addCol(infile,"question_sub",question_sub)
    infile <- addCol(infile,"question",question)
    infile <- addCol(infile,"table",table)
    infile <- addCol(infile,"date",date)
    
    for (r in 1:nrow(infile)) {
      if (grepl("Base:",infile[r,"Response"]) & !grepl("wtd",tolower(infile[r,"Response"]))) {
        base <- infile[r,"Response"]
      } else if (grepl("Table [123456789]",infile[r,"Response"])) {
        date <- infile[r-1,"Response"]
        table <- infile[r,"Response"]
        question <- infile[r+1,"Response"]
        if (!grepl("^Base",infile[r+2,"Response"])) {
          question_sub <- infile[r+2,"Response"]
        }
      }
      infile[r,"date"] <- date
      infile[r,"table"] <- table
      infile[r,"base"] <- base
      infile[r,"question"] <- question
      infile[r,"question_sub"] <- question_sub
    }
    infile <- infile[!is.na(infile$Total),]
    
    infile <- addCol(infile,"category",category)
    infile <- addCol(infile,"jobnr",jobnr)
    infile <- addCol(infile,"project",project)
    
    if (is.data.frame(outfile)) {
      outfile <- rbind.fill(outfile,infile)
    } else {
      outfile <- infile
    }
    
  }
  
  outfile <- outfile[!grepl("SUMMARY",outfile$question),]
  outfile
  
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
  
  if ((year==2014 & quarter==4)|(year==2015 & quarter==2)|(year==2015 & quarter==3) {
    fileOut <- paste("Q",quarter," ",year," CCI ",subdir," REGION.xls",sep="")
  }
  
  fileOut
}

getFileGLOBAL <- function(year,quarter) {
  
  fileOut <- ""; subdir <- "GLOBAL"
  
  if (year==2015 & quarter==3) {
    fileOut <- "Q3 2015 CCI RESULTS BY COUNTRY with Morocco.xls"
  } else if (year==2015 & quarter==4) {
    fileOut <- "Q4 2015 CCI RESULTS BY COUNTRY_REVISED 24Dec15.xls"
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

getFileREGION <- function(year,quarter) {
  
  fileOut <- ""; subdir <- "REGION"
  
  if (year==2015 & quarter==3) {
    fileOut <- "Q3 2015 CCI RESULTS BY REGION with Morocco.xls"
  } else if (year==2015 & quarter==4) {
    fileOut <- "Q4 2015 CCI RESULTS BY REGION_REVISED 24Dec15.xls"
  } else if (year==2011 & quarter==1) {
    fileOut <- "Q1 2011 CCI - Regional Results.xls"
  } else if (year==2012 & quarter==3) {
    fileOut <- "Q3 2012 CCI RESULTS BY REGION REVISED FOR FF Q21.xls"
  } else {
    fileOut <- paste("Q",quarter," ",year," CCI RESULTS BY REGION.xls",sep="")
  }
  
  fileOut
}

downloadCCI <- function(subdir,read=F) {
  #subdir <- "GLOBAL"; read <- F
  #rm(subdir,read,cciOut,y,q,fileOut,fileURL,bindat,temp)
  setwd(paste(datain,"/Files CCI/",subdir,sep=""))
  cciOut <- ""
  nrRows <- 10000; nrCols <- 62
  for (y in 2011:2015) {
    y <- 2015
    for (q in 1:4) {
      q <- 4
      fileOut <- ""
      #if (!(y==2015 & q==4)) {
        if (subdir == "AP") {
          fileOut <- getFileAP(y,q)
        } else if (subdir == "SEA") {
          fileOut <- getFileSEA(y,q)
        } else if (subdir == "REGION") {
          fileOut <- getFileREGION(y,q)
        } else {
          fileOut <- getFileGLOBAL(y,q)
        }
        if (!read) {
          fileURL <- paste(fileCCIRoot,gsub(" ", "%20", fileOut),sep="")
          bindat <- getBinaryURL(fileURL,userpwd=paste(userId,":",passWd,sep=""))
          writeBin(bindat,fileOut)
        } else if (read & !(fileOut == "")) {
          temp <- rd_CCI(fileOut,subdir,nrRows, nrCols)
          temp <- addCols(temp,fileOut,y,q)
          if (is.data.frame(cciOut)) {
            cciOut <- rbind.fill(cciOut,temp)
          } else {
            cciOut <- temp
          }
        }
      #}
    }
  }
  cciOut$region <- subdir
  cciOut <- cciOut[c(ncol(cciOut),1:ncol(cciOut)-1)]
  cciOut
}

downloadCCI_yq <- function(subdir,read=F, y, q) {
  #subdir <- "GLOBAL"; read <- F; y <- 2015; q <- 4
  #rm(subdir,read,cciOut,y,q,fileOut,fileURL,bindat,temp)
  setwd(paste(datain,"/Files CCI/",subdir,sep=""))
  cciOut <- ""
  nrRows <- 10000; nrCols <- 62
  fileOut <- ""
  if (subdir == "AP") {
    fileOut <- getFileAP(y,q)
  } else if (subdir == "SEA") {
    fileOut <- getFileSEA(y,q)
  } else if (subdir == "REGION") {
    fileOut <- getFileREGION(y,q)
  } else {
    fileOut <- getFileGLOBAL(y,q)
  }

  fileURL <- paste(fileCCIRoot,gsub(" ", "%20", fileOut),sep="")
  bindat <- getBinaryURL(fileURL,userpwd=paste(userId,":",passWd,sep=""))
  writeBin(bindat,fileOut)

}

formatCols <- function() {

  cciOutTOTAL$year <- factor(cciOutTOTAL$year)
  cciOutTOTAL$quarter <- factor(cciOutTOTAL$quarter)
  cciOutTOTAL$category <- factor(cciOutTOTAL$category)
  cciOutTOTAL$question <- factor(cciOutTOTAL$question)
  cciOutTOTAL$question_sub <- factor(cciOutTOTAL$question_sub)
  cciOutTOTAL$stat <- factor(cciOutTOTAL$stat)
  cciOutTOTAL$Response <- factor(cciOutTOTAL$Response)
  cciOutTOTAL$base <- factor(cciOutTOTAL$base)
  cciOutTOTAL$region.x <- factor(cciOutTOTAL$region.x)
  cciOutTOTAL$region.y <- factor(cciOutTOTAL$region.y)
  cciOutTOTAL$sourceFile.x <- factor(cciOutTOTAL$sourceFile.x)
  cciOutTOTAL$sourceFile.y <- factor(cciOutTOTAL$sourceFile.y)
  cciOutTOTAL$project.x <- factor(cciOutTOTAL$project.x)
  cciOutTOTAL$project.y <- factor(cciOutTOTAL$project.y)
  cciOutTOTAL$jobnr.x <- factor(cciOutTOTAL$jobnr.x)
  cciOutTOTAL$jobnr.y <- factor(cciOutTOTAL$jobnr.y)
  cciOutTOTAL$table.x <- factor(cciOutTOTAL$table.x)
  cciOutTOTAL$table.y <- factor(cciOutTOTAL$table.y)
  
  for (i in 15:77) {
    cciOutTOTAL[,i] <- as.numeric(cciOutTOTAL[,i])
  }
  for (i in 84:91) {
    cciOutTOTAL[,i] <- as.numeric(cciOutTOTAL[,i])
  }
  
  #cciOutTOTAL$date.x <- as.Date(cciOutTOTAL$date.x)
  #str(cciOutTOTAL)
  
}

setupCCI <- function() {
  
  fileCCIRoot <- "https://intranet.nielsen.com/company/news/newsletters/Consumer%20Confidence%20Concerns%20and%20Spending%20Library/"

  downloadCCI_yq("AP",read=F,y = 2015,q = 4)
  downloadCCI_yq("SEA",read=F,y = 2015,q = 4)
  # NOTE: FILES ARE IN XLSX FORMAT, SO DOES NOT DOWNLOAD AUTOMATICALLY
  downloadCCI_yq("GLOBAL",read=F,y = 2015,q = 4)
  downloadCCI_yq("REGION",read=F,y = 2015,q = 4)
  
  cciOutAP <- downloadCCI("AP",read=F)
  cciOutSEA <- downloadCCI("SEA",read=F)
  cciOutGLOBAL <- downloadCCI("GLOBAL",read=F)
  cciOutREGION <- downloadCCI("REGION",read=F)
  
  cciOutAP <- downloadCCI("AP",read=T)
  cciOutSEA <- downloadCCI("SEA",read=T)
  cciOutGLOBAL <- downloadCCI("GLOBAL",read=T)
  cciOutREGION <- downloadCCI("REGION",read=T)
  cciOut <- rbind.fill(cciOutAP,cciOutSEA,cciOutGLOBAL)
  cciOut[grepl("index by",cciOut$question_sub),"question_sub"] <- "None"
  
  #nrow(cciOut)
  #write.csv(cciOut,paste(datain,"/Files OUTPUT/cciOut.csv",sep=""),row.names=F)
  
  #nrow(cciOutREGION)
  #dupes <- duplicated(cciOutREGION[,c("region","year","quarter","category","question","question_sub","Response","base","date")])
  #nrow(cciOutREGION[dupes,])
  #write.csv(dupes,paste(datain,"/Files OUTPUT/dupes.csv",sep=""),row.names=F)
  dupes <- duplicated(cciOutREGION[,c("region","year","quarter","category","question","question_sub","Response","base")])
  cciOutREGION <- cciOutREGION[!dupes, ]
  cciOutREGION[grepl("index by",cciOutREGION$question_sub),"question_sub"] <- "None"
  #nrow(cciOutREGION)
  #rm(dupes)
  
  cciOut[grepl("by ap",cciOut$question_sub),"question_sub"] <- "None"
  cciOut[grepl("by sea",cciOut$question_sub),"question_sub"] <- "None"
  cciOut[grepl("by country",cciOut$question_sub),"question_sub"] <- "None"
  cciOutREGION[grepl("by region",cciOutREGION$question_sub),"question_sub"] <- "None"
  
  cciOutTOTAL <- merge(cciOut,cciOutREGION,by=c("year","quarter","category","question","question_sub","Response","base"),all.x=TRUE)
  #nrow(cciOutTOTAL)
  
  cciOutTOTAL <- addCol(cciOutTOTAL,"stat","Response")
  cciOutTOTAL <- cciOutTOTAL[c(2:6,1,7:length(cciOutTOTAL))]
  cciOutTOTAL[grepl("^BASE",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^SUM$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^S.D.$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^SE$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^TOP 2 BOX$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^BOTTOM 2 BOX$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^MEAN$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^TOTAL$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^MEDIAN$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^SD$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  cciOutTOTAL[grepl("^AVG$",toupper(cciOutTOTAL$Response)),"stat"] <- "summary"
  
  write.csv(cciOutTOTAL,paste(datain,"/Files OUTPUT/cciOutTOTAL.csv",sep=""),row.names=F)
  
  cciOutTOTAL_sample <- cciOutTOTAL[c("region.x","stat","year","quarter","question","question_sub","Response",
                                      "AU","CN","HK","IN","ID","KO","MY","NZ","PH","SG","TW","TH","VN","JP","AP")]
  
  cciOutTOTAL_sample <- cciOutTOTAL_sample[cciOutTOTAL_sample$region.x=="GLOBAL",]
  cciOutTOTAL_sample <- cciOutTOTAL_sample[cciOutTOTAL_sample$stat=="Response",]
  cciOutTOTAL_sample <- cciOutTOTAL_sample[!cciOutTOTAL_sample$question=="Q69. City /Country",]
  
  names(cciOutTOTAL_sample)[names(cciOutTOTAL_sample)=="AP"] <- "AP region"
  
  cciOutTOTAL_sample <- cciOutTOTAL_sample[c("year","quarter","question","question_sub","Response",
                                             "AU","CN","HK","IN","ID","KO","MY","NZ","PH","SG","TW","TH","VN","JP","AP region")]

  str(cciOutTOTAL_sample)
  cciOutTOTAL_sample <- melt(cciOutTOTAL_sample,
                       id.vars = c("year","quarter","question","question_sub","Response"),
                       measure.vars = names(cciOutTOTAL_sample)[6:length(cciOutTOTAL_sample)])
  
  names(cciOutTOTAL_sample)[names(cciOutTOTAL_sample)=="variable"] <- "geography"
  names(cciOutTOTAL_sample)[names(cciOutTOTAL_sample)=="Response"] <- "response"
  
  cciOutTOTAL_sample$value <- gsub("%","",x = cciOutTOTAL_sample$value)
  cciOutTOTAL_sample$value <- as.numeric(cciOutTOTAL_sample$value)
  
  #cciOutTOTAL_sample[cciOutTOTAL_sample$question=="Index. CONSUMER CONFIDENCE INDEX","value"] <-
  #  cciOutTOTAL_sample[cciOutTOTAL_sample$question=="Index. CONSUMER CONFIDENCE INDEX","value"]/100
  
  write.csv(cciOutTOTAL_sample,paste(datain,"/Files OUTPUT/cciOutTOTAL_sample.csv",sep=""),row.names=F)

}
