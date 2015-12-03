
################## WB FUNCTIoNS ############################################

WB_countrySN <- function(infile) {
  
  infile$country <- ""
  
  infile[infile$Country.Name=="Australia","country"] <- "AU"
  infile[infile$Country.Name=="China","country"] <- "CN"
  infile[infile$Country.Name=="Hong Kong SAR, China","country"] <- "HK"
  infile[infile$Country.Name=="India","country"] <- "IN"
  infile[infile$Country.Name=="Indonesia","country"] <- "ID"
  infile[infile$Country.Name=="Japan","country"] <- "JP"
  infile[infile$Country.Name=="Korea, Dem. Rep.","country"] <- "KO"
  infile[infile$Country.Name=="Malaysia","country"] <- "MY"
  infile[infile$Country.Name=="New Zealand","country"] <- "NZ"
  infile[infile$Country.Name=="Philippines","country"] <- "PH"
  infile[infile$Country.Name=="Singapore","country"] <- "SG"
  infile[infile$Country.Name=="Thailand","country"] <- "TH"
  infile[infile$Country.Name=="Vietnam","country"] <- "VN"
  
  infile
  
}

WB_year <- function(infile) {
  
  infile$year <- 0
  
  for (y in 2011:2015) {
  
    infile[grepl(paste(y),infile$variable),"year"] <- y
    
  }
  
  infile
  
}

WB_readData <- function(period) {
  
  #rm(infile,period,dataIn)
  
  setwd(paste(datain,"/Files Worldbank/",sep=""))
  
  dataIn <- loadWorkbook(paste("Worldbank ",period,".xls",sep=""))
  
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=1, endRow=18000, startCol=1, endCol=9, header=T)
  infile <- infile[!is.na(infile$Country.Name),]
  infile <- WB_countrySN(infile)
  names(infile)[names(infile)=="Series.Code"] <- "question"
  names(infile)[names(infile)=="Series.Name"] <- "question_sub"
  infile <- melt(infile,
                 id.vars = c("country","question","question_sub"),
                 measure.vars = names(infile)[grep("YR",names(infile))])
  
  infile <- WB_year(infile)
  names(infile)[names(infile)=="value"] <- "value.country"
  infile <- infile[c("year","country","question","question_sub","value.country")]
  infile <- infile[!is.na(infile$question),]
  
  write.csv(infile,paste(datain,"/Files OUTPUT/WorldbankOut.csv",sep=""),row.names=F)
  
  infile
  
}

setupWB <- function() {

  WBFile <- WB_readData("2015-12")
  
}