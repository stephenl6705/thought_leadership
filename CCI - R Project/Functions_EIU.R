
################## EIU FUNCTIoNS ############################################


eiu_readData <- function(period) {

  #period <- "2015_12_03_Q"
  #rm(period,dataIn,infile,lastRow)
  
  setwd(paste(datain,"/Files EIU/",sep=""))
  
  dataIn <- loadWorkbook(paste("eiu_",period,".xls",sep=""))
  
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=4, endRow=10000, startCol=1, endCol=102, header=T)
  names(infile)[4] <- "Series"
  infile <- infile[!is.na(infile$Series),]
  lastRow <- as.numeric(row.names(infile[infile$Series=="Notes",])) - 1
  
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=4, endRow=lastRow, startCol=1, endCol=102, header=T)
  names(infile)[2] <- "Country_sn"
  names(infile)[4] <- "Series"
  infile <- infile[!is.na(infile$Series),]
  
  infile <- melt(infile,
                 id.vars = c("Country","Country_sn","Series.Title","Series","Currency","Units"),
                 measure.vars = names(infile)[9:length(names(infile))-2])
  
  names(infile)[names(infile)=="variable"] <- "period"
  infile$year <- substr(infile$period,2,5)
  infile$quarter <- substr(infile$period,6,7)
  infile[infile$Country_sn=="KR","Country_sn"] <- "KO"

  eiuFile <- cast(infile, year + quarter + Series + Series.Title ~ Country_sn, sum)
  eiuFile$category <- "EIU"
  names(eiuFile)[names(eiuFile)=="Series"] <- "question"
  names(eiuFile)[names(eiuFile)=="Series.Title"] <- "question_sub"
  eiuFile$stat <- "Response"
  eiuFile$Response <- "Stat"
  eiuFile$base <- "Market"
  eiuFile$region.x <- "GLOBAL"
  eiuFile$project.x <- "The Economist Intelligence Unit"
  eiuFile$jobnr.x <- paste("eiu_",period,sep="")
  eiuFile$date.x <- period
  
  eiuFile
  
}

setupEIU <- function() {

  eiuFileQ <- eiu_readData("2015_12_03_Q")
  write.csv(eiuFileQ,paste(datain,"/Files OUTPUT/eiuOut_Q.csv",sep=""),row.names=F)
  
  eiuFileY <- eiu_readData("2015_12_03_Y")
  write.csv(eiuFileY,paste(datain,"/Files OUTPUT/eiuOut_Y.csv",sep=""),row.names=F)
  
  eiuFile <- rbind.fill(eiuFileQ,eiuFileY)
  write.csv(eiuFile,paste(datain,"/Files OUTPUT/eiuOut.csv",sep=""),row.names=F)
  
  
}

ftime_agg <- function (file,agg_list,order_list,desc_vec,stat_vec) {
  time_agg <-
    with(file,
         aggregate(
           agg_list
           ,by=order_list
           ,FUN=mean
         )
    )
  
  max <- length(desc_vec)
  for (i in 1:max) {
    names(time_agg)[i]<-paste(desc_vec[i])
  }
  max2 <- length(stat_vec)
  for (i in 1:max2) {
    names(time_agg)[max+i]<-paste(stat_vec[i])
  }
  time_agg <- time_agg[order(time_agg[1]),]
  time_agg
}
