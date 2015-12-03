
################## EIU FUNCTIoNS ############################################


eiu_readData <- function(period) {
  
  setwd(paste(datain,"/Files EIU/",sep=""))
  
  dataIn <- loadWorkbook(paste("eiu_",period,".xls",sep=""))
  
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=4, endRow=500, startCol=1, endCol=50, header=T)
  names(infile)[2] <- "Country_sn"
  names(infile)[4] <- "Series"
  infile <- infile[!is.na(infile$X2015Q2),]
  infile <- melt(infile,
                 id.vars = c("Country","Country_sn","Series.Title","Series","Currency","Units"),
                 measure.vars = names(infile)[9:length(names(infile))-2])
  
  names(infile)[names(infile)=="variable"] <- "period"
  infile$year <- substr(infile$period,2,5)
  infile$quarter <- substr(infile$period,6,7)
  infile[infile$Country_sn=="KR","Country_sn"] <- "KO"
  head(infile)
  head(cciOutTOTAL)
  
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
  
  write.csv(eiuFile,paste(datain,"/Files OUTPUT/eiuOut.csv",sep=""),row.names=F)
  
  eiuFile
  
}

setupEIU <- function() {

  eiuFile <- eiu_readData("2015_11_10")
  
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
