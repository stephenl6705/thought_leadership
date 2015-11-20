
################## EIU FUNCTIoNS ############################################


eiu_readDate <- function(period) {
  
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

cci_eiu_prepData <- function(cciInputFile,eiuInputFile) {
  
  cci_eiu_Out <- cciInputFile[cciInputFile$category=="CCI" & cciInputFile$region.x=="GLOBAL",]
  
  cci_eiu_Out <- rbind.fill(cci_eiu_Out,eiuInputFile)
  
  cci_eiu_Out[cci_eiu_Out$question_sub=="index by ap","question_sub"] <- "None"
  
  write.csv(cci_eiu_Out,paste(datain,"/Files OUTPUT/cci_eiu_Out.csv",sep=""),row.names=F)
  
  cci_eiu_Out2 <- melt(cci_eiu_Out,
                       id.vars = c("year","quarter","category","question","question_sub","stat","Response","base","region.x",
                                   "project.x","jobnr.x","date.x","table.x","Total.x",
                                   "region.y","sourceFile.y","project.y","jobnr.y","date.y","table.y","Total.y",
                                   "AP","EU","MEAP","LA","NA.","AME","SEA"),
                       measure.vars = names(cci_eiu_Out)[16:77])
  
  names(cci_eiu_Out2)[names(cci_eiu_Out2)=="variable"] <- "country"
  names(cci_eiu_Out2)[names(cci_eiu_Out2)=="Response"] <- "response"
  names(cci_eiu_Out2)[names(cci_eiu_Out2)=="value"] <- "value.country"
  names(cci_eiu_Out2)[names(cci_eiu_Out2)=="Total.x"] <- "value.global"
  names(cci_eiu_Out2)[names(cci_eiu_Out2)=="region.x"] <- "region"
  
  cci_eiu_Out2[cci_eiu_Out2$country == "PERU","country"] <- "PE"
  
  cci_eiu_Out2[cci_eiu_Out2$country %in%
                 c("AU","CN","HK","ID","IN","JP","KO","MY","NZ","PH","SG","TH","TW","VN"),"region"] <- "AP"
  
  cci_eiu_Out2[cci_eiu_Out2$country %in%
                 c("AT","BE","CH","CZ","DE","DK","EE","ES","FI","FR","GB","GR","HR","HU",
                   "IE","IL","IT","LT","LV","NL","NO","PL","PT","RO","RU","SE","TR","UA",
                   "BG","SK","RS","SI"),"region"] <- "EU"
  
  cci_eiu_Out2[cci_eiu_Out2$country %in%
                 c("ZA","AE","EG","PK","SA","MO"),"region"] <- "AME"
  
  cci_eiu_Out2[cci_eiu_Out2$country %in%
                 c("BR","MX","AR","CO","CL","VE","PE"),"region"] <- "LA"
  
  cci_eiu_Out2[cci_eiu_Out2$country %in%
                 c("CA","US"),"region"] <- "NA"
  
  cci_eiu_Out2[cci_eiu_Out2$region=="AP","value.region"] <- cci_eiu_Out2[cci_eiu_Out2$region=="AP","AP"]
  cci_eiu_Out2[cci_eiu_Out2$region=="EU","value.region"] <- cci_eiu_Out2[cci_eiu_Out2$region=="EU","EU"]
  cci_eiu_Out2[cci_eiu_Out2$region=="AME","value.region"] <- cci_eiu_Out2[cci_eiu_Out2$region=="AME","AME"]
  cci_eiu_Out2[cci_eiu_Out2$region=="LA","value.region"] <- cci_eiu_Out2[cci_eiu_Out2$region=="LA","LA"]
  cci_eiu_Out2[cci_eiu_Out2$region=="NA","value.region"] <- cci_eiu_Out2[cci_eiu_Out2$region=="NA","NA."]
  
  cci_eiu_Out2 <- cci_eiu_Out2[c("year","quarter","region","country","category","question","question_sub",
                                 "stat","response","base","value.country","value.region","value.global")]
  
  cci_eiu_Out2 <- cci_eiu_Out2[!is.na(cci_eiu_Out2$value.country),]
  cci_eiu_Out2[grepl("-",cci_eiu_Out2$value.country),"value.country"] <- NA
  cci_eiu_Out2[grepl("-",cci_eiu_Out2$value.region),"value.region"] <- NA
  cci_eiu_Out2[grepl("-",cci_eiu_Out2$value.global),"value.global"] <- NA
  #cci_eiu_Out2[is.na(cci_eiu_Out2$value.region),"value.region"] <- cci_eiu_Out2[is.na(cci_eiu_Out2$value.region),"value.country"]
  #cci_eiu_Out2[is.na(cci_eiu_Out2$value.global),"value.global"] <- cci_eiu_Out2[is.na(cci_eiu_Out2$value.global),"value.country"]
  
  write.csv(cci_eiu_Out2,paste(datain,"/Files OUTPUT/cci_eiu_Out2.csv",sep=""),row.names=F)
  
  cci_eiu_Out2
  
}

setupEIU <- function() {

  eiuFile <- eiu_readDate("2015_11_10")
  
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
