
################## COMBINING CCI AND EIU  ############################################

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

create_cci_eiu_smart <- function(infile) {

  cci_eiu_smart <- infile[
    (
      infile$question=="DCPI" | infile$question=="DGDP" | infile$response =="Average Index" |
        grepl("Q7",infile$question) | grepl("Q6[.]",infile$question)
    )
    &
      (
        infile$year %in% c("2011","2012","2013","2014","2015") &
          !(infile$year == "2015" & infile$quarter == "Q4") &
          infile$stat=="Response"
      )
    ,]
  
  
  cci_eiu_smart[cci_eiu_smart$region=="NA","region"] <- "NA."
  
  cci_eiu_smart$value.country <- gsub("%","",x = cci_eiu_smart$value.country)
  cci_eiu_smart$value.region <- gsub("%","",x = cci_eiu_smart$value.region)
  cci_eiu_smart$value.global <- gsub("%","",x = cci_eiu_smart$value.global)
  
  cci_eiu_smart$year <- factor(cci_eiu_smart$year)
  cci_eiu_smart$quarter <- factor(cci_eiu_smart$quarter)
  cci_eiu_smart$region <- factor(cci_eiu_smart$region)
  cci_eiu_smart$category <- factor(cci_eiu_smart$category)
  cci_eiu_smart$stat <- factor(cci_eiu_smart$stat)
  cci_eiu_smart$base <- factor(cci_eiu_smart$base)
  
  cci_eiu_smart$value.country <- as.numeric(cci_eiu_smart$value.country)
  cci_eiu_smart$value.region <- as.numeric(cci_eiu_smart$value.region)
  cci_eiu_smart$value.global <- as.numeric(cci_eiu_smart$value.global)
  
  cci_eiu_smart
  
}

rank_cci_question <- function(infile,question,rankyear,rankquarter) {

  #question <- "Q7a"
  #rm(infile_Q,temp,question,question_long,order_nm)
  
  infile_Q <- infile[grepl(paste(question,".",sep=""),infile$question),]
  infile_Q <- infile_Q[infile_Q$year==rankyear & infile_Q$quarter==rankquarter,]
  question_long <- infile_Q[1,"question"]
  head(infile_Q)
  infile_Q <- ftime_agg(infile_Q,
                                agg_list = infile_Q$value.region,
                                order_list = list(infile_Q$region,infile_Q$response),
                                desc_vec = c("region","response"),
                                stat_vec = c("value.region")
  )
  
  temp <- as.data.frame(t(tapply(infile_Q$value.region, list(infile_Q$region,infile_Q$response), max)))
  temp$order.AME <- rank(-temp$AME,ties.method= "first")
  temp$order.AP <- rank(-temp$AP,ties.method= "first")
  temp$order.EU <- rank(-temp$EU,ties.method= "first")
  temp$order.LA <- rank(-temp$LA,ties.method= "first")
  temp$order.NA <- rank(-temp$NA.,ties.method= "first")
  temp$response <- row.names(temp)
  row.names(temp) <- NULL
  temp <- temp[c("response","order.NA","order.AME","order.AP","order.EU","order.LA")]
  order_nm <- paste("order",question,sep="")
  temp[,"region"] <- "NA."; temp[,order_nm] <- temp$order.NA
  tempNA <- temp; tempNA[,"region"] <- "NA."; tempNA[,order_nm] <- tempNA$order.NA
  tempAME <- temp; tempAME[,"region"] <- "AME"; tempAME[,order_nm] <- tempAME$order.AME
  tempAP <- temp; tempAP[,"region"] <- "AP"; tempAP[,order_nm] <- tempAP$order.AP
  tempEU <- temp; tempEU[,"region"] <- "EU"; tempEU[,order_nm] <- tempEU$order.EU
  tempLA <- temp; tempLA[,"region"] <- "LA"; tempLA[,order_nm] <- tempLA$order.LA
  temp <- rbind(tempNA,tempAME,tempAP,tempEU,tempLA)
  temp <- temp[c("response","region",order_nm)]
  temp$question <- question_long
  rm(tempNA,tempAME,tempAP,tempEU,tempLA)
  
  infile <- merge(infile,temp,by=c("question","response","region"),all.x=T)
  
  infile  
  
}

rank_cci <- function(infile,rankyear,rankquarter) {
  
  #infile <- cci_eiu_smart; rankyear <- "2015"; rankquarter <- "Q3"
  #rm(infile,rankquarter,rankyear,rankname)
  
  infile<- rank_cci_question(infile,"Q6",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q7a",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q7b",rankyear,rankquarter)
  
  rankname <- paste("rank",rankyear,rankquarter,sep="")
  
  infile[,rankname] <- NA
  infile[!is.na(infile$orderQ6),rankname] <- infile[!is.na(infile$orderQ6),"orderQ6"]
  infile[!is.na(infile$orderQ7a),rankname] <- infile[!is.na(infile$orderQ7a),"orderQ7a"]
  infile[!is.na(infile$orderQ7b),rankname] <- infile[!is.na(infile$orderQ7b),"orderQ7b"]
  
  infile
  
}
  
setup_CCI_EIU <- function() {

  cci_eiu_Out2 <- cci_eiu_prepData(cciOutTOTAL,eiuFile)
  
  write.csv(cci_eiu_Out2,paste(datain,"/Files OUTPUT/cci_eiu_Out2.csv",sep=""),row.names=F)
  
  cci_eiu_smart <- create_cci_eiu_smart(cci_eiu_Out2)

  #cci_eiu_smart <- rank_cci(cci_eiu_smart,"2015","Q2")
  cci_eiu_smart <- rank_cci(cci_eiu_smart,"2015","Q3")
  
  cci_eiu_smart <- cci_eiu_smart[c("year","quarter","region","country","category",
                                   "question","question_sub","stat","response","base",
                                   "value.country","value.region","value.global",
                                   "rank2015Q3")]
  
  write.csv(cci_eiu_smart,paste(datain,"/Files OUTPUT/cci_eiu_csuite.csv",sep=""),row.names=F)
  
}

setup_CCI_EIU_WB <- function() {
  
  WBFile2 <- WBFile
  WBFile2$region <- "AP"
  WBFile2$category <- "WB"
  WBFile2$stat <- "Response"
  WBFile2$response <- "Stat"
  WBFile2$base <- "Market"
  
  cci_eiu_wb <- rbind.fill(cci_eiu_Out2,WBFile2)
  
  write.csv(cci_eiu_wb,paste(datain,"/Files OUTPUT/cci_eiu_wb.csv",sep=""),row.names=F)
  
}
