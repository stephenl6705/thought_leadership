
################## COMBINING CCI AND EIU  ############################################

cci_eiu_prepData <- function(cciInputFile,eiuInputFile) {

  #cciInputFile <- cciOutTOTAL; eiuInputFile <- eiuFile
  #rm(cciInputFile,eiuInputFile,dupes,cci_eiu_Out,cci_eiu_Out2)
  
  cciInputFile[cciInputFile$region.x=="GLOBAL","region.x"] <- "1.GLOBAL"
  cciInputFile[cciInputFile$region.x=="AP","region.x"] <- "2.AP"
  cciInputFile[cciInputFile$region.x=="SEA","region.x"] <- "3.SEA"
  
  cciInputFile <- cciInputFile[order(cciInputFile$region.x,
                                     cciInputFile$year,cciInputFile$quarter,
                                     cciInputFile$category,cciInputFile$question,cciInputFile$question_sub,
                                     cciInputFile$stat,cciInputFile$Response,cciInputFile$base,
                                     cciInputFile$jobnr.x,cciInputFile$table.x),]
  
  dupes <- duplicated(cciInputFile[,c("year","quarter",
                                      "category","question","question_sub",
                                      "stat","Response","base","jobnr.x","table.x")])

  #cciInputDupes <- cciInputFile[dupes,]
  #cciInputDupes <- cciInputDupes[order(cciInputDupes$year,cciInputDupes$quarter,cciInputDupes$category,cciInputDupes$question,
  #                       cciInputDupes$question_sub,cciInputDupes$stat,cciInputDupes$Response,cciInputDupes$base,
  #                       cciInputDupes$jobnr.x,cciInputDupes$table.x),]
  #write.csv(cciInputDupes,paste(datain,"/Files OUTPUT/cciInputDupes.csv",sep=""),row.names=F)
  
  cciInputFile <- cciInputFile[!dupes, ]
  
  cciInputFile[cciInputFile$region.x=="1.GLOBAL","region.x"] <- "GLOBAL"
  cciInputFile[cciInputFile$region.x=="2.AP","region.x"] <- "AP"
  cciInputFile[cciInputFile$region.x=="3.SEA","region.x"] <- "SEA"
  
  #cci_eiu_Out <- cciInputFile[cciInputFile$category=="CCI",]
  cci_eiu_Out <- cciInputFile
  
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
  
  #cci_eiu_Out2[cci_eiu_Out2$region=="AP","value.region"] <- cci_eiu_Out2[cci_eiu_Out2$region=="AP","value.global"]
  cci_eiu_Out2[cci_eiu_Out2$region %in% c("AP","SEA"),"value.global"] <- NA

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
  
  #head(cci_eiu_Out2[
  #  cci_eiu_Out2$country=="TW" & 
  #    cci_eiu_Out2$year==2015 & 
  #    cci_eiu_Out2$stat=="Response" & 
  #    cci_eiu_Out2$category=="EIU" & 
  #    cci_eiu_Out2$quarter=="Q3",],n=100)
  
  cci_eiu_Out2 <- cci_eiu_Out2[!is.na(cci_eiu_Out2$value.country),]
  #cci_eiu_Out2[grepl("-",cci_eiu_Out2$value.country),"value.country"] <- NA
  #cci_eiu_Out2[grepl("-",cci_eiu_Out2$value.region),"value.region"] <- NA
  #cci_eiu_Out2[grepl("-",cci_eiu_Out2$value.global),"value.global"] <- NA
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

create_cci_dashboard <- function(infile) {

  #infile <- cci_eiu_Out2
  #rm(infile)

  cci_dashboard <- infile[toupper(infile$category) %in% c("CCI","CONSUMER DEMAND","CORPORATE CITIZENSHIP","CSR"),]
  cci_dashboard <- cci_dashboard[(cci_dashboard$year %in% c("2011","2012","2013","2014","2015") &
                             !(cci_dashboard$year == "2015" & cci_dashboard$quarter == "Q4") &
                             cci_dashboard$stat=="Response"),]
  
  cci_dashboard <- cci_dashboard[
      (toupper(cci_dashboard$category) %in% c("CCI") & (
          toupper(cci_dashboard$response) =="AVERAGE INDEX" |
          grepl("Q7",toupper(cci_dashboard$question)) | grepl("Q6[.]",toupper(cci_dashboard$question)) |
          grepl("Q3[.]",toupper(cci_dashboard$question)) | grepl("Q4[.]",toupper(cci_dashboard$question)) |
          grepl("Q5[.]",toupper(cci_dashboard$question)) |
          grepl("Q8[.]",toupper(cci_dashboard$question)) | grepl("Q9[.]",toupper(cci_dashboard$question))
        )
      ) |
      (toupper(cci_dashboard$category) %in% c("CONSUMER DEMAND") & (
        grepl("Q13[ABC]SUM",toupper(cci_dashboard$question)) | grepl("Q19[ABC]SUM",toupper(cci_dashboard$question))
        )
      ) |
      (toupper(cci_dashboard$category) %in% c("CORPORATE CITIZENSHIP","CSR") & (
        grepl("Q22",toupper(cci_dashboard$question)) | grepl("Q16",toupper(cci_dashboard$question))
        )
      )
    ,]
  
  cci_dashboard[grepl("[Qq]13",cci_dashboard$question),"question"] <- gsub("13", "19", cci_dashboard[grepl("[Qq]13",cci_dashboard$question),"question"])
  cci_dashboard[grepl("[Qq]19",cci_dashboard$question),"question"] <- gsub("q", "Q", cci_dashboard[grepl("[Qq]19",cci_dashboard$question),"question"])
  is.na(cci_dashboard$value.region)

  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_10", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_9", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_8", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_7", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_6", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_5", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_4", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_3", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_2", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])
  cci_dashboard[grepl("Q22",cci_dashboard$question),"question"] <- gsub("_1", "", cci_dashboard[grepl("Q22",cci_dashboard$question),"question"])

  cci_dashboard$question_sub <- gsub(" - Businesses do enough to support society", "(R10)   Businesses do enough to support society ", cci_dashboard$question_sub)
  cci_dashboard$question_sub <- gsub(" - Businesses solve global problems through their regular operations", "(R8)    Businesses solve global problems through their regular operations ", cci_dashboard$question_sub)
  cci_dashboard$question_sub <- gsub(" - Businesses create global problems through their regular operations", "(R9)    Businesses create global problems through their regular operations ", cci_dashboard$question_sub)
  cci_dashboard$question_sub <- gsub(" - I am willing to pay extra for products and services that come from companies who have implemented programs that give back to society", "(R4)    I am willing to pay extra for products and services that come from companies who have implemented programs that give back to society ", cci_dashboard$question_sub)
  cci_dashboard$question_sub <- gsub(" - In the past 6 months, I purchased at least one product or service because I knew that the company had implemented a program to give back to society", "(R11)   In the past 6 months, I purchased at least one product or service because I knew that the company had implemented a program to give back to society", cci_dashboard$question_sub)

  #cci_dashboard_backup <- cci_dashboard
  #cci_dashboard <- cci_dashboard_backup

  cci_dashboard$category <- gsub("Corporate citizenship", "Corporate Citizenship", cci_dashboard$category)
  cci_dashboard$category <- gsub("Consumer demand", "Consumer Demand", cci_dashboard$category)
  
  cci_dashboard$category <- gsub("Corporate Citizenship", "CSR", cci_dashboard$category)
  cci_dashboard$question <- gsub("Q22. How much to you agree with the following statements[?]", "Q16. Level of agreement on corporate social responsibility", cci_dashboard$question)
  #summary(factor(cci_dashboard$category))
  
  cci_dashboard_q16 <- cci_dashboard[grepl("Q16",cci_dashboard$question),]
  cci_dashboard <- cci_dashboard[!grepl("Q16",cci_dashboard$question),]
  cci_dashboard_q16 <- cci_dashboard_q16[cci_dashboard_q16$response=="Top 2 Box [NET]",]
  cci_dashboard_q16$response <- cci_dashboard_q16$question_sub; cci_dashboard_q16$question_sub <- "None"
  cci_dashboard <- rbind(cci_dashboard,cci_dashboard_q16)
  
  cci_dashboard[cci_dashboard$region=="NA","region"] <- "NA."
  
  cci_dashboard$value.country <- gsub("%","",x = cci_dashboard$value.country)
  cci_dashboard$value.region <- gsub("%","",x = cci_dashboard$value.region)
  cci_dashboard$value.global <- gsub("%","",x = cci_dashboard$value.global)
  
  cci_dashboard$year <- factor(cci_dashboard$year)
  cci_dashboard$quarter <- factor(cci_dashboard$quarter)
  cci_dashboard$region <- factor(cci_dashboard$region)
  cci_dashboard$category <- factor(cci_dashboard$category)
  cci_dashboard$stat <- factor(cci_dashboard$stat)
  cci_dashboard$base <- factor(cci_dashboard$base)
  
  cci_dashboard$value.country <- as.numeric(cci_dashboard$value.country)
  cci_dashboard$value.region <- as.numeric(cci_dashboard$value.region)
  cci_dashboard$value.global <- as.numeric(cci_dashboard$value.global)
  
  cci_dashboard
  
}


rank_cci_question <- function(infile,question,rankyear,rankquarter) {

  #question <- "Q3"
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

rank_cci_dashboard <- function(infile,rankyear,rankquarter) {
  
  #infile <- cci_dashboard; rankyear <- "2015"; rankquarter <- "Q3"
  #rm(infile,rankquarter,rankyear,rankname)
  
  infile<- rank_cci_question(infile,"Q6",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q7a",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q7b",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q19a",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q19b",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q19c",rankyear,rankquarter)
  infile<- rank_cci_question(infile,"Q16","2011","Q3")
  
  rankname <- paste("rank",rankyear,rankquarter,sep="")
  
  infile[,rankname] <- NA
  infile[!is.na(infile$orderQ6),rankname] <- infile[!is.na(infile$orderQ6),"orderQ6"]
  infile[!is.na(infile$orderQ7a),rankname] <- infile[!is.na(infile$orderQ7a),"orderQ7a"]
  infile[!is.na(infile$orderQ7b),rankname] <- infile[!is.na(infile$orderQ7b),"orderQ7b"]
  infile[!is.na(infile$orderQ19a),rankname] <- infile[!is.na(infile$orderQ19a),"orderQ19a"]
  infile[!is.na(infile$orderQ19b),rankname] <- infile[!is.na(infile$orderQ19b),"orderQ19b"]
  infile[!is.na(infile$orderQ19c),rankname] <- infile[!is.na(infile$orderQ19c),"orderQ19c"]
  infile[!is.na(infile$orderQ16),rankname] <- infile[!is.na(infile$orderQ16),"orderQ16"]
  
  infile
  
}

setup_CCI_EIU <- function() {

  cci_eiu_Out2 <- cci_eiu_prepData(cciOutTOTAL,eiuFile)
  
  #write.csv(cci_eiu_Out2,paste(datain,"/Files OUTPUT/cci_eiu_Out2.csv",sep=""),row.names=F)
  
  cci_eiu_smart <- create_cci_eiu_smart(cci_eiu_Out2)

  #cci_eiu_smart <- rank_cci(cci_eiu_smart,"2015","Q2")
  cci_eiu_smart <- rank_cci(cci_eiu_smart,"2015","Q3")
  
  cci_eiu_smart <- cci_eiu_smart[c("year","quarter","region","country","category",
                                   "question","question_sub","stat","response","base",
                                   "value.country","value.region","value.global",
                                   "rank2015Q3")]
  
  write.csv(cci_eiu_smart,paste(datain,"/Files OUTPUT/cci_eiu_csuite.csv",sep=""),row.names=F)

  cci_eiu_smart
  
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

  nis_cci_ap <- cci_eiu_wb[cci_eiu_wb$region=="AP" & !(cci_eiu_wb$category %in% c("WB","EIU")),]
  
  nis_cci_ap[!(nis_cci_ap$question_sub=="None"),"question"] <- 
    gsub("_[123456789][0123456789]?","",nis_cci_ap[!(nis_cci_ap$question_sub=="None"),"question"])
  
  write.csv(nis_cci_ap,paste(datain,"/Files OUTPUT/NIS AP - CCI.csv",sep=""),row.names=F)
  
  nis_cci_ap_2011 <- nis_cci_ap[nis_cci_ap$year=="2011",]
  nis_cci_ap_2012 <- nis_cci_ap[nis_cci_ap$year=="2012",]
  nis_cci_ap_2013 <- nis_cci_ap[nis_cci_ap$year=="2013",]
  nis_cci_ap_2014 <- nis_cci_ap[nis_cci_ap$year=="2014",]
  nis_cci_ap_2015 <- nis_cci_ap[nis_cci_ap$year=="2015",]

  write.csv(nis_cci_ap_2011,paste(datain,"/Files OUTPUT/NIS AP - CCI 2011.csv",sep=""),row.names=F)
  write.csv(nis_cci_ap_2012,paste(datain,"/Files OUTPUT/NIS AP - CCI 2012.csv",sep=""),row.names=F)
  write.csv(nis_cci_ap_2013,paste(datain,"/Files OUTPUT/NIS AP - CCI 2013.csv",sep=""),row.names=F)
  write.csv(nis_cci_ap_2014,paste(datain,"/Files OUTPUT/NIS AP - CCI 2014.csv",sep=""),row.names=F)
  write.csv(nis_cci_ap_2015,paste(datain,"/Files OUTPUT/NIS AP - CCI 2015.csv",sep=""),row.names=F)
  
  
  
  cci_eiu_wb
  
}

setup_CCI_DASHBOARD <- function() {
  
  cci_dashboard <- create_cci_dashboard(cci_eiu_Out2)
  #summary(factor(substring(cci_dashboard$question,1,4)))

  #write.csv(cci_dashboard,paste(datain,"/Files OUTPUT/cci_dashboard_test.csv",sep=""),row.names=F)
  
  #cci_dashboard_backup <- cci_dashboard
  #cci_dashboard <- cci_dashboard_backup
  
  cci_dashboard <- rank_cci_dashboard(cci_dashboard,"2015","Q3")
  
  cci_dashboard <- cci_dashboard[cci_dashboard$region=="AP",]
  
  cci_dashboard <- cci_dashboard[c("year","quarter","country","category",
                                   "question","question_sub","response",
                                   "value.country","value.region","rank2015Q3")]
  
  cci_dashboard <- cci_dashboard[,c("year","quarter","country","category",
                                    "question","question_sub","response",
                                    "value.country","value.region","rank2015Q3")]
  
  write.csv(cci_dashboard,paste(datain,"/Files OUTPUT/cci_dashboard.csv",sep=""),row.names=F)
  
  cci_dashboard
  
}
