
userId <- "langestrst01"
passWd <- "Devt0314"

Rdir <- "~/projects/thought_leadership/R_Code"

source(file.path(Rdir,"libraries.R"))
source(file.path(Rdir,"Functions_CCI.R"))

datain <- "~/projects/thought_leadership"
shinyApps <- "~/ShinyApps/"

#setupCCI()

#setupEIU("2016_08_11")

#setup_CCI_EIU

#setup_CCI_EIU_WB

cciOutTOTAL <- rbind.fill(cciOut,cciOutREGION)
cciOutTOTAL <- addCol(cciOutTOTAL,"stat","Response")
cciOutTOTAL <- cciOutTOTAL %>% select(region, year, quarter, category, question, question_sub, stat, base, Response, Total, AU:SEA)
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
names(cciOutTOTAL)[names(cciOutTOTAL)=="NA."] <- "NAm"

long_CCI <- cciOutTOTAL %>% gather(Country, Value, AU:SEA)

long_CCI$valtype <- "abs"
long_CCI[grepl('%',long_CCI$Value),"valtype"] <- "perc"

#spread_CCI <- long_CCI %>% spread(valtype,Value)

long_CCI$Value <- gsub("%","",long_CCI$Value)
long_CCI$Total <- gsub("%","",long_CCI$Total)
long_CCI$Value <- as.numeric(long_CCI$Value)
long_CCI$Total <- as.numeric(long_CCI$Total)

head(long_CCI)

str(long_CCI)

summary(factor(long_CCI$valtype))

write.csv(long_CCI,paste(shinyApps,"cci_average_index/long_CCI.csv",sep=""))

subset <- long_CCI[long_CCI$region=='GLOBAL'
                   & long_CCI$question=="Index. CONSUMER CONFIDENCE INDEX"
                   & long_CCI$stat=="Response"
                   ,c("year","quarter","Response","Country","Value","valtype")]

subset$yearqtr = paste(subset$year,subset$quarter,sep="")
head(subset)

subset_au = subset[subset$Country=="AU",]
head(subset_au)
barplot(table(subset_au$yearqtr))

plot(x = 1 : 20, y = subset_au$Value, type = "l", xlab = 'Year-Quarter', main = 'Base graphics')

ggplot(subset_au, aes(x=1:20, y=Value)) + geom_line() + xlab("Year-Quarter") + ggtitle("ggplot2")

CCIgroupByDateCountry =
  filter(long_CCI, region=="GLOBAL") %>%
  filter(question=="Index. CONSUMER CONFIDENCE INDEX") %>%
  filter(stat=="Response") %>%
  group_by(Country, year, quarter) %>%
  summarise(meanValue = mean(Value, na.rm = TRUE))

CCIgroupByDateCountry$yearqtr <-
  format(
    as.yearqtr(paste(CCIgroupByDateCountry$year,CCIgroupByDateCountry$quarter)
               ,format = "%Y Q%q")
    ,format = "%y/0%q")


CCIgroupByDateCountry$Country_long <- CCIgroupByDateCountry$Country
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "AU","Country_long"] <- "Australia"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "CN","Country_long"] <- "China"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "HK","Country_long"] <- "Hong Kong"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "IN","Country_long"] <- "India"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "ID","Country_long"] <- "Indonesia"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "KO","Country_long"] <- "Korea"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "MY","Country_long"] <- "Malaysia"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "NZ","Country_long"] <- "New Zealand"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "PH","Country_long"] <- "Phillipines"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "SG","Country_long"] <- "Singapore"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "TW","Country_long"] <- "Taiwan"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "TH","Country_long"] <- "Thailand"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "VN","Country_long"] <- "Vietnam"
CCIgroupByDateCountry[CCIgroupByDateCountry$Country == "JP","Country_long"] <- "Japan"

head(CCIgroupByDateCountry)
write.csv(CCIgroupByDateCountry,paste0(datain,"/CCIgroupByDateCountry.csv"))

########### MAP #################
library(googleVis)
library(rgdal)
world <- readOGR(dsn = ".", layer =
                   "world_country_admin_boundary_shapefile_with_fips_codes")
countries <- world@data
countries <- cbind(id = rownames(countries), countries)
countries <- merge(countries, groupByCountry, by.x = "CNTRY_NAME",
                   by.y = "country", all.x = TRUE)
map.df <- fortify(world)
map.df <- merge(map.df, countries, by = "id")

############ SQLITE ##########################

library(sqldf)

db <- dbConnect(SQLite(), dbname="CCI.sqlite")
rm(db)

dbWriteTable(conn = db, name = "long_CCI", value = long_CCI, row.names = FALSE)
dbReadTable(db, "long_CCI")

dbListTables(db)
dbListFields(db, "long_CCI") 
head(long_CCI)
