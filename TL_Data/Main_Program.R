
Rdir <- "~/PROJECTS/Thought Leadership/TL_Data"

source(file.path(Rdir,"libraries.R"))

datain <- "~/PROJECTS/Thought Leadership"

setwd(datain)

rd_CCI <- function(filename,row) {
  
  filename <- "Q3 2015 CCI AP REGION.xls"
  # rm(filename,infile,dataIn,rowstart,rowend);
  
  dataIn <- loadWorkbook(filename)
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=1, endRow=10000, startCol=1, endCol=16, header=F)
  infile <- infile[!is.na(infile$Col2),]
  rowstart <- as.numeric(rownames(infile[1,]))
  rowend <- as.numeric(rows[length(rownames(infile))])
  infile = readWorksheet(dataIn, sheet = getSheets(dataIn)[1],useCachedValues=FALSE,
                         startRow=rowstart, endRow=rowend, startCol=1, endCol=16, header=TRUE)
  names(infile)[1] <- "Response"
  infile <- infile[!is.na(infile$Response),]
  infile$question <- "CCI"
  infile <- infile[c(ncol(infile),1:ncol(infile)-1)]
  infile$base <- infile[1,"Response"]
  infile <- infile[c(1,2,ncol(infile),4:ncol(infile)-1)]
  question <- infile[1,"question"]
  base <- infile[1,"base"]
  for (r in 1:nrow(infile)) {
    if (grepl("Base:",infile[r,"Response"]) & !grepl("wtd",infile[r,"Response"])) {
      base <- infile[r,"Response"]
    } else if (grepl("^Q[1,2,3,4,5,6]",infile[r,"Response"])) {
      question <- infile[r,"Response"]
    }
    infile[r,"base"] <- base
    infile[r,"question"] <- question
  }
  infile <- infile[!is.na(infile$Total),]
  head(infile,n=5)
  write.csv(infile,paste(datain,"/cci.csv",sep=""),row.names=F)
  
  infile
}
