#!/usr/bin/env Rscript

#rm(list=ls())
#save(cci_eiu_csuite, file = "cci_eiu_csuite.RData")
#save(cci_eiu_Out2, file = "cci_eiu_Out2.RData")
#save(cci_eiu_smart, file = "cci_eiu_smart.RData")
#save(CCIgroupByDateCountry, file = "CCIgroupByDateCountry.RData")
#save(cciOut, file = "cciOut.RData")
#save(cciOutAP, file = "cciOutAP.RData")
#save(cciOutGLOBAL, file = "cciOutGLOBAL.RData")
#save(cciOutREGION, file = "cciOutREGION.RData")
#save(cciOutSEA, file = "cciOutSEA.RData")
#save(cciOutTOTAL, file = "cciOutTOTAL.RData")
#save(eiuFile, file = "eiuFile.RData")
#save(eiuFileQ, file = "eiuFileQ.RData")
#save(eiuFileY, file = "eiuFileY.RData")
#save(long_CCI, file = "long_CCI.RData")

#rm(cci_eiu_csuite,cci_eiu_Out2,cci_eiu_smart,
#   CCIgroupByDateCountry,
#   cciOut,cciOutAP,cciOutGLOBAL,cciOutREGION,cciOutSEA,cciOutTOTAL,
#   eiuFile,eiuFileQ,eiuFileY,
#   long_CCI)

args = commandArgs(trailingOnly=TRUE)

if (length(args)<=1) {
  stop("\nTwo arguments must be supplied including year (e.g. 2016) + quarter (e.g 2)\nThird argument is e.g. 2016_Q2\n", call.=FALSE)
} else if (length(args)==2) {
  args[3] = paste0(args[1],"_Q",args[2])
}

# args[1] <- 2017; args[2] <- 1; args[3] <- paste0(args[1],"_Q",args[2])

userId <- "langestrst01"
# passWd <- "Fill in your enterprise password"
passWd <- "Devt0517"
fileCCIRoot <- "https://intranet.nielsen.com/company/news/newsletters/Consumer%20Confidence%20Concerns%20and%20Spending%20Library/"

Rdir <- "~/projects/thought_leadership/R_Code"

source(file.path(Rdir,"libraries.R"))
source(file.path(Rdir,"Functions_CCI.R"))
source(file.path(Rdir,"Functions_EIU.R"))
source(file.path(Rdir,"Functions_CCI_EIU.R"))

datain <- "~/projects/thought_leadership"
shinyApps <- "~/ShinyApps/"

cciOutTOTAL <- setupCCI(download = F, year = args[1], quarter = args[2]) # CHECK FILENAMES ON ISHARE

eiuFile <- setupEIU(args[3]) # NOTE filename = "eiu_2016_Q2_Q" or "eiu_2016_Q2_Y"

cci_eiu_csuite <- setup_CCI_EIU(cciOutTOTAL,eiuFile, year=args[1], quarter=paste0("Q",args[2]))
head(cciOutTOTAL[cciOutTOTAL$year==2017,])
