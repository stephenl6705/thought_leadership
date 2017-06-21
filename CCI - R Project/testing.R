
Rdir <- "~/SEANAP PROJECTS/Thought Leadership/CCI - R Project"

source(file.path(Rdir,"libraries.R"))

datain <- "~/SEANAP PROJECTS/Thought Leadership"

setwd(paste0(datain,"/files_cci_drive"))
fileURL <- 'https://drive.google.com/drive/folders/0B5qpmN53np2ucEFNR0FhQzc3N3c'
fileOut <- "Q1 2010 ONL7760_Consumer_Confidence_Country_Wgt_(P)_2010-04-09.xls"
bindat <- getBinaryURL(fileURL)
writeBin(bindat,fileOut)
getwd()

https://drive.google.com/open?id=0B_ms17BBz6JtclpGTzNUMXg2U1E
https://drive.google.com/open?id=0B_ms17BBz6JtbS1xQWNMLVdVc3M
https://drive.google.com/open?id=0B_ms17BBz6JtZnRaalpKNDk2TlE

# you'll need RGoogleDocs (with RCurl dependency..)

install.packages('devtools')
library(devtools)
install_github("RGoogleDocs", "duncantl")
library(RGoogleDocs)

gpasswd = "Devt0314"
auth = getGoogleAuth("stephen.langestraat@nielsen.com", gpasswd)
con = getGoogleDocsConnection(auth)

CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
docs <- getDocs(con, cainfo = CAINFO)

# get file references
hrefs <- lapply(docs, function(x) return(x@access["href"]))
keys <- sub(".*/full/.*%3A(.*)", "\\1", hrefs)
types <- sub(".*/full/(.*)%3A.*", "\\1", hrefs)

# make urls (for url-scheme see: http://techathlon.com/download-shared-files-google-drive/)
# put format parameter for other output formats!
pdf_urls <- paste0("https://docs.google.com/uc?export=download&id=", keys)
doc_urls <- paste0("https://docs.google.com/document/d/", keys, "/export?format=", "txt")

# download documents with your browser
gdoc_ids <- grep("document", types)
lapply(gdoc_ids, function(x) shell.exec(doc_urls[x]))

pdf_ids <- grep("pdf", types, ignore.case = T)
lapply(pdf_ids, function(x) shell.exec(pdf_urls[x]))