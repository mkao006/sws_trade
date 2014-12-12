## PACKAGE BUILDER ----------------------------------------------------
library("devtools")
library(roxygen2)
setwd("C:/Users/prakash/Dropbox/workspace/r.packages/")
if(file.exists("./mx.compile"))
    unlink("mx.compile", recursive = TRUE)
create("mx.compile")
file.copy(list.files("mx.codes/", "\\.R$", full.names = TRUE), "./mx.compile/R/")
setwd("./mx.compile")
document()
setwd("..")
# install("mx.compile")
# install_github('mx.compile','AdamNewb')

library(XML)
library(plyr)
library(reshape2)

UNSDlist <- read.csv("UNSDxml.csv", header=FALSE,sep = ",")
MXpath <- ("C:/Users/prakash/Dropbox/CPC-FBS/Compile/inData/UNSD/")

###########################################################################################
#################################### set booleans #########################################

##horribly long process to convert xml files to csv, though needed to reduce processing time
##given the slowness of xml-to-data-frame
createCSV <- FALSE
outXML <- FALSE
#########################################################################################

for(i in 1:length(UNSDlist[[1]])){
  xmlUNSD <- as.character(UNSDlist[i, 1]) 
  
  if(createCSV == TRUE){
        fun.MX.XML2CSV (substr(xmlUNSD, 1, 15), MXpath)
   }

  x <- paste0(MXpath, xmlUNSD)
  if(file.exists(x) & file.info(x)$size > 4096){
   if(nchar(xmlUNSD) == 19){
    commYR <<- substr(xmlUNSD,1,11)
    intYR <- as.numeric(substr(xmlUNSD,8,11))
    intHS <- as.numeric(substr(xmlUNSD,1,6))
    
    LogFile = paste(paste(intHS, intYR, sep= "_"), ".txt", sep = "")
    if(file.exists(LogFile))
    unlink(LogFile)
	
    rawDF <- fun.MX.READCSV (x)
  
    MX.Prelim.DAT <- fun.MX.PRELIMS (rawDF, intYR) 
     nonRepImpList <- MX.Prelim.DAT[[1]]
     nonRepExpList <- MX.Prelim.DAT[[2]]
     reMX <- MX.Prelim.DAT[[3]]
     rawDF <- MX.Prelim.DAT[[4]]
     euCodes <- MX.Prelim.DAT[[5]]
     unSpecDF <- MX.Prelim.DAT[[6]]
     
	 write.csv(reMX, file = paste0(paste(commYR, "reMX"), ".csv"), row.names = FALSE)

     errFile = paste0(paste(intHS, intYR, sep="_"), "err.txt")
     if(file.exists(errFile))
     unlink(errFile)

  if(max(rawDF$rgCode)>1){
   
    MX.RAW.DAT <- fun.MX.RAWVAL(rawDF, intHS, euCodes)
    write.csv(MX.RAW.DAT[[1]], file = paste0(paste(commYR, "rawVal"), ".csv"), row.names = FALSE)
    
	
	MX.BASIC.DAT <- fun.MX.BASICVAL(MX.RAW.DAT[[1]], euCodes, file = LogFile)
    write.csv(MX.BASIC.DAT[[1]], file = paste0(paste(commYR, "basicVal"), ".csv"), row.names = FALSE) 

  	MX.FULL.DAT <- fun.MX.FULLVAL(MX.BASIC.DAT[[1]],
                                  Min.UV_M = MX.BASIC.DAT[[2]],
								  Min.UV_X = MX.BASIC.DAT[[3]],
								  Max.UV_M = MX.BASIC.DAT[[4]],
								  Max.UV_X = MX.BASIC.DAT[[5]],
	                              median.UV_M = MX.BASIC.DAT[[6]],
								  median.UV_X = MX.BASIC.DAT[[7]],
								  euCodes,
	                              file = LogFile)
    write.csv(MX.FULL.DAT[[1]], file = paste0(paste(commYR, "fullVal"), ".csv"), row.names = FALSE) 

    MX.RELVAL.DAT <- fun.MX.RELVAL (MX.FULL.DAT[[1]],
                                    euCodes,
                                    errTol = 5, 
						            file = LogFile)
	write.csv(MX.RELVAL.DAT[[1]], file = paste0(paste(commYR, "relVal"), ".csv"), row.names = FALSE) 
	
	
	#alerts for significantly missing 
    unSpecDF <- subset(unSpecDF, NetWeight / sum(MX.RELVAL.DAT$NW_X) >= 0.01)
    if(nrow(unSpecDF) > 0){
       write.csv(unSpecDF, paste(paste(intHS, intYR, sep = "_"), "-unSPEC.csv", sep = ""), row.names = FALSE)
       cat("ATTENTION, SIGNIFICANT UNSPECIFIED TRADE of: ", unSpecDF$NetWeight, "\n", file = LogFile, append = TRUE)
    }

     missNW <- subset(rawDF, rawDF$qtCode == 1)
     missNW <- subset(missNW, missNW[missNW$qtCode == 1, "TradeValue"] / sum(rawDF$TradeValue) >= 0.001)

     if(nrow(missNW) > 0){
        write.csv(missNW, paste(paste(intHS, intYR, sep = "_"), "-MISS.csv", sep = ""), row.names = FALSE)
     cat("ATTENTION, SIGNIFICANT MISSING TRADE associated with value of: ", missNW$TradeValue, "\n", file = LogFile, append = TRUE)
     }
     
	 if(outXML == TRUE){
	 fun.MX.OUTXML (MX.RELVAL.DAT, MXpath)
     }
						  
  } else {
  cat("No EXPORTS in ", xmlUNSD, "\n", file = errFile, append = TRUE)
  }
  } else {
  cat("missing files: ", xmlUNSD, "\n")
  }
  }
}
