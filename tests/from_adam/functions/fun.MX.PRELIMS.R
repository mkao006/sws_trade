#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.PRELIMS()


 fun.MX.PRELIMS = function (rawDF, intYR) {

  #non reporting countries
   nonRep <- rawDF[rawDF$ptCode %in% rawDF$rtCode == FALSE, ]  
   nonRep <- nonRep[!nonRep$ptCode == 0, ]
   nonRepImp <- nonRep[nonRep$rgCode == 2, ] 
   nonRepImpList <<-unique(nonRepImp$ptCode)
   nonRepExp <- nonRep[nonRep$rgCode == 1, ] 
   nonRepExpList <<- unique(nonRepExp$ptCode)

# record re-imports and re-exports
   reMXcodes <- c("3", "4")
   reMX <- rawDF[rawDF$rgCode %in% reMXcodes, ]
   reMX <- reMX[!reMX$ptCode == 0, ]

# add re-exports to exports
   rawDF$rgCode[rawDF$rgCode == 3] <- 1
# add re-imports to imports
   rawDF$rgCode[rawDF$rgCode == 4] <- 2
 
# remove self trade!
   rawDF <- subset(rawDF, rtCode != ptCode)
   
# missing NW in qtCode==8
    cond_NW8 <- rawDF$qtCode == 8 & rawDF$NetWeight == 0 | is.na(rawDF$NetWeight) == TRUE
   rawDF[cond_NW8, "qtCode"] <-1
    cond_NW5 <- rawDF$qtCode == 5 & rawDF$NetWeight == 0 | is.na(rawDF$NetWeight) == TRUE
   rawDF[cond_NW5, "qtCode"] <-1
    cond_NW7 <- rawDF$qtCode == 7 & rawDF$NetWeight == 0 | is.na(rawDF$NetWeight) == TRUE
   rawDF[cond_NW7, "qtCode"] <-1
   
# redundant country codes
   redCodes <- c(0, 97, 158,  412, 527, 568, 577, 637, 837, 838,  899)

#get unspecified data before eliminating redundancy 
   unSpecCode <- c(839, 899)
   unSpecDF <- rawDF[rawDF$ptCode %in% unSpecCode, ] 

   rawDF<-rawDF[rawDF$rtCode %in% redCodes == FALSE, ] 
   rawDF<-rawDF[rawDF$ptCode %in% redCodes == FALSE, ] 
   rawDF<-rawDF[rawDF$rtCode %in% unSpecCode == FALSE, ] 
   rawDF<-rawDF[rawDF$ptCode %in% unSpecCode == FALSE, ] 

# dissemination versus processing codes
   rawDF$rtCode[rawDF$rtCode == 840] <- 842
   rawDF$ptCode[rawDF$ptCode == 840] <- 842
   rawDF$rtCode[rawDF$rtCode == 250] <- 251
   rawDF$ptCode[rawDF$ptCode == 250] <- 251
   rawDF$rtCode[rawDF$rtCode == 380] <- 381
   rawDF$ptCode[rawDF$ptCode == 380] <- 381
   rawDF$rtCode[rawDF$rtCode == 578] <- 579
   rawDF$ptCode[rawDF$ptCode == 578] <- 579
   rawDF$rtCode[rawDF$rtCode == 756] <- 757
   rawDF$ptCode[rawDF$ptCode == 756] <- 757

## EU intra trade
  if(intYR < 1995){euCodes = c(528, 56, 251, 276, 381, 442, 150, 208, 372, 826, 300, 620, 724)} else
    if(intYR >= 1995 & intYR < 2005){euCodes = c(528, 56,251, 276, 381, 442,150,208,372,826,300,620,724,40,246,752)} else
       if(intYR >= 2005 & intYR < 2007){euCodes = c(528, 56, 251, 276, 381, 442, 150, 208, 372, 826, 300, 620, 724, 40, 246, 752, 196, 203, 63, 348, 428, 440, 470, 616, 703, 198)} else
		 if(intYR >= 2007){euCodes = c(528, 56, 251, 276, 381, 442, 150, 208, 372, 826, 300, 620, 724, 40, 246, 752, 196, 203, 63, 348, 428, 440, 470, 616, 703, 198, 100, 642)}

  
  return(list(nonRepImpList, nonRepExpList, reMX, rawDF, euCodes, unSpecDF))

 }