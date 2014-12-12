#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.OUTXML()


    fun.MX.OUTXML = function (x,
					          MXpath) {
    
       MQ <- aggregate(cbind(NW_M) ~ rtCode, data = x, FUN = sum)
          colnames(MQ)[colnames(MQ) == "NW_M"] = paste0("MQ", commYR)
       MV <- aggregate(cbind(TV_M) ~ rtCode, data = x, FUN = sum)
          colnames(MV)[colnames(MV) == "TV_M"] = paste0("MV", commYR)
       M <- merge(MQ, MV, by = "rtCode")

       XQ <- aggregate(cbind(NW_X) ~ ptCode, data = x, FUN = sum)
         colnames(XQ)[colnames(XQ) == "NW_X"] = paste0("XQ", commYR)
       XV <- aggregate(cbind(TV_X) ~ ptCode, data = x, FUN = sum)
         colnames(XV)[colnames(XV) == "TV_X"] = paste0("XV", commYR)
       X <- merge(XQ, XV, by = "ptCode")

  write.csv(M, file = paste(paste(commYR, "relM"), ".csv", sep = ""), row.names = FALSE) 
  write.csv(X, file = paste(paste(commYR, "relX"), ".csv", sep = ""), row.names = FALSE) 
  
     for(df in list(M, X)){
      if(substr(colnames(df[2]), 0, 2) == "XQ"){ix <- c("X")
      } else {
      ix <- c("M")}
      data <-df
      xml <- xmlTree()
      xml$addTag("document", close = FALSE)
     for (i in 1:nrow(data)) {
      xml$addTag("row", close = FALSE)
       for (j in names(data)) {
          xml$addTag(j, data[i, j])
      }
      xml$closeTag()
     }
     xml$closeTag()
     saveXML(xml, paste(paste(commYR, ix), ".xml", sep = ""))
    }
  }
