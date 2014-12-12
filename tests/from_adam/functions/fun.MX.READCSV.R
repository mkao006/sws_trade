#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.READCSV()


  fun.MX.READCSV = function (x) {

    print(paste0("Reading: ", x))
	
	rawDF_raw <- read.csv(x, header = TRUE, sep = ",")
			  
    rawDF = ddply(.data = rawDF_raw, 
	              .variable = .(pfCode, yr, rgCode, rtCode, ptCode, 
				                cmdCode, qtCode, estCode, htCode),
		          .fun = function(x) colSums(x[, c("NetWeight", "TradeValue")])
                  )
				  
    rawDF$metaData <- rep("", nrow(rawDF));
	
  return(rawDF)
   }