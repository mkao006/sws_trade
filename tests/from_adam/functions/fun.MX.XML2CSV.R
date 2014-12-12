#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.XML2CSV()


    fun.MX.XML2CSV = function (xmlUNSD,
					          MXpath) {
    
	 rawDF_raw <- xmlToDataFrame(xmlUNSD, stringsAsFactors = FALSE,
                                colClass = c("character", rep("numeric", 12)))	
     write.csv(rawDF_raw, file = 
          paste0(MXpath, 
		  paste0(xmlUNSD, ".csv")), 
          row.names = FALSE)
		  
	print(paste0("Converting: ", xmlUNSD))
	
    }