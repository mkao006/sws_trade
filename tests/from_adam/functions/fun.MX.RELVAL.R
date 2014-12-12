#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.RELVAL()


 fun.MX.RELVAL = function (x, 
                          euCodes,
                          errTol, 
						  file = ""){


 #select "nochange values" or nonsense
  z <- x[which(!x$NW_M - x$NW_X == 0), ]
  if(nrow(z) > 0){
   z[(z$NW_M / z$NW_X) > 0.95 & (z$NW_M / z$NW_X) < 1.05 | (z$NW_X / z$NW_M) > 0.95 & 
     (z$NW_X / z$NW_M) < 1.05, "metaData"] <- "within tolerance"
   x[rownames(z), ] <- z[rownames(z), ]
  }

  x_noch  <-subset(x,   metaData == "noChange" | metaData == "nonse NW_X" | metaData == "nonse NW_M")
  if(nrow(x_noch) == 0) {
   warning("No values tagged 'noChange' in this data,  so 'add reliability' operation useless")
  } else {

 #build error metric: 
  x_noch$errorNW <- 100 * abs((x_noch$NW_M - x_noch$NW_X) / x_noch$NW_X)

 #build simple index: perc(err <param errTol) by country
  M_index <- ddply(x_noch, .(rtCode), function(x) with(x, sum(NW_M[errorNW < errTol], na.rm = TRUE) / sum(NW_M, na.rm = TRUE)))
  X_index <- ddply(x_noch, .(ptCode), function(x) with(x, sum(NW_X[errorNW < errTol], na.rm = TRUE) / sum(NW_X, na.rm = TRUE)))

 #assemble indices:
  indices <- merge(M_index, X_index, by.x = "rtCode", by.y = "ptCode", all = TRUE)

 #build "worst partner" index index:
  x_noch_relab1 <- ddply(x_noch, .(rtCode), function(x) data.frame(x, rt_relab = merge(x, M_index, all.x = TRUE)$V1))
  x_noch_relab <- ddply(x_noch_relab1, .(rtCode), function(x) data.frame(x, pt_relab = merge(x, X_index, all.x = TRUE)$V1))

  M_index_trim <- ddply(x_noch_relab, .(rtCode), function(x) 
	with(x[ifelse(nrow(x) >1, -which.max(x$errorNW * (1 - x$pt_relab)), 1), ], 
	       sum(NW_M[errorNW<errTol], na.rm = TRUE) / sum(NW_M, na.rm = TRUE)))

  X_index_trim <- ddply(x_noch_relab, .(ptCode), function(x) 
	with(x[ifelse(nrow(x) >1, -which.max(x$errorNW * (1 - x$rt_relab)), 1), ], 
	       sum(NW_X[errorNW<errTol], na.rm = TRUE) / sum(NW_X, na.rm = TRUE)))

	#x_noch_relab<-na.omit(x_noch_relab)
	#x_noch_relaball1 <-na.omit(x_noch_relaball1)

  x_noch_relaball1 <- ddply(x_noch_relab, .(rtCode), function(x) data.frame(x, rt_relab_trim = merge(x, M_index_trim, all.x = TRUE)$V1))
  x_noch_relaball <- ddply(x_noch_relaball1, .(rtCode), function(x) data.frame(x, pt_relab_trim = merge(x, X_index_trim, all.x = TRUE)$V1))

 #select correct flow from relability:
  comparRel <- x_noch_relaball[, "rt_relab_trim"] > x_noch_relaball[, "pt_relab_trim"]
  sameRel <- 100 * mean(x_noch_relaball[, "rt_relab_trim"]  ==  x_noch_relaball[, "pt_relab_trim"])
	
  cat("% with same relability index: ", sameRel, "%\n", sep = "", file = file, append = TRUE)

  for(i in 1:nrow(x_noch_relaball)){
   keep <-  ifelse(comparRel[i], "NW_M", "NW_X")
   change <- ifelse(comparRel[i], "NW_X", "NW_M")
   x_noch_relaball[i, keep] <- x_noch_relaball[i, change]
   x_noch_relaball[i, "metaData"] <- paste("impu_reliab", ifelse(comparRel[i], "M", "X"), sep = "_")
  }
	 
 #recondition values and unit values 
  x_noch_relaball[x_noch_relaball$metaData  ==  "impu_reliab_X", "TV_X" ] <- 
         x_noch_relaball[x_noch_relaball$metaData  ==  "impu_reliab_X", "TV_M"] * .9
  x_noch_relaball[x_noch_relaball$metaData  ==  "impu_reliab_M", "TV_M" ] <- 
         x_noch_relaball[x_noch_relaball$metaData  ==  "impu_reliab_M", "TV_X"] * 1.1
  x_noch_relaball$UV_M <- x_noch_relaball$TV_M / x_noch_relaball$NW_M * 1000
  x_noch_relaball$UV_X <- x_noch_relaball$TV_X / x_noch_relaball$NW_X * 1000
	 
 #add to big data frame:
  if(!all(x_noch_relaball[, 1:2]  ==  x_noch[, 1:2])) cat("Problem with rel function!!\n")
  
  rownames(x_noch_relaball) <- rownames(x_noch)
  x$errorNW <- x$rt_relab <- x$pt_relab <- x$rt_relab_trim <- x$pt_relab_trim <- NA
  x[rownames(x_noch_relaball), ] <- x_noch_relaball[rownames(x_noch_relaball), ]
  
 } 
 
 xeu <- x[x$rtCode  %in% euCodes & x$ptCode %in% euCodes, ]
 return(list(x, xeu))

}