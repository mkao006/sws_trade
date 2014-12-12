#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.RAWVAL()


 fun.MX.RAWVAL = function (data, HScode, euCodes) {

   if(!missing(HScode)) data <- subset(data, cmdCode %in% grep(HScode, unique(data$cmdCode), value = TRUE))
   partDAT <- subset(data, rgCode %in% 1:2 & qtCode %in% 1:8)
  
# melt data
   meltDAT <- melt(partDAT, 
                   id.vars = c("rtCode", "ptCode", "rgCode", "qtCode", "yr", "metaData"),
				   measure.vars = c("NetWeight", "TradeValue"))
   meltDAT2 <- meltDAT

# substitute rt and pt code for exporters: (pt now denotes exporters)
   meltDAT2[meltDAT2$rgCode == 2, "ptCode"] <- meltDAT[meltDAT$rgCode == 2, "rtCode"]
   meltDAT2[meltDAT2$rgCode == 2, "rtCode"] <- meltDAT[meltDAT$rgCode == 2, "ptCode"]

# cast data, one col for M, one for X (need to sum as multiple cmdCodes per dyad)
   castDAT = dcast(meltDAT2, yr + rtCode + ptCode + qtCode + metaData ~ rgCode + variable,  
                   fun.aggregate = sum)

# check for special case when sparse data indicating only one direction
   if(length(unique(meltDAT2$rgCode)) == 1) {
     if(unique(meltDAT2$rgCode) == 1) {
      castDAT <- cbind(castDAT, NA, NA)
     } else if(unique(meltDAT2$rgCode) == 1){
      castDAT <- cbind(castDAT[, 1:3], NA, NA, castDAT[, 4:5])
     }
   }

  
# deal with paired qtCode=1 and qtCode=8 by aggregating over ptCodes and rtCodes
   castDATd <- castDAT[, c("yr", "rtCode", "ptCode")]
   dup <- which(duplicated(castDATd) | duplicated(castDATd[nrow(castDATd):1, ])[nrow(castDATd):1])

   if (length(dup) > 0) {
   castDATx <- castDAT[-dup, ]
   castDATy <- castDAT[dup, ]
   castDATy <- aggregate(x = castDATy[, c("1_NetWeight", "1_TradeValue", "2_NetWeight", "2_TradeValue")],
               by = list(castDATy$yr, castDATy$rtCode, castDATy$ptCode), FUN = sum)

   colnames(castDATy)[colnames(castDATy) == "Group.1"] <- "yr"
   colnames(castDATy)[colnames(castDATy) == "Group.2"] <- "rtCode"
   colnames(castDATy)[colnames(castDATy) == "Group.3"] <- "ptCode"

   castDATy["qtCode"] <- 8
   castDATy["metaData"] <- "Dup 1 <> 8"
   castDAT <- rbind(castDATx, castDATy)
   }
  
# formatoutput
   colnames(castDAT) <- c("Year", "rtCode", "ptCode", "qtCode", "metaData", "NW_M", "TV_M", "NW_X","TV_X")
   castDAT <- castDAT[, c("Year", "rtCode", "ptCode", "qtCode", "metaData", "NW_M", "TV_M", "NW_X","TV_X")]

# dalculate UVs
  castDAT$UV_M <- castDAT$TV_M / castDAT$NW_M * 1000;
  castDAT$UV_X <- castDAT$TV_X / castDAT$NW_X * 1000;

 
# calculate EU-intra
  castDATeu <- castDAT[castDAT$rtCode  %in% euCodes & castDAT$ptCode %in% euCodes, ]
  #castDATeu <- castDAT[castDAT$rtCode & castDAT$ptCode %in% euCodes, ]
  
  return(list(castDAT, castDATeu))

 }