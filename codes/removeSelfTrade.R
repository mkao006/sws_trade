##' This function removes self-trade, which is deemed to be erroneous
##'
##' @param data The data frame
##' @param reportingCountry The column which represents the reporting country.
##' @param partnerCounytry The column which represents the partner
##' country of the trade.
##'
##' @export
##' 

removeSelfTrade = function(data, reportingCountry, partnerCountry){
    noSelfTrade = data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
    noSelfTrade
}
