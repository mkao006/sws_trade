##' This function mirrors trade by replacing missing trade if
##' corresponding trade is available and reported by the trading
##' partner.
##'
##' @param data The data frame
##' @param reportingCountry The column represents the reporting country.
##' @param partnerCountry The column representing the partner country
##' of the trade.
##' @param reverseTradePrefix The prefix which indicates the flow is
##' reported by the partner country.
##' @param valueColumns The column which represents the traded value
##' and quantities.
##' @param flagColumns The column which represents the flag of the
##' trade.
##'
##' @export
##' 

mirrorTrade = function(data, reportingCountry, partnerCountry, reverseTradePrefix,
                       valueColumns, flagColumns){
    base = copy(data)
    tmp = data[, !flagColumns, with = FALSE]

    reverseReportingName = paste0(reverseTradePrefix, reportingCountry)
    reversePartnerName = paste0(reverseTradePrefix, partnerCountry)
    setnames(tmp, old = c(reportingCountry, partnerCountry),
             new = c(reverseReportingName, reversePartnerName))
    
    base[, `:=`(c(reverseReportingName, reversePartnerName),
                list(.SD[[partnerCountry]], .SD[[reportingCountry]]))]

    setnames(tmp, old = valueColumns,
             new = paste0(reverseTradePrefix, valueColumns))

    mirroredTrade = merge(base, tmp,
        by = intersect(colnames(base), colnames(tmp)), all = TRUE)
    ## Fill in the missing trade country name
    ## mirroredTrade[is.na(.SD[[reportingCountry]]),
    ##             `:=`(c(reportingCountry), get(reversePartnerName))]
    ## mirroredTrade[is.na(.SD[[partnerCountry]]),
    ##             `:=`(c(parnterCountry), get(reverseReportingName))]
    ## TODO (Michael): Values should be filled here to ensure the base
    ##                 is symmetrical for validation.
    mirroredTrade
}
