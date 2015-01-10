##' This function calculates the unit value of the trade.
##'
##' @param data The data frame
##' @param importUnitValue The column representing the import unit value.
##' @param importTradeValue The column representing the import trade value.
##' @param importTradeQuantity The column representing the import trade quantity.
##' @param exportUnitValue The column representing the export unit value.
##' @param exportTradeValue The column representing the export trade value.
##' @param exportTradeQuantity The column representing the export trade quantity.
##'
##' @export
##' 


calculateUnitValue = function(data, importUnitValue, importTradeValue,
    importTradeQuantity, exportUnitValue, exportTradeValue, exportTradeQuantity){
    missingCol = setdiff(c(importUnitValue, importTradeValue, importTradeQuantity,
        exportUnitValue, exportUnitValue, exportTradeQuantity), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    
    data[, `:=`(c(importUnitValue, exportUnitValue),
                list(computeRatio(get(importTradeValue),
                                  get(importTradeQuantity)),
                computeRatio(get(exportTradeValue),
                             get(exportTradeQuantity))))]
    data
}
