##' This function updates the trade value.
##'
##' @param data The data table
##' @param unitValue The column representing trade unit value.
##' @param value The column representing trade value.
##' @param quantity The column representing trade quantity.
##'
##' @export


updateTradeValue = function(data, unitValue, value, quantity){
    updatedTradeValue = copy(data)
    updatedTradeValue[!is.na(updatedTradeValue[[value]]),
                    `:=`(c(value), get(quantity) * get(unitValue))]
    updatedTradeValue    
}
