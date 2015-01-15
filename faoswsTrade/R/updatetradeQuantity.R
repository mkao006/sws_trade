##' This function calculates the trade quantity based on trade value
##' and trade unit value.
##'
##' @param data The data table
##' @param unitValue The column representing trade unit value.
##' @param value The column representing trade value.
##' @param quantity The column representing trade quantity.
##'
##' @export
##' 


updateTradeQuantity = function(data, unitValue, value, quantity){
    updatedTradeQuantity = copy(data)
    updatedTradeQuantity[!is.na(updatedTradeQuantity[[quantity]]),
                    `:=`(c(quantity), get(value)/get(unitValue))]
    updatedTradeQuantity    
}
