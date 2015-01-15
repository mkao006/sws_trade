##' This function removes quantity and value which are inconsistent
##'
##' Inconsistent value is defined as either one of the quantity or
##' value is zero while the respective is not. In this case, the zero
##' value will be replaced will missing value.
##'
##' @param data The data frame
##' @param quantity The column representing quantity.
##' @param value The column representing value
##'
##' @export
##' 

removeInconsistentQuantityValue = function(data, quantity, value){
    data[data[[quantity]] == 0 & data[[value]] != 0,
         `:=`(c(quantity), NA)]
    data[data[[quantity]] != 0 & data[[value]] == 0,
         `:=`(c(value), NA)]
    data
}
