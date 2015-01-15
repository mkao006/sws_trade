##' This function impute missing unit value
##'
##' Missing unit value are first imputed by it's mirror value,
##' followed by the median of the value.
##'
##' @param data The data frame
##' @param unitValue The unit value calculated for the reporting country.
##' @param mirrorUnitValue The unit value calculated for the partner country.
##'
##' @export
##' 

imputeUnitValue = function(data, unitValue, mirrorUnitValue){
    imputed = copy(data)
    ## Impute by mirror
    imputed[is.na(imputed[[unitValue]]) & !is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(unitValue), .SD[[mirrorUnitValue]])]
    imputed[is.na(imputed[[mirrorUnitValue]]) & !is.na(imputed[[unitValue]]),
            `:=`(c(mirrorUnitValue), .SD[[unitValue]])]
    
    ## Impute by median
    imputed[is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(mirrorUnitValue), median(.SD[[mirrorUnitValue]], na.rm = TRUE))]
    imputed[is.na(imputed[[unitValue]]),
            `:=`(c(unitValue), median(.SD[[unitValue]], na.rm = TRUE))]
    imputed
}
