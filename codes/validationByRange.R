##' This function validates value by removing values which are outliers.
##'
##' Tukey's method is used to determine whether a value is an outlier.
##'
##' @param value The vector of value.
##'
##' @export
##' 


validationByRange = function(value){
    newValue = value
    q = quantile(value, probs = c(0.25, 0.75), na.rm = TRUE)
    min = q[1] - 1.5 * diff(q)
    max = q[2] + 1.5 * diff(q)
    badValue = which(value > max | value < min)
    newValue[badValue] = median(newValue[-badValue], na.rm = TRUE)
    ## list(value = value, newValue = newValue)
    newValue
}
