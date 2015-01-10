##' This is the wrapper function of the mirror validation and range validation.
##'
##' @param data The data frame
##' @param value The value reported by the reporting country
##' @param mirrorValue The value reported by the partner country
##' @param ratioBoundary A real number which is multiplied to the
##' slope of the total least squares to determine the decision bounds.
##' @param plot logical, whether the graph of the TLS should be plotted.
##'
##' @export
##'


validation = function(data, value, mirrorValue, ratioBoundary = 3, plot = FALSE){
    valid = copy(data)
    valid[, `:=`(c(value, mirrorValue),
                 validationByMirrorValue(.SD[[value]], .SD[[mirrorValue]],
                                         ratioBoundary = ratioBoundary,
                                         plot = plot))]
    valid[, `:=`(c(value), validationByRange(.SD[[value]]))]
    valid[, `:=`(c(mirrorValue), validationByRange(.SD[[mirrorValue]]))]
    valid
}
