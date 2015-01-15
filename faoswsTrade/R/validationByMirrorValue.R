##' This function validates trade by compare the unit value of the
##' reporting country and the partner country.
##'
##' The validation is done via total least squares, value outside of
##' the ratio bound are perceived as unreliable and will be replaced
##' by the expected value of the total least squares fit.
##'
##' @param value The value reported by the reporting country
##' @param mirrorValue The value reported by the partner country.
##' @param ratioBoundary A real number which is multiplied to the
##' slope of the total least squares to determine the decision bounds.
##'
##' @export
##' 


validationByMirrorValue = function(value, mirrorValue, ratioBoundary,
    plot = FALSE){
    pr = prcomp(~value + mirrorValue)
    slope = pr$rotation[2,1] / pr$rotation[1,1]
    intercept = pr$center[2] - slope*pr$center[1]
    valueRatio = (mirrorValue - intercept)/value

    ## Ratios which are outside the range.
    badRatio = which(valueRatio > slope * ratioBoundary |
                     valueRatio < slope/ratioBoundary)
    svec = c(1, slope)
    ## projection on to the orthorgonal line
    valueBasedOnExpectedRatio  =
        ((matrix(c(value, mirrorValue - intercept), nc = 2) %*% svec)/
            c(svec %*% svec)) %*% matrix(svec, nc = 2)
    ## Assing the new values to those that lies out side the ratio range
    newValue = value
    newMirrorValue = mirrorValue
    newValue[badRatio] = valueBasedOnExpectedRatio[badRatio, 1]
    newMirrorValue[badRatio] = valueBasedOnExpectedRatio[badRatio, 2] + intercept
    if(plot){
        plot(value, mirrorValue, pch = 19, col = "red")
        abline(a = intercept, b = slope)
        abline(a = intercept, b = slope * ratioBoundary, col = "red")
        abline(a = intercept, b = slope/ratioBoundary, col = "red")        
        points(newValue, newMirrorValue, col = "blue", pch = 19)
    }
    ## list(intercept = intercept, slope= slope, newValue = newValue,
    ##      newMirrorValue = newMirrorValue)
    list(newValue, newMirrorValue)
}
