##' Function to balance trade quantity based on reliability
##'
##' If discrepency exists between trading partner, then the country
##' with higher reliability index will be taken.
##'
##' @param data The data table.
##' @param import The import quantity of the reporting country.
##' @param export The export quantity of the reporting country.
##' @param reverseImport The import quatity reported by the partner country.
##' @param reverseExport The export quantity reported by the partner country.
##' @param reportingReliability The column representing the calculated
##' reliability for the reporting country.
##' @param partnerReliability The column representing the calculated
##' reliability for the partner country.
##' @param pctTolerance Percentage, the tolerance level of discrepancy
##' between trading partner is deemd tolerable.
##'
##' @export
##' 

balanceTradeQuantity = function(data, import, export, reverseImport, reverseExport,
    reportingReliability, partnerReliability, pctTolerance){
    balanced = copy(data)
    balanced[reportingReliability < partnerReliability &
             (abs(balanced[[import]] - balanced[[reverseExport]])/
                  balanced[[import]] > pctTolerance),
             `:=`(c(import), get(reverseExport))]
    balanced[reportingReliability < partnerReliability &
             (abs(balanced[[export]] - balanced[[reverseImport]])/
                  balanced[[export]]> pctTolerance),
             `:=`(c(export), get(reverseImport))]
    balanced
}
