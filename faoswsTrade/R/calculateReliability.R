##' This function calculates the trade reliability for the reporting
##' country based on quantity.
##'
##' @param data The data table.
##' @param import The import quantity reported by the reporting country.
##' @param export The export quantity reported by the reporting country.
##' @param reversemport The import quantity reported by the partner country.
##' @param reverseExport The export quantity reported by the partner country.
##' @param reportingCountry The column representing the reporting country.
##' @param partnerCountry The column representing the partner country.
##' @param pctTolerance Percentage, the tolerance level of discrepancy
##' between trading partner is deemd tolerable.
##'
##' @export
##' 

calculateReliability = function(data, import, export, reverseImport,
    reverseExport, reportingCountry, partnerCountry, pctTolerance){

    ## TODO (Michael): Discard worst discrepancy
    reliability =
        data[, (sum(abs(.SD[[import]] - .SD[[reverseExport]])/
                        .SD[[import]] <= pctTolerance, na.rm = TRUE) +
               (sum(abs(.SD[[export]] - .SD[[reverseImport]])/
                        .SD[[export]] <= pctTolerance, na.rm = TRUE)))/
                            (2 * .N),
             by = reportingCountry]
    setnames(reliability, old = "V1", new = "reportingReliability")
    reportingReliability = merge(data, reliability, by = reportingCountry,
                                 all.x = TRUE)
    setnames(reliability,
             old = c(reportingCountry, "reportingReliability"),
             new = c(partnerCountry, "partnerReliability"))
    finalReliability = merge(reportingReliability, reliability,
        by = partnerCountry, all.x = TRUE)
    finalReliability
}
