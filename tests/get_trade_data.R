suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)    
})


## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_measuredElementTrade_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "3f113726-f40e-44b3-b2af-d5f0de77c386"
        ## token = "7fe7cbec-2346-46de-9a3a-8437eca18e2a"
        )
}


allReportingCountryCode =
    GetCodeList("trade", "ct_raw_tf", dimension = "reportingCountryM49")$code
allPartnerCountryCode =
    GetCodeList("trade", "ct_raw_tf", dimension = "partnerCountryM49")$code
## allItemCode =
##     GetCodeList("trade", "ct_raw_tf", dimension = "measuredItemHS")$code

cerealItemCode =
    adjacent2edge(GetCodeTree("trade", "ct_raw_tf",
                              dimension = "measuredItemHS"))[parent == "10",
                                  children]


## NOTE (Michael): Lets work with elements first, then expand them
##                 when we have the formula table.

allElementCodes = GetCodeList("trade", "ct_raw_tf", "measuredElementTrade")
elementCode = c("5601", "5612", "5621", "5622", "5630", "5901", "5912", "5921",
    "5922", "5930")
elementCodeName = c("importQuantity", "reimportQuantity", "importValue",
    "reimportValue", "importUnitValue", "exportQuantity", "reexportQuantity",
    "exportValue", "reexportValue", "exportUnitValue")

## assign name for convinience
mapply(FUN = function(name, colname){
    assign(x = name, value = colname, envir = .GlobalEnv)
}, name = elementCodeName, colname = paste0(valuePrefix, elementCode))

allYears = as.character(1990:2013)



dimensions =
    list(Dimension(name = "reportingCountryM49", keys = allReportingCountryCode),
         Dimension(name = "partnerCountryM49", keys = allPartnerCountryCode),
         Dimension(name = "measuredItemHS", keys = cerealItemCode),
         Dimension(name = "measuredElementTrade", keys = elementCode),
         Dimension(name = "timePointYears", keys = allYears))

newKey = DatasetKey(domain = "trade", dataset = "ct_raw_tf",
    dimensions = dimensions)

newPivot = c(
    Pivoting(code = "reportingCountryM49", ascending = TRUE),
    Pivoting(code = "partnerCountryM49", ascending = TRUE),
    Pivoting(code = "measuredItemHS", ascending = TRUE),
    Pivoting(code = "timePointYears", ascending = FALSE),
    Pivoting(code = "measuredElementTrade", ascending = TRUE)
)

cerealTrade = GetData(key = newKey, normalized = FALSE, pivoting = newPivot)
cerealTrade[,timePointYears := as.numeric(timePointYears)]
setkeyv(cerealTrade, cols = c("reportingCountryM49", "partnerCountryM49",
                         "measuredItemHS", "timePointYears"))
## NOTE (Michael): The meta data shows which procedure was done to
##                 obtain the value. This is replaced by flag

cerealTrade[, `:=`(c(grep(valuePrefix, colnames(cerealTrade), value = TRUE)),
                   lapply(c(grep(valuePrefix, colnames(cerealTrade),
                                 value = TRUE)),
                          FUN = function(x) as.numeric(cerealTrade[[x]])))]
                          



addRetradeToTrade = function(data, import, reimport, export, reexport){
    missingCol = setdiff(c(import, reimport, export, reexport), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    data[, `:=`(c(import, export),
                list(sum(get(import), get(reimport)),
                     sum(get(export), get(reexport))))]
    ## NOTE (Michael): What do we do with the retrades in the data base?
    data
}

addRetradeToTrade(cerealTrade, import = importQuantity,
                  reimport = reimportQuantity, export = exportQuantity,
                  reexport = reexportQuantity)

removeSelfTrade = function(data, reportingCountry, partnerCountry){
    data = data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
    data
}
removeSelfTrade(cerealTrade, "reportingCountryM49", "partnerCountryM49")

## Function addRetradeToTrade and removeSelfTrade reproduces
## fun.MX.PRELIMS. The hard code are not replicated.

calculateUnitValue = function(data, importUnitValue, importTradeValue,
    importTradeQuantity, exportUnitValue, exportTradeValue, exportTradeQuantity){
    missingCol = setdiff(c(importUnitValue, importTradeValue, importTradeQuantity,
        exportUnitValue, exportUnitValue, exportTradeQuantity), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    data[, `:=`(c(importUnitValue, exportUnitValue),
                list(computeRatio(get(importTradeValue),
                                  get(importTradeQuantity)) * 1000,
                computeRatio(get(exportTradeValue),
                             get(exportTradeQuantity)) * 1000))]
    data
}
calculateUnitValue(cerealTrade, importUnitValue, importValue, importQuantity,
                   exportUnitValue, exportValue, exportQuantity)



## Function calculateUnitValue reproduces fun.MX.BASICVAL




## NOTE (Michael): Why does the fun.MX.RAWVAL has an aggregation over
##                 the key, where does the duplicate transaction come
##                 from?
##
## NOTE (Michael): Why are quantity codes 1 and 8 aggregated?
##
## NOTE (Michael): The calculation of x_tol on line 19 is wrong in
##                 fun.MX.BASICVAL. You can not calculate the ratio of
##                 two sum when the set are different. They should be
##                 indexedx, take the ratio then average it.
##
## NOTE (Michael): Same as above for the mean from line 44 to 55 in
##                 fun.MX.BASICVAL.
##
## NOTE (Michael): The replacement of non-finite value on line 60 and
##                 61, should be removed and corrected. If the unit
##                 value is infinite, this implies the quantity is
##                 zero. Maybe the value should be set as zero as well
##                 while the unitvalue is NA. If the value is
##                 non-zero, maybe correct the value.
##
## NOTE (Michael): There are duplicate in the assignment of
##                 mean.UN_M. The subset is different, the first one
##                 takes the consistent set while the second while
##                 took the original set but with NW greater than the
##                 threshhold MINq.
##
## NOTE (Michael): Whether the original value existed was not checked
##                 before replacement.
##
## NOTE (Michael): What are country codes zero? Unspecified?
##
## NOTE (Michael): Why is the unit value of import higher than export
##                 when mirroring.
##

