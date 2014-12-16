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
elementCode = c("5600", "5612", "5621", "5622", "5630", "5900", "5912", "5921",
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
                          FUN = function(x) as.numeric(.SD[[x]])))]


addRetradeToTrade = function(data, import, reimport, export, reexport){
    missingCol = setdiff(c(import, reimport, export, reexport), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    data[, `:=`(c(import, export),
                list(ifelse(apply(is.na(.SD[, c(import, reimport), with = FALSE]),
                                  1, all), NA,
                            rowSums(.SD[, c(import, reimport), with = FALSE],
                                    na.rm=TRUE)),
                     ifelse(apply(is.na(.SD[, c(export, reexport), with = FALSE]),
                                  1, all), NA,
                            rowSums(.SD[, c(export, reexport), with = FALSE],
                                    na.rm=TRUE))))]
    data[, `:=`(c(reimport, reexport), NA)]
    ## NOTE (Michael): What do we do with the retrades in the data base?
    data
}

addRetradeToTrade(cerealTrade, import = importQuantity,
                  reimport = reimportQuantity, export = exportQuantity,
                  reexport = reexportQuantity)


removeSelfTrade = function(data, reportingCountry, partnerCountry){
    noSelfTrade = data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
    noSelfTrade
}
removeSelfTrade(cerealTrade, "reportingCountryM49", "partnerCountryM49")



removeInconsistentQuantityValue = function(data, quantity, value){
    data[data[[quantity]] == 0 & data[[value]] != 0,
         `:=`(c(quantity), NA)]
    data[data[[quantity]] != 0 & data[[value]] == 0,
         `:=`(c(value), NA)]
    data
}

removeInconsistentQuantityValue(data = cerealTrade,
                                quantity = "Value_measuredElementTrade_5601",
                                value = "Value_measuredElementTrade_5621")

removeInconsistentQuantityValue(data = cerealTrade,
                                quantity = "Value_measuredElementTrade_5901",
                                value = "Value_measuredElementTrade_5921")


mirrorTrade = function(data, reportingCountry, partnerCountry, reverseTradePrefix,
                       valueColumns, flagColumns){
    base = copy(data)
    tmp = data[, !flagColumns, with = FALSE]

    reverseReportingName = paste0(reverseTradePrefix, reportingCountry)
    reversePartnerName = paste0(reverseTradePrefix, partnerCountry)

    setnames(tmp, old = c(reportingCountry, partnerCountry),
             new = c(reverseReportingName, reversePartnerName))

    base[, `:=`(c(reverseReportingName, reversePartnerName),
                list(.SD[[partnerCountry]], .SD[[reportingCountry]]))]

    setnames(tmp, old = valueColumns,
             new = paste0(reverseTradePrefix, valueColumns))

    ## print(str(base))
    ## print(str(tmp))
    ## print(flagColumns)
    ## print(intersect(colnames(base), colnames(tmp)))
    mirroredTrade = merge(base, tmp,
        by = intersect(colnames(base), colnames(tmp)), all = TRUE)
    ## print(str(mirroredTrade))
    ## Fill in the missing trade country name
    ## print(reportingCountry)
    ## print(partnerCountry)
    ## mirroredTrade[is.na(.SD[[reportingCountry]]),
    ##             `:=`(c(reportingCountry), get(reversePartnerName))]
    ## mirroredTrade[is.na(.SD[[partnerCountry]]),
    ##             `:=`(c(parnterCountry), get(reverseReportingName))]
    ## TODO (Michael): Values should be filled here to ensure the base
    ##                 is symmetrical for validation.
    mirroredTrade
}

mirroredTrade =
    mirrorTrade(data = cerealTrade,
                reportingCountry = "reportingCountryM49",
                partnerCountry = "partnerCountryM49",
                reverseTradePrefix = "reverse_",
                valueColumns = grep("Value_", colnames(cerealTrade), value = TRUE))


write.csv(mirroredTrade[, !grep("flag", colnames(mirroredTrade)), with = FALSE],
          file = "checkFormat.csv", na = "", row.names = FALSE)

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



validationByMirrorValue = function(value, mirrorValue, pctTolerance){
    pr = prcomp(~value + mirrorValue)
    slope = pr$rotation[2,1] / pr$rotation[1,1]
    intercept = pr$center[2] - slope*pr$center[1]
    valueRatio = (mirrorValue - intercept)/value

    ## Ratios which are outside the range.
    badRatio = which(valueRatio > slope * (1 + pctTolerance/100) |
                     valueRatio < slope * (1 - pctTolerance/100))
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
    ## list(intercept = intercept, slope= slope, newValue = newValue,
    ##      newMirrorValue = newMirrorValue)
    list(newValue, newMirrorValue)
}

x = 1:100 + rnorm(100, sd = 10)
y = 2 + 2 * x + rnorm(100, sd = 30)
plot(x, y, cex = 2)
abline(coef = coef(lm(y ~ x)), col = "red")
with(validationByMirrorValue(x, y, 30),
     abline(a = intercept, b = slope))
with(validationByMirrorValue(x, y, 30),
     points(newValue, newMirrorValue, col = "blue", pch = 19))


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

validation = function(data, value, mirrorValue, pctTolerance = 50){
    valid = copy(data)
    valid[, `:=`(c(value, mirrorValue),
                 validationByMirrorValue(.SD[[value]], .SD[[mirrorValue]],
                                         pctTolerance = pctTolerance))]
    valid[, `:=`(c(value), validationByRange(.SD[[value]]))]
    valid[, `:=`(c(mirrorValue), validationByRange(.SD[[mirrorValue]]))]
    valid
}


x = rexp(300)
hist(x, breaks = 100)
with(validationByRange(x), plot(value, newValue))

par(mfrow = c(2, 1))
hist(log(x), breaks = 100)
with(validationByRange(log(x)), hist(exp(newValue), breaks = 100, xlim = range(x)))


rawValues =
    cerealTrade %>%
    removeSelfTrade(data = ., reportingCountry = "reportingCountryM49",
                    partnerCountry = "partnerCountryM49") %>%
    removeInconsistentQuantityValue(data = ., quantity = importQuantity,
                                    value = importValue) %>%
    removeInconsistentQuantityValue(data = ., quantity = exportQuantity,
                                    value = exportValue) %>% 
    addRetradeToTrade(data = ., import = importQuantity,
                      reimport = reimportQuantity, export = exportQuantity,
                      reexport = reexportQuantity)

validUnitValue =
    rawValues %>%
    mirrorTrade(data = .,
                reportingCountry = "reportingCountryM49",
                partnerCountry = "partnerCountryM49",
                reverseTradePrefix = "reverse_",
                valueColumns = grep("Value", colnames(.), value = TRUE),
                flagColumns = grep("flag", colnames(.), value = TRUE)) %>%
    calculateUnitValue(data = .,
                       importUnitValue = importUnitValue,
                       importTradeValue = importValue,
                       importTradeQuantity = importQuantity,
                       exportUnitValue = exportUnitValue,
                       exportTradeValue = exportValue,
                       exportTradeQuantity = exportQuantity) %>%
    calculateUnitValue(data = .,
                       importUnitValue = paste0("reverse_", importUnitValue),
                       importTradeValue = paste0("reverse_", importValue),
                       importTradeQuantity = paste0("reverse_", importQuantity),
                       exportUnitValue = paste0("reverse_", exportUnitValue),
                       exportTradeValue = paste0("reverse_", exportValue),
                       exportTradeQuantity = paste0("reverse_", exportQuantity)) %>%
    validation(data = .,
               value = "Value_measuredElementTrade_5630",
               mirrorValue = "reverse_Value_measuredElementTrade_5930") %>%
    validation(data = .,
               value = "Value_measuredElementTrade_5930",
               mirrorValue = "reverse_Value_measuredElementTrade_5630")

par(mfrow = c(2, 1))
plot(na.omit(validUnitValue[, list(Value_measuredElementTrade_5630,
                                   reverse_Value_measuredElementTrade_5930)]))
abline(a = 0, b = 1, col = "red")
plot(na.omit(validUnitValue[, list(Value_measuredElementTrade_5930,
                                   reverse_Value_measuredElementTrade_5630)]))
abline(a = 0, b = 1, col = "red")

balancedTrade =
    validaUnitValue %>%
    calculateQuantity %>%
    calculateReliability %>%
    balanceTradeQuantity %>%
    calculateValue



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

