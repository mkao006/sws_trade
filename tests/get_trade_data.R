suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)    
})


## Setting up variables
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"
## flagObsPrefix = "flagObservationStatus_"
## flagMethodPrefix = "flagMethod_"
reverseTradePrefix = "reverse_"

## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "3f113726-f40e-44b3-b2af-d5f0de77c386"
        ## token = "7fe7cbec-2346-46de-9a3a-8437eca18e2a"
        )
}

## Get all reporting country codes
allReportingCountryCode =
    GetCodeList("trade", "ct_raw_tf", dimension = reportingCountryVar)$code

## Get all partner country codes
allPartnerCountryCode =
    GetCodeList("trade", "ct_raw_tf", dimension = partnerCountryVar)$code

## Get Item codes
##
## NOTE (Michael): Working with cereals for now.

## allItemCode =
##     GetCodeList("trade", "ct_raw_tf", dimension = "measuredItemHS")$code

cerealItemCode =
    adjacent2edge(
        GetCodeTree("trade", "ct_raw_tf", dimension = itemVar)
    )[parent == "10", children]


## Get relevant element codes and set names
##
## NOTE (Michael): Lets work with set elements first, then expand them
##                 when we have the formula table.

allElementCodes = GetCodeList("trade", "ct_raw_tf", "measuredElementTrade")
elementCode = c("5600", "5612", "5621", "5622", "5630", "5900", "5912", "5921",
    "5922", "5930")
elementCodeName = c("importQuantity", "reimportQuantity", "importValue",
    "reimportValue", "importUnitValue", "exportQuantity", "reexportQuantity",
    "exportValue", "reexportValue", "exportUnitValue")

## assign name globally for convinience
mapply(FUN = function(name, colname){
    assign(x = name, value = colname, envir = .GlobalEnv)
}, name = elementCodeName,
       colname = paste0(valuePrefix, elementVar, "_", elementCode))

## TODO (Michael): Need to do one for reverse trade

## Get all relevant years.
allYears = as.character(1990:2013)


getComtradeRawData = function(){
    dimensions =
        list(Dimension(name = "reportingCountryM49",
                       keys = allReportingCountryCode),
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

    comtradeRaw = GetData(key = newKey, normalized = FALSE, pivoting = newPivot)
    comtradeRaw[,timePointYears := as.numeric(timePointYears)]
    setkeyv(comtradeRaw, cols = c("reportingCountryM49", "partnerCountryM49",
                             "measuredItemHS", "timePointYears"))
    ## NOTE (Michael): The meta data shows which procedure was done to
    ##                 obtain the value. This is replaced by flag

    comtradeRaw[, `:=`(c(grep(valuePrefix, colnames(comtradeRaw), value = TRUE)),
                       lapply(c(grep(valuePrefix, colnames(comtradeRaw),
                                     value = TRUE)),
                              FUN = function(x) as.numeric(.SD[[x]])))]
    comtradeRaw
}

addRetradeToTrade = function(data, importQuantity, reimportQuantity,
    exportQuantity, reexportQuantity, importValue, reimportValue,
    exportValue, reexportValue){
    missingCol =
        setdiff(c(importQuantity, reimportQuantity, exportQuantity,
                  reexportQuantity, importValue, reimportValue, exportValue,
                  reexportValue),
                colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    data[, `:=`(c(importQuantity, exportQuantity, importValue, exportValue),
                list(ifelse(apply(is.na(.SD[, c(importQuantity, reimportQuantity),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(importQuantity, reimportQuantity),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(exportQuantity, reexportQuantity),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(exportQuantity, reexportQuantity),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(importValue, reimportValue),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(importValue, reimportValue),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(exportValue, reexportValue),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(exportValue, reexportValue),
                                        with = FALSE], na.rm = TRUE))))]
    data[, `:=`(c(reimportQuantity, reexportQuantity,
                  reimportValue, reexportValue), NA)]
    ## NOTE (Michael): What do we do with the retrades in the data base?
    data
}


removeSelfTrade = function(data, reportingCountry, partnerCountry){
    noSelfTrade = data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
    noSelfTrade
}


removeInconsistentQuantityValue = function(data, quantity, value){
    data[data[[quantity]] == 0 & data[[value]] != 0,
         `:=`(c(quantity), NA)]
    data[data[[quantity]] != 0 & data[[value]] == 0,
         `:=`(c(value), NA)]
    data
}


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

    mirroredTrade = merge(base, tmp,
        by = intersect(colnames(base), colnames(tmp)), all = TRUE)
    ## Fill in the missing trade country name
    ## mirroredTrade[is.na(.SD[[reportingCountry]]),
    ##             `:=`(c(reportingCountry), get(reversePartnerName))]
    ## mirroredTrade[is.na(.SD[[partnerCountry]]),
    ##             `:=`(c(parnterCountry), get(reverseReportingName))]
    ## TODO (Michael): Values should be filled here to ensure the base
    ##                 is symmetrical for validation.
    mirroredTrade
}

calculateUnitValue = function(data, importUnitValue, importTradeValue,
    importTradeQuantity, exportUnitValue, exportTradeValue, exportTradeQuantity){
    missingCol = setdiff(c(importUnitValue, importTradeValue, importTradeQuantity,
        exportUnitValue, exportUnitValue, exportTradeQuantity), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    
    data[, `:=`(c(importUnitValue, exportUnitValue),
                list(computeRatio(get(importTradeValue),
                                  get(importTradeQuantity)),
                computeRatio(get(exportTradeValue),
                             get(exportTradeQuantity))))]
    data
}



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

## x = 1:100 + rnorm(100, sd = 10)
## y = 2 + 2 * x + rnorm(100, sd = 30)
## plot(x, y, cex = 2)
## abline(coef = coef(lm(y ~ x)), col = "red")
## with(validationByMirrorValue(x, y, 30),
##      abline(a = intercept, b = slope))
## with(validationByMirrorValue(x, y, 30),
##      points(newValue, newMirrorValue, col = "blue", pch = 19))


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

imputeUnitValue = function(data, unitValue, mirrorUnitValue){
    imputed = copy(data)
    imputed[is.na(imputed[[unitValue]]) & !is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(unitValue), .SD[[mirrorUnitValue]])]
    imputed[is.na(imputed[[mirrorUnitValue]]) & !is.na(imputed[[unitValue]]),
            `:=`(c(mirrorUnitValue), .SD[[unitValue]])]
    imputed[is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(mirrorUnitValue), median(.SD[[mirrorUnitValue]], na.rm = TRUE))]
    imputed[is.na(imputed[[unitValue]]),
            `:=`(c(unitValue), median(.SD[[unitValue]], na.rm = TRUE))]
    imputed
}

updateTradeQuantity = function(data, unitValue, value, quantity){
    updatedTradeQuantity = copy(data)
    updatedTradeQuantity[!is.na(updatedTradeQuantity[[quantity]]),
                    `:=`(c(quantity), get(value)/get(unitValue))]
    updatedTradeQuantity    
}


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
    setnames(reliability, old = c(reportingCountry, "reportingReliability"),
             new = c(partnerCountry, "partnerReliability"))
    finalReliability = merge(reportingReliability, reliability,
        by = partnerCountry, all.x = TRUE)
    finalReliability
}

## reliabilityTrade = calculateReliability(mirroredTrade,
##                      import = importQuantity,
##                      export = exportQuantity,
##                      reverseImport = paste0("reverse_", importQuantity),
##                      reverseExport = paste0("reverse_", exportQuantity),
##                      reportingCountry = "reportingCountryM49",
##                      partnerCountry = "partnerCountryM49",
##                      pctTolerance = 0.05)



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

## balancedTrade =
##     balanceTradeQuantity(reliabilityTrade,
##                          import = importQuantity,
##                          export = exportQuantity,
##                          reverseImport = paste0("reverse_", importQuantity),
##                          reverseExport = paste0("reverse_", exportQuantity),
##                          reportingReliability = "reportingReliability",
##                          partnerReliability = "partnerReliability",
##                          pctTolerance = 0.05)



updateTradeValue = function(data, unitValue, value, quantity){
    updatedTradeValue = copy(data)
    updatedTradeValue[!is.na(updatedTradeValue[[value]]),
                    `:=`(c(value), get(quantity) * get(unitValue))]
    updatedTradeValue    
}

## x = rexp(300)
## hist(x, breaks = 100)
## with(validationByRange(x), plot(value, newValue))

## par(mfrow = c(2, 1))
## hist(log(x), breaks = 100)
## with(validationByRange(log(x)), hist(exp(newValue), breaks = 100, xlim = range(x)))


test = getComtradeRawData()
load("cerealTrade.RData")
cerealTrade[,timePointYears := as.numeric(timePointYears)]


rawValues =
    cerealTrade %>%
    removeSelfTrade(data = ., reportingCountry = reportingCountryVar,
                    partnerCountry = partnerCountryVar) %>%
    removeInconsistentQuantityValue(data = ., quantity = importQuantity,
                                    value = importValue) %>%
    removeInconsistentQuantityValue(data = ., quantity = exportQuantity,
                                    value = exportValue) %>% 
    addRetradeToTrade(data = .,
                      importQuantity = importQuantity,
                      reimportQuantity = reimportQuantity,
                      exportQuantity = exportQuantity,
                      reexportQuantity = reexportQuantity,
                      importValue = importValue,
                      reimportValue = reimportValue,
                      exportValue = exportValue,
                      reexportValue = reexportValue)

mirrorData =
    rawValues %>%
    mirrorTrade(data = .,
                reportingCountry = reportingCountryVar,
                partnerCountry = partnerCountryVar,
                reverseTradePrefix = reverseTradePrefix,
                valueColumns = grep(valuePrefix, colnames(.), value = TRUE),
                flagColumns = grep(flagPrefix, colnames(.), value = TRUE))

validUnitValue =
    mirrorData %>%
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
               value = importUnitValue,
               mirrorValue = paste0("reverse_", exportUnitValue),
               pctTolerance = 50) %>%
    validation(data = .,
               value = exportUnitValue,
               mirrorValue = paste0("reverse_", importUnitValue),
               pctTolerance = 50) %>%
    imputeUnitValue(data = .,
                    unitValue = importUnitValue,
                    mirrorUnitValue = paste0("reverse_", exportUnitValue)) %>%
    imputeUnitValue(data = .,
                    unitValue = exportUnitValue,
                    mirrorUnitValue = paste0("reverse_", importUnitValue))
                        



balancedTrade =
    validUnitValue %>%
    updateTradeQuantity(data = .,
                      unitValue = importUnitValue,
                      value = importValue,
                      quantity = importQuantity) %>%
    updateTradeQuantity(data = .,
                      unitValue = exportUnitValue,
                      value = exportValue,
                      quantity = exportQuantity) %>%
    updateTradeQuantity(data = .,
                      unitValue = paste0("reverse_", importUnitValue),
                      value = paste0("reverse_", importValue),
                      quantity = paste0("reverse_", importQuantity)) %>%
    updateTradeQuantity(data = .,
                      unitValue = paste0("reverse_", exportUnitValue),
                      value = paste0("reverse_", exportValue),
                      quantity = paste0("reverse_", exportQuantity)) %>%
    calculateReliability(data = .,
                         import = importQuantity,
                         export = exportQuantity,
                         reverseImport = paste0("reverse_", importQuantity),
                         reverseExport = paste0("reverse_", exportQuantity),
                         reportingCountry = "reportingCountryM49",
                         partnerCountry = "partnerCountryM49",
                         pctTolerance = 0.05) %>% 
    balanceTradeQuantity(data = .,
                         import = importQuantity,
                         export = exportQuantity,
                         reverseImport = paste0("reverse_", importQuantity),
                         reverseExport = paste0("reverse_", exportQuantity),
                         reportingReliability = "reportingReliability",
                         partnerReliability = "partnerReliability",
                         pctTolerance = 0.05) %>%
    updateTradeValue(data = .,
                      unitValue = importUnitValue,
                      value = importValue,
                      quantity = importQuantity) %>%
    updateTradeValue(data = .,
                      unitValue = exportUnitValue,
                      value = exportValue,
                      quantity = exportQuantity) %>%
    ## NOTE (Michael): Calculation of the revser is probably not
    ##                 required, since they will be discarded.
    updateTradeValue(data = .,
                      unitValue = paste0("reverse_", importUnitValue),
                      value = paste0("reverse_", importValue),
                      quantity = paste0("reverse_", importQuantity)) %>%
    updateTradeValue(data = .,
                      unitValue = paste0("reverse_", exportUnitValue),
                      value = paste0("reverse_", exportValue),
                      quantity = paste0("reverse_", exportQuantity)) 





## write.csv(validUnitValue[, !grep("flag", colnames(validUnitValue), value = TRUE),
##                          with = FALSE],
##           file = "checkUnitValue.csv", row.names = FALSE,
##           na = "")

## par(mfrow = c(2, 1))
## plot(na.omit(mirrorData[, list(Value_measuredElementTrade_5630,
##                                    reverse_Value_measuredElementTrade_5930)]),
##      col = "red", cex = 2, xlim = c(0, 10000), ylim = c(0, 10000))
## points(na.omit(validUnitValue[, list(Value_measuredElementTrade_5630,
##                                    reverse_Value_measuredElementTrade_5930)]),
##        pch = 19)
## abline(a = 0, b = 1, col = "red")
## plot(na.omit(mirrorData[, list(Value_measuredElementTrade_5930,
##                                    reverse_Value_measuredElementTrade_5630)]),
##      col = "red", cex = 2, xlim = c(0, 10000), ylim = c(0, 10000))
## points(na.omit(validUnitValue[, list(Value_measuredElementTrade_5630,
##                                    reverse_Value_measuredElementTrade_5930)]),
##        pch = 19)
## abline(a = 0, b = 1, col = "red")







## NOTE (Michael): Looks like there is something wrong with projection.
##
## NOTE (Michael): Double check the mirror function, it appears to be
##                 doing something wrong.
##
## TODO (Michael): Remove hard coded names.
##
## TODO (Michael): Make names more meaningful, the tolerance should be
##                 specific to each function such as unit value
##                 tolerance and quantity tolerance. Further, value
##                 should be trade value etc.
##
## TODO (Michael): Need to think about flags, we can just make
##                 everything official for now I suppose.
##
## TODO (Michael): Maybe write a wrapper for each procedure.
##
## TODO (Michael): fill missing unit value after validation.
