#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.COMPCOMP()


fun.MX.FULLVAL = function (x2, Min.UV_M,
    Min.UV_X,
    Max.UV_M,
    Max.UV_X,
    median.UV_M,
    median.UV_X,
    euCodes,
    file = "") {

                                        #clean nonsense weights based on nonsense UVs using cons flows max min
    nonse_NW_M <- x2$UV_M > Max.UV_M | x2$UV_M < Min.UV_M
    nonse_NW_X <- x2$UV_X > Max.UV_X | x2$UV_X < Min.UV_X
    x2[nonse_NW_M, "NW_M"] <- x2[nonse_NW_M, "TV_M"] / (median.UV_M / 1000)
    x2[nonse_NW_X, "NW_X"] <- x2[nonse_NW_X, "TV_X"] / (median.UV_X / 1000)
    x2[nonse_NW_M, "UV_M"] <- median.UV_M
    x2[nonse_NW_X, "UV_X"] <- median.UV_X
    x2[nonse_NW_M, "metaData"] <- "nonse NW_M"
    x2[nonse_NW_X, "metaData"] <- "nonse NW_X"

    
                                        #change zeros into counterpart, if exists:
    cond_NW_M <- x2$NW_M == 0 & x2$NW_X > 0
    x2[cond_NW_M, "NW_M"] <- x2[cond_NW_M, "NW_X"]
    x2[cond_NW_M, "UV_M"] <- median.UV_M
    x2[cond_NW_M, "TV_M"] <- x2[cond_NW_M, "NW_X"] * median.UV_M / 1000
    x2[cond_NW_M, "metaData"] <- paste("TM_Xmedian", collapse = "")
    cond_NW_X <- x2$NW_X == 0 & x2$NW_M> 0
    x2[cond_NW_X, "NW_X"] <- x2[cond_NW_X, "NW_M"]
    x2[cond_NW_X, "UV_X"] <- median.UV_X
    x2[cond_NW_X, "TV_X"] <- x2[cond_NW_X, "NW_M"] * median.UV_X / 1000
    x2[cond_NW_X, "metaData"] <- paste("TX_Mmedian", collapse = "")
    
                                        #fill meta for values not changed:
    x2[x2$metaData == "", "metaData"] <- paste("noChange", collapse = "")
                                        #print some info:
    n_tmm <- sum(cond_NW_M)
    n_tmx <- sum(cond_NW_X)
    n_both <- nrow(subset(x2, metaData == "noChange"))
    n <- nrow(x2) / 100

    cat("\t-assigned exports to imports:\t\t", round(n_tmx / n), "%\n", file = file, append = TRUE)
    cat("\t-assigned imports to exports:\t\t", round(n_tmm / n), "%\n", file = file, append = TRUE)
    cat("\t-contain both imports and exports:\t", round(n_both / n), "%\n", file = file, append = TRUE)

    x_noch<-subset(x2, metaData == "noChange")
    cat("Remaining data to align: ", round(100 * nrow(x_noch) / nrow(x2), 2), "%\n", file = file, append = TRUE)
    x_both <- x2#subset(x2, metaData=="noChange")

                                        #impute NW when too big UV min/max of consistent TFs:
    if(nrow(x_cons)/nrow(x2) > 0.2){
        x_bigUV_M <- subset(x_both, UV_M - Max.UV_M > 0)
        x_smallUV_M <- subset(x_both, UV_M - Min.UV_M <= 0)
        if(nrow(x_bigUV_M) > 0){
            x2[rownames(x_bigUV_M), "NW_M"] <- x2[rownames(x_bigUV_M), "NW_X"]
            x2[rownames(x_bigUV_M), "TV_M"] <- x2[rownames(x_bigUV_M), "NW_X"] * median.UV_M / 1000
            x2[rownames(x_bigUV_M), "metaData"] <- paste("impu_outl_bigM", x2[rownames(x_bigUV_M), "metaData"], sep=" ")
        }
        if(nrow(x_smallUV_M) > 0){
            x2[rownames(x_smallUV_M), "NW_M"] <- x2[rownames(x_smallUV_M), "NW_X"]
            x2[rownames(x_smallUV_M), "TV_M"] <- x2[rownames(x_smallUV_M), "NW_X"] * median.UV_M / 1000
            x2[rownames(x_smallUV_M), "metaData"] <- paste("impu_outl_smallM", x2[rownames(x_smallUV_M), "metaData"], sep =" ")
        }
        x_bigUV_X <- subset(x_both, UV_X - Max.UV_X  > 0)
        x_smallUV_X <- subset(x_both, UV_X - Min.UV_X <= 0)
        if(nrow(x_bigUV_X) > 0){
            x2[rownames(x_bigUV_X), "NW_X"] <- x2[rownames(x_bigUV_X), "NW_M"]
            x2[rownames(x_bigUV_X), "TV_X"] <- x2[rownames(x_bigUV_X), "NW_M"] * median.UV_X / 1000
            x2[rownames(x_bigUV_X), "metaData"] <- paste("impu_outl_bigX", x2[rownames(x_bigUV_X), "metaData"], sep = " ")
        }
        if(nrow(x_smallUV_X) > 0){
            x2[rownames(x_smallUV_X), "NW_X"] <- x2[rownames(x_smallUV_X), "NW_M"]
            x2[rownames(x_smallUV_X), "TV_X"] <- x2[rownames(x_smallUV_X), "NW_M"] * median.UV_M / 1000
            x2[rownames(x_smallUV_X), "metaData"] <- paste("impu_outl_smallX", x2[rownames(x_smallUV_X), "metaData"], sep = " ")
        }
    } else {
        x_bigUV_M <- subset(x_both, UV_M / median.UV_M > 3)
        x_smallUV_M <- subset(x_both, UV_M / median.UV_M < 1/3)
        if(nrow(x_bigUV_M) > 0){
            x2[rownames(x_bigUV_M), "NW_M"] <- x2[rownames(x_bigUV_M), "NW_X"]
            x2[rownames(x_bigUV_M), "TV_M"] <- x2[rownames(x_bigUV_M), "NW_X"] * median.UV_M / 1000
            x2[rownames(x_bigUV_M), "metaData"] <- paste("impu_outl_bigM", x2[rownames(x_bigUV_M), "metaData"], sep = " ")
        }
        if(nrow(x_smallUV_M) > 0){
            x2[rownames(x_smallUV_M), "NW_M"] <- x2[rownames(x_smallUV_M), "NW_X"]
            x2[rownames(x_smallUV_M), "TV_M"] <- x2[rownames(x_smallUV_M), "NW_X"] * median.UV_M / 1000
            x2[rownames(x_smallUV_M), "metaData"] <- paste("impu_outl_smallM", x2[rownames(x_smallUV_M), "metaData"], sep = " ")
        }
        x_bigUV_X <- subset(x_both, UV_X / median.UV_X  > 3)
        x_smallUV_X <- subset(x_both, UV_X / median.UV_X < 1/3)
        if(nrow(x_bigUV_X) > 0){
            x2[rownames(x_bigUV_X), "NW_X"] <- x2[rownames(x_bigUV_X), "NW_M"]
            x2[rownames(x_bigUV_X), "TV_X"] <- x2[rownames(x_bigUV_X), "NW_M"] * median.UV_X / 1000
            x2[rownames(x_bigUV_X), "metaData"] <- paste("impu_outl_bigX", x2[rownames(x_bigUV_X), "metaData"], sep = " ")
        }
        if(nrow(x_smallUV_X) > 0){
            x2[rownames(x_smallUV_X), "NW_X"] <- x2[rownames(x_smallUV_X), "NW_M"]
            x2[rownames(x_smallUV_X), "TV_X"] <- x2[rownames(x_smallUV_X), "NW_M"] * median.UV_X / 1000
            x2[rownames(x_smallUV_X), "metaData"] <- paste("impu_outl_smallX", x2[rownames(x_smallUV_X), "metaData"], sep = " ")
        }
    }
    
    x2eu <- x2[x2$rtCode  %in% euCodes & x2$ptCode %in% euCodes, ]
    
    fullVal <- x2
    return(list(fullVal, x2eu)) 

}
