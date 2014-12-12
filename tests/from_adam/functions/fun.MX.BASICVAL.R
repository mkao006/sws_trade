#' Function to compile fbs primary domestic availability
#'
#' This function used OCBS data from EST and then calculates feed.
#' @param start year.
#' @keywords feed
#' @export
#' @examples
#' fun.MX.BASICVAL()


fun.MX.BASICVAL = function (x, euCodes, file = "") {
    
                                        # if(!all(colnames(x) %in% c("Year", "rtCode", "ptCode", "qtCode", "TV_M", "TV_X", 
                                        # "NW_M", "NW_X", "UV_M", "UV_X", "metaData"))) 
                                        # stop("Input not as expected")
    
    
                                        #find consistent data and central tendencies
    x_tol <- (sum(x$NW_X, na.rm = TRUE) + sum(x$NW_M, na.rm = TRUE)) / nrow(x) * .05
    x_cons <<- subset(x, (UV_M / UV_X) < 2 & (UV_X / UV_M) > 0.5 & NW_X > x_tol)
    ratio_cons <<- sum(x_cons$NW_X, na.rm = TRUE) / sum(x$NW_X, na.rm = TRUE)

    cat ("Total exports: ", sum(x$NW_X, na.rm = TRUE), "\n",sep = "", file = file, append = TRUE) 
    cat ("Total imports: ", sum(x$NW_M, na.rm = TRUE), "\n",sep = "", file = file, append = TRUE) 
    cat ("ratio of quasi consistent export volume: ", ratio_cons, "\n",sep = "", file = file, append = TRUE) 

    MINq <- median(x$NW_M, na.rm = TRUE) / 10
    
    if(!is.na(ratio_cons) & ratio_cons > 0.09){ 
                                        #consistent ratio above threshold
        Min.UV_X <- min(x_cons[x_cons$NW_X > MINq, ]$UV_X, na.rm = TRUE)
        Max.UV_X <- max(x_cons[x_cons$NW_X > MINq, ]$UV_X, na.rm = TRUE)
        Min.UV_M <- min(x_cons[x_cons$NW_M > MINq, ]$UV_M, na.rm = TRUE)
        Max.UV_M <- max(x_cons[x_cons$NW_M > MINq, ]$UV_M, na.rm = TRUE)
        
        cat("Min.UV_X: ", Min.UV_X, "\n",sep = "", file = file, append = TRUE) 
        cat("Min.UV_M: ", Min.UV_M, "\n",sep = "", file = file, append = TRUE) 
        cat("Max.UV_X: ", Max.UV_X, "\n",sep = "", file = file, append = TRUE) 
        cat("Max.UV_M: ", Max.UV_M, "\n",sep = "", file = file, append = TRUE)
        
        cat("ratio of quasi consistent transactions: ", nrow(x_cons)/nrow(x), "\n", sep = "", file = file, append = TRUE)
        cat("ratio of quasi consistent transactions: ", nrow(x_cons)/nrow(x), "\n")  
        
        if(any(x_cons$UV_M > 0 & !is.na(x_cons$UV_M))){
            mean.UV_M <- sum(x_cons$TV_M, na.rm = TRUE) / sum(x_cons$NW_M, na.rm = TRUE) * 1000
            median.UV_M <- median(x_cons$UV_M, na.rm = TRUE)
            cat ("mean.UV_M= ", mean.UV_M, "\n", sep = "", file = file, append = TRUE) 
            cat ("median.UV_M= ", median.UV_M, "\n", sep = "", file = file, append = TRUE)
        }
        if(any(x_cons$UV_X > 0 & !is.na(x_cons$UV_X))){
            mean.UV_X <- sum(x_cons$TV_X, na.rm = TRUE) / sum(x_cons$NW_X, na.rm = TRUE) * 1000
            median.UV_X <- median(x_cons$UV_X, na.rm = TRUE)
            cat ("mean.UV_X= ", mean.UV_X, "\n", sep = "", file = file, append = TRUE) 
            cat ("median.UV_X= ", median.UV_X, "\n", sep = "", file = file, append = TRUE)
        }
    } else {
        
                                        #consistent ratio below threshold
        
        x$UV_M[!is.finite(x$UV_M)] <- NA
        x$UV_X[!is.finite(x$UV_X)] <- NA
        
        Min.UV_X <- mean(x[x$NW_X > MINq, ]$UV_X, na.rm = TRUE) * 0.25
        Max.UV_X <- mean(x[x$NW_X > MINq, ]$UV_X, na.rm = TRUE) * 4
        Min.UV_M <- mean(x[x$NW_M > MINq, ]$UV_M, na.rm = TRUE) * 0.25
        Max.UV_M <- mean(x[x$NW_M > MINq, ]$UV_M, na.rm = TRUE) * 4
        
        if(any(x$UV_M > 0 & !is.na(x$UV_M))){
            mean.UV_M <- sum(x[x$NW_M > MINq, ]$TV_M, na.rm = TRUE) / sum(x[x$NW_M > MINq, ]$NW_M, na.rm = TRUE) * 1000
            median.UV_M <- median(x[x$NW_M > MINq, ]$UV_M, na.rm = TRUE)
            cat ("mean.UV_M = ", mean.UV_M, "\n", sep = "", file = file, append = TRUE) 
            cat ("median.UV_M = ", median.UV_M, "\n", sep = "", file = file, append = TRUE)
        } else { 
            mean.UV_M <- mean.UV_X * 1.1
            Min.UV_M <- Min.UV_X * 1.1
            Max.UV_M <- Max.UV_X * 1.1
            median.UV_M <- median.UV_X * 1.1
        }
        
        if(any(x$UV_X > 0 & !is.na(x$UV_X))){
            mean.UV_X <- sum(x[x$NW_X > MINq, ]$TV_X, na.rm = TRUE) / sum(x[x$NW_X > MINq, ]$NW_X, na.rm = TRUE) * 1000
            median.UV_X <- median(x[x$NW_X > MINq, ]$UV_X, na.rm = TRUE)
            cat ("mean.UV_X= ", mean.UV_X, "\n", sep = "", file = file, append = TRUE) 
            cat ("median.UV_X= ", median.UV_X, "\n", sep = "", file = file, append = TRUE) 
        } else { 
            mean.UV_X <- mean.UV_M * .9
            Min.UV_X <- Min.UV_M * .9
            Max.UV_X <- Max.UV_M * .9
            median.UV_X <- median.UV_M * .9
        }
        if(is.na(mean.UV_X)){
            mean.UV_X <- mean.UV_M * .9
            Min.UV_X <- Min.UV_M * .9
            Max.UV_X <- Max.UV_M * .9
        }
        if(is.na(median.UV_X)){
            median.UV_X <- median.UV_M * .9
        }
        if(is.na(mean.UV_M)){
            mean.UV_M <- mean.UV_X * 1.1
            Min.UV_M <- Min.UV_X * 1.1
            Max.UV_M <- Max.UV_X * 1.1
        }
        if(is.na(median.UV_X)){
            median.UV_M <- median.UV_X * 1.1
        }
    }

                                        #change NA inf into zeros
    to0 <- sapply(x[, c("TV_M", "TV_X","NW_M", "NW_X", "UV_M", "UV_X")],
                  function(x) ifelse(is.na(x), 0, x))
    if(is.numeric(to0) & is.null(dim(to0))) to0 <- as.data.frame(matrix(to0, nrow = 1, dimnames = list("a", names(to0))))
    x2 <- cbind(x[, c("Year", "rtCode", "ptCode", "qtCode")], to0, x[, "metaData", drop = FALSE]) 
    is.na(x2) <- do.call(cbind, lapply(x2, is.infinite))
    x2[is.na(x2)] <- 0

    x2[x2$rtCode %in% nonRepImpList, "NW_M"] <- x2[x2$rtCode %in% nonRepImpList, "NW_X"]
    x2[x2$rtCode %in% nonRepImpList, "UV_M"] <- x2[x2$rtCode %in% nonRepImpList, "UV_X"] * 1.1 #(median.UV_M/median.UV_X)
    x2[x2$rtCode %in% nonRepImpList, "TV_M"] <- x2[x2$rtCode %in% nonRepImpList, "TV_X"] * 1.1 # (median.UV_M/1000)
    x2[x2$rtCode %in% nonRepImpList, "metaData"] <-"mirrored import for non-reporter"
    
    x2[x2$ptCode %in% nonRepExpList, "NW_X"] <- x2[x2$ptCode %in% nonRepExpList, "NW_M"]
    x2[x2$ptCode %in% nonRepExpList, "UV_X"] <- x2[x2$ptCode %in% nonRepExpList, "UV_M"] * 0.9 # (median.UV_X/median.UV_M)
    x2[x2$ptCode %in% nonRepExpList, "TV_X"] <- x2[x2$ptCode %in% nonRepExpList, "TV_M"] * 0.9 # (median.UV_X/1000)
    x2[x2$ptCode %in% nonRepExpList, "metaData"] <- "mirrored export for non-reporter"
    
    
                                        #deal with NW=0 & TRADE VALUE> 0, i.e. qtCode=1
    if(any(x2$qtCode==1)){
        x2[x2$qtCode==1, "NW_M"] <- x2[x2$qtCode == 1, "TV_M"] / median.UV_M * 1000
	x2[x2$qtCode==1, "NW_X"] <- x2[x2$qtCode == 1, "TV_X"] / median.UV_X * 1000
	x2[x2$qtCode==1, "UV_M"] <- median.UV_M
        x2[x2$qtCode==1, "UV_X"] <- median.UV_X
        x2[x2$qtCode==1, "metaData"] <- "value-only reporter [median]"
    }

                                        #clean order-of-magnitude outliers, ignoring consistent UVs  
    cond_MX100 <- x2$UV_M > 0 & x2$UV_X > 0 & x2$UV_X < median.UV_X / 100 & x2$TV_X > 0
    cond_MM100 <- x2$UV_X > 0 & x2$UV_M > 0 & x2$UV_M < median.UV_M / 100 & x2$TV_M > 0
    cond_MX_100 <- x2$UV_M > 0 & x2$UV_X > 0 & x2$UV_X > median.UV_X * 100 & x2$TV_X > 0
    cond_MM_100 <- x2$UV_X > 0 & x2$UV_M > 0 & x2$UV_M > median.UV_M * 100 & x2$TV_M > 0
    cond_EQ <- is.infinite(x2$UV_M / x2$UV_X) & is.infinite(x2$UV_X / x2$UV_M) & 
        (x2$UV_M / x2$UV_X) < 2 & (x2$UV_M / x2$UV_X) > 0.5 

    x2[cond_MX100 & !cond_EQ, "UV_X"] <- x2[cond_MX100 & !cond_EQ, "UV_M"] * .9
    x2[cond_MX100 & !cond_EQ, "NW_X"] <- x2[cond_MX100 & !cond_EQ, "TV_X"] / 
        x2[cond_MX100 & !cond_EQ, "UV_X"] * 1000
    x2[cond_MX100 & !cond_EQ, "metaData"] <- paste("X.mag.err.div100", collapse = "")
    
    x2[cond_MM100 & !cond_EQ, "UV_M"] <- x2[cond_MM100 & !cond_EQ, "UV_X"] * 1.1
    x2[cond_MM100 & !cond_EQ, "NW_M"] <- x2[cond_MM100 & !cond_EQ, "TV_M"] / 
        x2[cond_MM100 & !cond_EQ, "UV_M"] * 1000
    x2[cond_MM100 & !cond_EQ, "metaData"] <- paste("M.mag.err.div100", collapse = "")
    
    x2[cond_MX_100 & !cond_EQ, "UV_X"] <- x2[cond_MX_100 & !cond_EQ, "UV_M"] * .9
    x2[cond_MX_100 & !cond_EQ, "NW_X"] <- x2[cond_MX_100 & !cond_EQ, "TV_X"] /
        x2[cond_MX_100 & !cond_EQ, "UV_X"] * 1000
    x2[cond_MX_100 & !cond_EQ, "metaData"] <- paste("X.mag.err.tim100", collapse = "")
    
    x2[cond_MM_100 & !cond_EQ, "UV_M"] <- x2[cond_MM_100 & !cond_EQ, "UV_X"] * 1.1
    x2[cond_MM_100 & !cond_EQ, "NW_M"] <- x2[cond_MM_100 & !cond_EQ, "TV_M"] / 
        x2[cond_MM_100 & !cond_EQ, "UV_M"] * 1000
    x2[cond_MM_100 & !cond_EQ, "metaData"] <- paste("M.mag.err.tim100", collapse = "")
    
    basicVal <- x2
    
    
    x2eu <- x2[x2$rtCode  %in% euCodes & x2$ptCode %in% euCodes, ]
    
    return(list(basicVal,
                Min.UV_M,
                Min.UV_X,
                Max.UV_M,
                Max.UV_X,
                median.UV_M,
                median.UV_X,
                x2eu)) 

}
