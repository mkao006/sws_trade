## http://comtrade.un.org/ws/getTariffLine.aspx?cc=20097*&y=2001&comp=false
## http://comtrade.un.org/ws/get.aspx?cc=080610&y=2001&comp=false 

##______________________________LOGIC FILTERS_____________________________________________________________________________________________________
## http://unstats.un.org/unsd/tradekb/Knowledgebase/UN-Comtrade-Reference-Tables
## 1: trade.ToCompareData: transorm data into suitable form
## 2: trade.CompleteCompareData: add value for cases flow reported only by one. Metadata codes: TM_X/TM_M (TM: trade mirror)
## 3: validateCompData: validate data for extreme UV cases or extreme UV. Take the most likely value. Metadata codes: impu_outl, impu_rat
## 4: reliability index is in two steps:
##          - step 1: reliab is percentage of total country trade (X or M) that is reported with less than 5% discrepancy
##          - step 2: reliab_trim is same as step 1, removing "worst" trade partner. Worst is taken as: err * 1 /(1- reliability). Sort of weighting error with "unreliability"
##__________________________________________________________________________________________________________________________________________________

library(data.table)
library(faosws)
library(plyr)
library(reshape2)

## Set up for the test environment
tokenFile = "token.txt"
TEST_MODE=FALSE
LOCAL=FALSE
if ((LOCAL==FALSE) && file.exists(tokenFile))
{
	token = readChar(tokenFile,file.info(tokenFile)$size)
	token = sub('[\n\r]','',token)
 	GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws",token)
	TEST_MODE=TRUE
	options(error=utils::recover)
}
print(!TEST_MODE)
if (!TEST_MODE)
{
	consolepath=paste(R_SWS_SHARE_PATH,strsplit(swsContext.username,"/")[[1]][2],sep="/")
	if (!file.exists(consolepath))
	{
		dir.create(consolepath, recursive=TRUE)
	}
	consolefile = file (paste(consolepath,"RConsole_output.txt",sep="/"), open="wt")
	sink (consolefile, append=FALSE, type="output", split=TRUE)
	sink (consolefile, append=FALSE, type="message", split=FALSE)
}

# setwd("C:/Users/prakash/Dropbox/workspace/java/x2fbs/war/ds/UNSD/")
# UNSDlist <- read.csv("C:/Users/prakash/Dropbox/workspace/java/x2fbs/war/UNSDxml.csv",header=FALSE,sep = ",")
#######################################################################################################################
#################################### set constants ####################################################################
VAL_COL_PREFIX <- "Value_measuredElementTrade"
OP_VAL_COL_PREFIX <- "Value_timePointYears"
OP_FLAG_COL_PREFIX <- "flagTrade_timePointYears"

# Data flags
FLAG_IMPUTED <- "E"
# UNSD unit codes
UNIT_CODE_NONE <- 1
UNIT_CODE_M <- 4
UNIT_CODE_HEAD <- 5
UNIT_CODE_LITRE <- 7
UNIT_CODE_KG <- 8
UNIT_CODE_KHEAD <- 9
UNIT_CODE_M3 <- 12
# These are terrible hard coded - should use the API
OUTPUT_ELEMENTS <- c("5600","5621","5900","5921")
ELE_IMPORT <- c(
	"5600",
	"5601",
	"5608",
	"5609",
	"5610",
	"5611",
	"5616"
)
ELE_REIMPORT <- c(
	"5606",
	"5612",
	"5613",
	"5614"
)
ELE_EXPORT <- c(
	"5900",
	"5901",
	"5908",
	"5909",
	"5910",
	"5911",
	"5916"
)
ELE_REEXPORT <- c(
	"5906",
	"5912",
	"5913",
	"5914"
)
ELE_IMPORT_VAL <- "5621"
ELE_IMPORT_UVAL <- "5630"
ELE_EXPORT_VAL <- "5921"
ELE_EXPORT_UVAL <- "5930"
FLOW_EXPORT <- 1
FLOW_IMPORT <- 2
FLOW_REEXPORT <- 3
FLOW_REIMPORT <- 4
TOLERANCE=0.05
LOWER_TOLERANCE=100.0-TOLERANCE
UPPER_TOLERANCE=100.0+TOLERANCE
ELE_MAP <- merge (
	data.frame (
		unsd_flow=c(
			rep(FLOW_EXPORT,length(ELE_EXPORT)+1),
			rep(FLOW_IMPORT,length(ELE_IMPORT)+1),
			rep(FLOW_REEXPORT,length(ELE_REEXPORT)),
			rep(FLOW_REIMPORT,length(ELE_REIMPORT))
		),
		element=c(ELE_EXPORT,ELE_EXPORT_VAL,ELE_IMPORT,ELE_IMPORT_VAL,ELE_REEXPORT,ELE_REIMPORT)
	),
	data.frame(
		unit=c(rep("NO",2), rep("KG",4), rep("L",2), rep("HEAD",4), rep("KHEAD",4)),
		element=c(
			c("5621","5921"),
			c("5600","5606","5900","5907"),
			c("5601","5901"),
			c("5608","5613","5908","5913"),
			c("5609","5614","5909","5914")
	  	)),
by="element")
UNIT_MAP <- data.frame(
	unsd_unit=c(UNIT_CODE_NONE,UNIT_CODE_M,UNIT_CODE_HEAD,UNIT_CODE_LITRE,UNIT_CODE_KG,UNIT_CODE_KHEAD,UNIT_CODE_M3),
	fao_unit=c("NO","M","HEAD","L","KG","KHEAD","M3")
	)
#################################### set booleans #####################################################################
outPRE<-TRUE
outBAS<-TRUE
outREL<-TRUE

outMISSPEC<-TRUE
outXML<-TRUE

####################################### parse params ##################################################################
incEU.INTRA<-FALSE
incEU.INTRA = as.logical(swsContext.computationParams$incEUintra)
if (incEU.INTRA==TRUE)
{
	ext<<-"_eu"
}else{
	ext<<-""
}

euCodes <<- c()
trade.SetEuCodes = function (intYR)
{
	## EU intra trade
	if(intYR<1995)
	{
		euCodes <<- c(528, 56, 251, 276, 381, 442, 150, 208, 372, 826, 300, 620, 724)
	} else if(intYR >=1995 & intYR<2005)
	{
		euCodes <<- c(528,56,251,276,381,442,150,208,372,826,300,620,724,40,246,752)
	} else if(intYR>=2005 & intYR<2007)
	{
		euCodes <<- c(528,56, 251, 276, 381, 442, 150, 208, 372, 826, 300, 620, 724, 40, 246, 752, 196, 203, 63, 348, 428, 440, 470, 616, 703, 198)
	} else if(intYR>=2007)
	{
		euCodes <<- c(528,56, 251, 276, 381, 442, 150, 208, 372, 826, 300, 620, 724, 40, 246, 752, 196, 203, 63, 348, 428, 440, 470, 616, 703, 198, 100, 642)
	}
}

#################################### function ###############################
# trade.Transform2Adam() Transform SWS denormalized data into the data.frame
#	layout expected by Adam's processing:
# 		column names = (yr, rgCode, rtCode, ptCode, cmdCode, qtCode, TradeQuantity, NetWeight, TradeValue)
# 		column meanings = (yr, flow, reporter, partner, commod, unit, quantity, weight, value)
# 	Note that columns pfCode, estCode and htCode from the raw CT data are ommitted
#	because the code does not appear to use them.
# @param data The denormalized SWS raw comtrade TF data
# @return a data.frame containing the data
#############################################################################
trade.Transform2Adam = function (data)
{
	dataOut <- NULL
	i = 1

	if (nrow(data) > 0)
	{
		year <- data[1]$timePointYears
		rgCode <- list()
		rtCode <- list()
		ptCode <- list()
		cmdCode <- list()
		qtCode <- list()
		origQty <- list()
		stdQty <- list()
		value <- list()

		for (r in 1:nrow(data))
		{
			impStdQty <- NA
			impOrigQty <- NA
			expStdQty <- NA
			expOrigQty <- NA

			for (v in 5:ncol(data))
			{
				ele <- sub(paste("^",VAL_COL_PREFIX,"_",sep=""),"",colnames(data)[v])
				ele_info <- ELE_MAP[ELE_MAP$element==ele,]
				if (nrow(ele_info) == 0) next

				if (ele == ELE_IMPORT_VAL)
				{
					impValue <- data[r,v,with=FALSE]
				} else if (ele == ELE_EXPORT_VAL)
				{
					expValue <- data[r,v,with=FALSE]
				} else if (ele %in% ELE_IMPORT)
				{
					if (ele_info$unit == "KG")
					{
						impStdQty <- data[r,v,with=FALSE]
						impQtCodeS <- as.numeric(UNIT_MAP[UNIT_MAP$fao_unit==as.character(ele_info$unit),]$unsd_unit)
					} else
					{
						impOrigQty <- data[r,v,with=FALSE]
						impQtCodeO <- as.numeric(UNIT_MAP[UNIT_MAP$fao_unit==as.character(ele_info$unit),]$unsd_unit)
					}
				} else if (ele %in% ELE_EXPORT)
				{
					if (ele_info$unit == "KG")
					{
						expStdQty <- data[r,v,with=FALSE]
						expQtCodeS <- as.numeric(UNIT_MAP[UNIT_MAP$fao_unit==as.character(ele_info$unit),]$unsd_unit)
					} else
					{
						expOrigQty <- data[r,v,with=FALSE]
						expQtCodeO <- as.numeric(UNIT_MAP[UNIT_MAP$fao_unit==as.character(ele_info$unit),]$unsd_unit)
					}
				}
			}

			if (!is.na(impStdQty) || !is.na(impOrigQty))
			{
#	column names = (yr, rgCode, rtCode, ptCode, cmdCode, qtCode)
				rgCode[i] <- FLOW_IMPORT
				rtCode[i] <- data[r]$reportingCountryM49
				ptCode[i] <- data[r]$partnerCountryM49
				cmdCode[i] <- data[r]$measuredItemHS
				origQty[i] <- impOrigQty
				stdQty[i] <- impStdQty
				value[i] <- impValue
				if (!is.na(origQty[i]))
				{
					qtCode[i] <- impQtCodeO
				} else
				{
					qtCode[i] <- impQtCodeS
				}
				i <- i + 1
			}
			if (!is.na(expStdQty) || !is.na(expOrigQty))
			{
#	column names = (yr, rgCode, rtCode, ptCode, cmdCode, qtCode)
				rgCode[i] <- FLOW_EXPORT
				rtCode[i] <- data[r]$reportingCountryM49
				ptCode[i] <- data[r]$partnerCountryM49
				cmdCode[i] <- data[r]$measuredItemHS
				origQty[i] <- expOrigQty
				stdQty[i] <- expStdQty
				value[i] <- expValue
				if (!is.na(origQty[i]))
				{
					qtCode[i] <- expQtCodeO
				} else
				{
					qtCode[i] <- expQtCodeS
				}
				i <- i + 1
			}
		}
	}

	if (i > 1)
	{
		dataOut <- data.frame(
			yr=rep(year,i-1),
			rgCode=as.numeric(rgCode),
			rtCode=as.character(rtCode),
			ptCode=as.character(ptCode),
			cmdCode=as.character(cmdCode),
			qtCode=as.numeric(qtCode),
			TradeQuantity=as.numeric(origQty),
			NetWeight=as.numeric(stdQty),
			TradeValue=as.numeric(value))

# remove the factors
		dataOut$yr <- as.character(dataOut$yr)
		dataOut$rtCode <- as.character(dataOut$rtCode)
		dataOut$ptCode <- as.character(dataOut$ptCode)
		dataOut$cmdCode <- as.character(dataOut$cmdCode)
	}

	dataOut
}

#################################### function ###############################
# trade.SaveCompletedTF() Save trade flow data  after cleaning, mirroring
# and balancing to the database
# @param com The commodity ID (HS)
# @param yr The year
# @param data The data to save
#############################################################################
trade.SaveCompletedTF = function (com, yr, data)
{
	nr = nrow(data)
	
	if (nr > 0)
	{
		toSave = NULL
		item_col = "measuredItemHS"
		ele_col_prefix = "Value_measuredElementTrade"
		flag_col_prefix = "flagTrade_measuredElementTrade"

#		"flagTrade_measuredElementTrade_5911 Value_measuredElementTrade_5912"
# poke the item column into the table
		data[item_col] = rep(com,nr)
		data["TV_MF"] = rep(FLAG_IMPUTED,nr)
		data["TV_XF"] = rep(FLAG_IMPUTED,nr)
		data["NW_MF"] = rep(FLAG_IMPUTED,nr)
		data["NW_XF"] = rep(FLAG_IMPUTED,nr)
		data["UV_MF"] = rep(FLAG_IMPUTED,nr)
		data["UV_XF"] = rep(FLAG_IMPUTED,nr)

# have to do this for each unit, as the element codes are unit dependent
		for (u in 1:nrow(UNIT_MAP))
		{
			qtCode = UNIT_MAP[u,"unsd_unit"]
			if (nrow(data[data$qtCode==qtCode,]) > 0)
			{
				fao_unit = as.character(UNIT_MAP[u,"fao_unit"])
				impq_ele = as.character(ELE_MAP[ELE_MAP$unsd_flow==FLOW_IMPORT & ELE_MAP$unit==fao_unit,"element"])
				expq_ele = as.character(ELE_MAP[ELE_MAP$unsd_flow==FLOW_EXPORT & ELE_MAP$unit==fao_unit,"element"])
				colnames = c("reportingCountryM49","partnerCountryM49",item_col,"timePointYears",
					paste(ele_col_prefix,ELE_IMPORT_VAL,sep="_"),
					paste(flag_col_prefix,ELE_IMPORT_VAL,sep="_"),
					paste(ele_col_prefix,ELE_EXPORT_VAL,sep="_"),
					paste(flag_col_prefix,ELE_EXPORT_VAL,sep="_"),
					paste(ele_col_prefix,impq_ele,sep="_"),
					paste(flag_col_prefix,impq_ele,sep="_"),
					paste(ele_col_prefix,expq_ele,sep="_"),
					paste(flag_col_prefix,expq_ele,sep="_"),
#					paste(ele_col_prefix,ELE_IMPORT_UVAL,sep="_"),
#					paste(flag_col_prefix,ELE_IMPORT_UVAL,sep="_"),
#					paste(ele_col_prefix,ELE_EXPORT_UVAL,sep="_"),
#					paste(flag_col_prefix,ELE_EXPORT_UVAL,sep="_"),
					"Comment")
				toSave = data.table(data[data$qtCode==qtCode,c("rtCode","ptCode",item_col,"Year",
					"TV_M","TV_MF","TV_X","TV_XF",
					"NW_M","NW_MF","NW_X","NW_XF",
#					"UV_M","UV_MF","UV_X","UV_XF",
					"metaData")])
				setnames (toSave,colnames(toSave),colnames)

				SaveData ("trade", "completed_tf", data=toSave, normalized=FALSE);
			}
		}
	}
}

#################################### function ###############################
# trade.SaveTotals() Save total trade data to the database
# @param com The commodity ID (HS)
# @param yr The year
# @param data The data to save
#############################################################################
trade.SaveTotals = function (com, yr, data)
{
	nr = nrow(data)
	
	if (nr > 0)
	{
		area <- list()
		elem <- list()
		VQ <- list()

		ind <- array(dim=4)
		eles <- c("5600","5621","5900","5921")
		prefs <- c("MQ","MV","XQ","XV")

		names <- colnames(data)
		for (cn in 1:length(names))
		{
			i = which (prefs == substr(names[cn],1,2))
			if (length(i) > 0)
			{
				ind[i] <- cn
			}
		}

		k = 1
		for (i in 1:nr)
		{
			for (j in 1:4)
			{
				area[k] <- data[i,]$rtCode
				elem[k] <- eles[j]
				VQ[k] <- data[i,ind[j]]
				k <- k + 1
			}
		}

#		example <- GetData (swsContext.datasets[[2]],flags=TRUE)
		nr = length(area)
		toSave <- data.table(
			geographicAreaM49=as.character(area),
			measuredElementTrade=as.character(elem),
			measuredItemHS=rep(com,nr),
			timePointYears=rep(as.character(yr),nr),
			Value=as.numeric(VQ),
			flagTrade=rep(FLAG_IMPUTED,nr))
#		oldnames = colnames(toSave)
#		newnames = c("geographicAreaM49","measuredItemHS","measuredElementTrade",
#			paste(OP_VAL_COL_PREFIX,as.character(yr),sep="_"),
#			paste(OP_FLAG_COL_PREFIX,as.character(yr),sep="_"))
#		setnames (toSave,oldnames,newnames)

		SaveData ("trade", "total_trade", data=toSave, normalized=TRUE);
	}
}

#################################### function ###############################
# trade.GetData() Fetch data for one year and commodity
#     with the element axis denormalized
# @param com The commodity ID (HS)
# @param yr The year
# @return a data.table containing the data
#############################################################################
trade.GetData = function (com, yr)
{
## Pivot to vectorize trade elements
	newPivot = c(
		Pivoting(code = "timePointYears", ascending = FALSE),
		Pivoting(code= "reportingCountryM49", ascending = TRUE),
		Pivoting(code= "partnerCountryM49", ascending = TRUE),
		Pivoting(code= "measuredItemHS", ascending = TRUE),
		Pivoting(code= "measuredElementTrade", ascending = TRUE)
		)

# get data just for one year and commodity
	key = swsContext.datasets[[1]]@dimensions
	for (k in 1:length(key))
	{
		if (key[[k]]@name == "timePointYears")
		{
			key[[k]]@keys = c(as.character(yr))
		} else if (key[[k]]@name == "measuredItemHS")
		{
			key[[k]]@keys = c(com)
		}
	}
	swsContext.datasets[[1]]@dimensions = key

## Query the data
	data = GetData(
		key = swsContext.datasets[[1]],
		flags = TRUE,
		normalized = FALSE,
		pivoting = newPivot
	)

    trade.Transform2Adam (data)
}

purgeList = c(
	"complete_compVal","x","trade.AddReliability","MINq","nonRep","nonRepExp","nonRepExpList","nonRepImp","nonRepImpList","ratio_cons", 
	"reMX","reMXcodes","x_cons","allDup","trade.AllDuplicated","commYR","complete_compValRel","trade.CompleteCompareData","cond_NW5", 
	"cond_NW7","cond_NW8","trade.CountUnidirectional","data","df","dup_above","dup_below","euCodes","intHS","intYR","ix","j", 
	"LogFile","M","Max.UV_M","Max.UV_X","mean.UV_M","mean.UV_X","median.UV_M","median.UV_X","Min.UV_M","Min.UV_X", 
	"missNW","MQ","MV","parts","rawDF","rawDF_raw","trade.ToCompareData","unSpecCode","unSpecDF","whole","X","xml", 
	"xmlUNSD","XQ","XV"
)

#############################################
## Small convenenience function
#############################################
trade.AllDuplicated <- function(x)
{
  dup_below <- duplicated(x)
  dup_above <- duplicated(x, fromLast=TRUE)
  allDup <- apply(cbind(dup_above, dup_below), 1, any)
  allDup
}

#############################################
## Check uni-directional flows. 
#############################################
trade.CountUnidirectional <- function(rawData, groups)
{
#NAC	n <- nrow(rawDF)
	n <- nrow(rawData)

## rotate data: invert exports, so that exp B - A becomes comparable to imp A - B
  rawDF2 <- rawData
  rawDF2[rawData$rgCode==2,"ptCode"] <- rawData[rawData$rgCode==2,"rtCode"]
  rawDF2[rawData$rgCode==2,"rtCode"] <- rawData[rawData$rgCode==2,"ptCode"]


  
## compute for whole data frame
  gen_res <- 100*(n-sum(trade.AllDuplicated(rawDF2[,c("rtCode", "ptCode")])))/n
  gen <- data.frame(cmdCode="all", perc_unmatched=gen_res)
  
## compute either for whole groups, or specified ones (arg groups)
  if(missing(groups)) {
	res <- ddply(rawDF2, .(cmdCode), function(x) a=100*(nrow(x)-sum(trade.AllDuplicated(x[,c("rtCode", "ptCode")])))/nrow(x))
	res[,1] <- as.character(res[,1])
	colnames(res) <- c("cmdCode", "perc_unmatched")
  } else {
	ng <- length(groups)
	res <- data.frame(cmdCode=rep(NA, ng), perc_unmatched=rep(NA, ng))
	for(i in 1:ng){
	  codes_i <- grep(groups[i],unique(rawDF2$cmdCode), value=TRUE)
	  sub_i <- subset(rawDF2, cmdCode%in%codes_i, c("rtCode", "ptCode"))
	  n_i <- nrow(sub_i)
	  res[i,] <- c(groups[i], 100*(n_i-sum(trade.AllDuplicated(sub_i)))/n_i)
	}
	res[,2] <- as.numeric(res[,2])
  }

## merge
  res <- rbind(gen, res)
  res$perc_unmatched <- paste(round(res$perc_unmatched,1),"%", sep="")
  return(res)
}

#############################################
## trade.ToCompareData: put df into 2 cols, M and X
############################################
trade.ToCompareData  <- function(data, HScode)
{

  if(!missing(HScode)) data <- subset(data, cmdCode%in%grep(HScode,unique(data$cmdCode), value=TRUE))
  partDAT <- subset(data, rgCode%in%1:2 & qtCode%in%1:8)
  
## melt data
  meltDAT <- melt(partDAT, id.vars=c("rtCode", "ptCode", "rgCode", "qtCode", "yr", "metaData"), measure.vars=c("NetWeight","TradeValue")) ######QTC
  meltDAT2<-meltDAT
  
	 

## substitute rt and pt code for exporters: (pt now denotes exporters)
  meltDAT2[meltDAT2$rgCode==FLOW_IMPORT,"ptCode"] <- meltDAT[meltDAT$rgCode==FLOW_IMPORT,"rtCode"]
  meltDAT2[meltDAT2$rgCode==FLOW_IMPORT,"rtCode"] <- meltDAT[meltDAT$rgCode==FLOW_IMPORT,"ptCode"]

## Cast data, one col for M, one for X (need to sum as multiple cmdCodes per dyad)
  castDAT=dcast(meltDAT2, yr+ rtCode +ptCode + qtCode + metaData   ~ rgCode+variable,  fun.aggregate=sum)


## Check for special case when sparse data indicating only one direction
  if(length(unique(meltDAT2$rgCode))==1)
  {
	if(unique(meltDAT2$rgCode)==FLOW_IMPORT) # imports only
	{
	  castDAT <- cbind(castDAT, NA, NA)
	} else if(unique(meltDAT2$rgCode)==FLOW_EXPORT)	# exports only
	{
	  castDAT <- cbind(castDAT[,1:3], NA, NA, castDAT[,4:5])
	}
  }

## Formatoutput
  colnames(castDAT) <- c("Year", "rtCode", "ptCode", "qtCode", "metaData", "NW_M", "TV_M", "NW_X","TV_X")
  castDAT <- castDAT[,c("Year", "rtCode", "ptCode", "qtCode","metaData", "NW_M", "TV_M", "NW_X","TV_X")]

## Calculate UVs
castDAT$UV_M = castDAT$TV_M/castDAT$NW_M * 1000;
castDAT$UV_X = castDAT$TV_X/castDAT$NW_X * 1000;

if(incEU.INTRA==FALSE)
{
	castDAT<-castDAT[!(castDAT$rtCode  %in% euCodes==TRUE & castDAT$ptCode %in% euCodes==TRUE),]
} 
	 
castDATx<-castDAT

#NAC	if(outPRE==TRUE){
#NAC	write.csv(castDATx,file=paste(paste(commYR,"raw",sep=""),".csv", sep=ext), row.names=FALSE)
#NAC	}

  return(castDAT)
 
}

#############################################
## trade.CompleteCompareData 
#############################################
trade.CompleteCompareData <- function(x, UVratio_tol=2, file="")
{
  
	if(!all(colnames(x)%in% c("Year", "rtCode", "ptCode", "qtCode","TV_M", "TV_X", "NW_M", "NW_X", "UV_M", "UV_X", "metaData"))) 
		stop("Input x not as expected")
 
  
## find consistent data and central tendencies
	x_tol <- (sum(x$NW_X,na.rm=TRUE) + sum(x$NW_M, na.rm=TRUE))/nrow(x)*.05
	x_cons <<-subset(x, (UV_M/UV_X)<2 & (UV_X/UV_M)>0.5 & NW_X>x_tol )
	ratio_cons<<-sum(x_cons$NW_X,na.rm=TRUE)/sum(x$NW_X,na.rm=TRUE)

	cat ("Total exports: ", sum(x$NW_X,na.rm=TRUE), "\n",sep="", file=file, append=TRUE) 
	cat ("Total imports: ", sum(x$NW_M,na.rm=TRUE), "\n",sep="", file=file, append=TRUE) 
	cat ("ratio of quasi consistent export volume: ", ratio_cons, "\n",sep="", file=file, append=TRUE) 

	MINq <<- median(x$NW_M, na.rm=TRUE)/10
  
	if(!is.na(ratio_cons) & ratio_cons>0.09)
	{ 
		Min.UV_X<<-min(x_cons[x_cons$NW_X> MINq,]$UV_X,na.rm=TRUE)
		Max.UV_X<<-max(x_cons[x_cons$NW_X> MINq,]$UV_X,na.rm=TRUE)
		Min.UV_M<<-min(x_cons[x_cons$NW_M> MINq,]$UV_M,na.rm=TRUE)
		Max.UV_M<<-max(x_cons[x_cons$NW_M> MINq,]$UV_M,na.rm=TRUE)

		cat ("Min.UV_X: ", Min.UV_X, "\n",sep="", file=file, append=TRUE) 
		cat ("Min.UV_M: ", Min.UV_M, "\n",sep="", file=file, append=TRUE) 
		cat ("Max.UV_X: ", Max.UV_X, "\n",sep="", file=file, append=TRUE) 
		cat ("Max.UV_M: ", Max.UV_M, "\n",sep="", file=file, append=TRUE)

		cat ("ratio of quasi consistent transactions: ", nrow(x_cons)/nrow(x), "\n",sep="", file=file, append=TRUE)
		cat ("ratio of quasi consistent transactions: ", nrow(x_cons)/nrow(x), "\n")  



		if(any(x_cons$UV_M>0 & !is.na(x_cons$UV_M))){
		mean.UV_M<<-sum(x_cons$TV_M,na.rm=TRUE)/sum(x_cons$NW_M,na.rm=TRUE)*1000
		median.UV_M<<-median(x_cons$UV_M,na.rm=TRUE)
		cat ("mean.UV_M= ", mean.UV_M, "\n",sep="", file=file, append=TRUE) 
		cat ("median.UV_M= ", median.UV_M, "\n",sep="", file=file, append=TRUE)

		}
		if(any(x_cons$UV_X>0 & !is.na(x_cons$UV_X))){
		mean.UV_X<<-sum(x_cons$TV_X,na.rm=TRUE)/sum(x_cons$NW_X,na.rm=TRUE)*1000
		median.UV_X<<-median(x_cons$UV_X,na.rm=TRUE)
		cat ("mean.UV_X= ", mean.UV_X, "\n",sep="", file=file, append=TRUE) 
		cat ("median.UV_X= ", median.UV_X, "\n",sep="", file=file, append=TRUE)

		}

	} else
	{
		x<<-x

		x$UV_M[!is.finite(x$UV_M)] <- NA
		x$UV_X[!is.finite(x$UV_X)] <- NA


		Min.UV_X<<-mean(x[x$NW_X> MINq,]$UV_X,na.rm=TRUE) * 0.25
		Max.UV_X<<-mean(x[x$NW_X> MINq,]$UV_X,na.rm=TRUE) * 4
		Min.UV_M<<-mean(x[x$NW_M> MINq,]$UV_M,na.rm=TRUE) * 0.25
		Max.UV_M<<-mean(x[x$NW_M> MINq,]$UV_M,na.rm=TRUE) * 4

		if(any(x$UV_M>0 & !is.na(x$UV_M))){
		mean.UV_M<<-sum(x[x$NW_M> MINq,]$TV_M,na.rm=TRUE)/sum(x[x$NW_M> MINq,]$NW_M,na.rm=TRUE)*1000
		median.UV_M<<-median(x[x$NW_M> MINq,]$UV_M,na.rm=TRUE)
		cat ("mean.UV_M= ", mean.UV_M, "\n",sep="", file=file, append=TRUE) 
		cat ("median.UV_M= ", median.UV_M, "\n",sep="", file=file, append=TRUE)
		} else {}
		if(any(x$UV_X>0 & !is.na(x$UV_X))){
		mean.UV_X<<-sum(x[x$NW_X> MINq,]$TV_X,na.rm=TRUE)/sum(x[x$NW_X> MINq,]$NW_X,na.rm=TRUE)*1000
		median.UV_X<<-median(x[x$NW_X> MINq,]$UV_X,na.rm=TRUE)
		cat ("mean.UV_X= ", mean.UV_X, "\n",sep="", file=file, append=TRUE) 
		cat ("median.UV_X= ", median.UV_X, "\n",sep="", file=file, append=TRUE) 
		} else { 
		mean.UV_X<<-mean.UV_M*.9
		Min.UV_X<<-Min.UV_M*.9
		Max.UV_X<<-Max.UV_M*.9
		median.UV_X<<-median.UV_M*.9
		}
		if(is.na(mean.UV_X)){
		mean.UV_X<<-mean.UV_M*.9
		Min.UV_X<<-Min.UV_M*.9
		Max.UV_X<<-Max.UV_M*.9
		}
		if(is.na(median.UV_X)){
		median.UV_X<<-median.UV_M*.9
		}
	}

## change NA inf into zeros

  to0 <- sapply(x[,c("TV_M", "TV_X","NW_M", "NW_X", "UV_M", "UV_X")],  function(x) ifelse(is.na(x), 0, x))
  if(is.numeric(to0) & is.null(dim(to0))) to0 <- as.data.frame(matrix(to0, nrow=1, dimnames=list("a", names(to0))))
  x2 <- cbind(x[,c("Year", "rtCode", "ptCode", "qtCode")],to0 , x[,"metaData", drop=FALSE]) #"TV_M", "TV_X" 
  is.na(x2) <- do.call(cbind,lapply(x2, is.infinite))

   x2[is.na(x2)]<-0

 x2[x2$rtCode %in% nonRepImpList, "NW_M"]<-x2[x2$rtCode %in% nonRepImpList, "NW_X"]
 x2[x2$rtCode %in% nonRepImpList, "UV_M"]<-x2[x2$rtCode %in% nonRepImpList, "UV_X"] * 1.1 #(median.UV_M/median.UV_X)
 x2[x2$rtCode %in% nonRepImpList, "TV_M"]<-x2[x2$rtCode %in% nonRepImpList, "NW_X"] * 1.1 # (median.UV_M/1000)
 x2[x2$rtCode %in% nonRepImpList, "metaData"]<-"mirrored import for non-reporter"
 
 x2[x2$ptCode %in% nonRepExpList, "NW_X"]<-x2[x2$ptCode %in% nonRepExpList, "NW_M"]
 x2[x2$ptCode %in% nonRepExpList, "UV_X"]<-x2[x2$ptCode %in% nonRepExpList, "UV_M"] * 0.9 # (median.UV_X/median.UV_M)
 x2[x2$ptCode %in% nonRepExpList, "TV_X"]<-x2[x2$ptCode %in% nonRepExpList, "TV_M"] * 0.9 # (median.UV_X/1000)
 x2[x2$ptCode %in% nonRepExpList, "metaData"]<-"mirrored export for non-reporter"
 
 
## deal with NW =0 & TRADE VALUE>0, i.e. qtCode=1
  if(any(x2$qtCode==1)){
	x2[x2$qtCode==1,"NW_M"] <- x2[x2$qtCode==1, "TV_M"] /median.UV_M * 1000
	x2[x2$qtCode==1,"NW_X"] <- x2[x2$qtCode==1, "TV_X"] /median.UV_X * 1000
	x2[x2$qtCode==1,"UV_M"] <- median.UV_M
	x2[x2$qtCode==1,"UV_X"] <- median.UV_X
	x2[x2$qtCode==1,"metaData"] <- "value-only reporter [median] "
  }

# ## Clean order-of-magnitude outliers, ignoring consistent UVs  
   cond_MX100 <- x2$UV_X < mean.UV_X/50 &  x2$UV_X > mean.UV_X/500
   cond_MM100 <- x2$UV_M < mean.UV_M/50 &  x2$UV_M > mean.UV_M/500
   cond_MX_100<- x2$UV_X > mean.UV_X*50 &  x2$UV_X < mean.UV_X*500
   cond_MM_100<- x2$UV_M > mean.UV_M*50 &  x2$UV_M < mean.UV_M*500 
   cond_EQ<- (x2$UV_M/x2$UV_X)<2 & (x2$UV_M/x2$UV_X)>0.5 

   x2[cond_MX100 & !cond_EQ, "NW_X"] <- x2[cond_MX100 & !cond_EQ, "NW_X"]/10
   x2[cond_MX100 & !cond_EQ, "metaData"] <- paste("X100", collapse = "")
   x2[cond_MM100 & !cond_EQ, "NW_M"] <- x2[cond_MM100 & !cond_EQ, "NW_M"]/10
   x2[cond_MM100 & !cond_EQ, "metaData"] <- paste("M100", collapse = "")
   x2[cond_MX_100 & !cond_EQ, "NW_X"] <- x2[cond_MX_100 & !cond_EQ, "NW_X"]*10
   x2[cond_MX_100 & !cond_EQ, "metaData"] <- paste("X_100", collapse = "")
   x2[cond_MM_100 & !cond_EQ, "NW_M"] <- x2[cond_MM_100 & !cond_EQ, "NW_M"]*10
   x2[cond_MM_100 & !cond_EQ, "metaData"] <- paste("M_100", collapse = "")
   

#NAC	if(outBAS==TRUE){
#NAC	write.csv(x2,file=paste(paste(commYR,"basicVal",sep=""),".csv", sep=ext), row.names=FALSE) 
#NAC	}

## clean nonsense weights based on nonsense UVs using cons flows max min
nonse_NW_M <-x2$UV_M>Max.UV_M | x2$UV_M<Min.UV_M
nonse_NW_X <-x2$UV_X>Max.UV_X | x2$UV_X<Min.UV_X
x2[nonse_NW_M,"NW_M"]<-x2[nonse_NW_M,"TV_M"]/(median.UV_M/1000)
x2[nonse_NW_X,"NW_X"]<-x2[nonse_NW_X,"TV_X"]/(median.UV_X/1000)
x2[nonse_NW_M,"UV_M"]<-median.UV_M
x2[nonse_NW_X,"UV_X"]<-median.UV_X
x2[nonse_NW_M,"metaData"]<-"nonse NW_M"
x2[nonse_NW_X,"metaData"]<-"nonse NW_X"

 
## change zeros into counterpart, if exists:
  cond_NW_M <- x2$NW_M==0 & x2$NW_X>0
  x2[cond_NW_M,"NW_M"] <- x2[cond_NW_M,"NW_X"]
  x2[cond_NW_M,"UV_M"] <- median.UV_M
  x2[cond_NW_M,"TV_M"] <- x2[cond_NW_M,"NW_X"]*median.UV_M/1000
  x2[cond_NW_M,"metaData"] <- paste("TM_Xmedian", collapse = "")
  cond_NW_X <- x2$NW_X==0 & x2$NW_M>0
  x2[cond_NW_X,"NW_X"] <- x2[cond_NW_X,"NW_M"]
  x2[cond_NW_X,"UV_X"] <- median.UV_X
  x2[cond_NW_X,"TV_X"] <- x2[cond_NW_X,"NW_M"]*median.UV_X/1000
  x2[cond_NW_X,"metaData"] <- paste("TX_Mmedian", collapse = "")
  
## Fill meta for values not changed:
  x2[x2$metaData=="", "metaData"] <- paste("noChange", collapse = "")
## print some infos:
  n_tmm <- sum(cond_NW_M)
  n_tmx <- sum(cond_NW_X)
  n_both <- nrow(subset(x2, metaData== "noChange"))
  n<- nrow(x2)/100

  # cat("Number of dyads:\n", file=file)
  cat("\t-assigned exports to imports:\t\t", round(n_tmx/n), "%\n", file=file, append=TRUE)
  cat("\t-assigned imports to exports:\t\t", round(n_tmm/n), "%\n", file=file, append=TRUE)
  cat("\t-contain both imports and exports:\t", round(n_both/n), "%\n", file=file, append=TRUE)

#____________________________________________________________________________________________________    
  x_noch<-subset(x2, metaData=="noChange")
  cat("Remaining data to align: ", round(100*nrow(x_noch)/nrow(x2), 2), "%\n", file=file, append=TRUE)


x_both <- x2#subset(x2, metaData=="noChange")


## impute NW when too big UV min/max of consistent TFs:
if(nrow(x_cons)/nrow(x2)>0.2){
  x_bigUV_M <- subset(x_both, UV_M-Max.UV_M> 0)
  x_smallUV_M <- subset(x_both, UV_M-Min.UV_M<= 0)
  if(nrow(x_bigUV_M)>0){
	x2[rownames(x_bigUV_M),"NW_M"] <- x2[rownames(x_bigUV_M),"NW_X"]
	x2[rownames(x_bigUV_M),"TV_M"] <- x2[rownames(x_bigUV_M),"NW_X"] * median.UV_M/1000
	x2[rownames(x_bigUV_M),"metaData"] <- paste("impu_outl_bigM", x2[rownames(x_bigUV_M),"metaData"], sep=" ")
  }
  if(nrow(x_smallUV_M)>0){
	x2[rownames(x_smallUV_M),"NW_M"] <- x2[rownames(x_smallUV_M),"NW_X"]
	x2[rownames(x_smallUV_M),"TV_M"] <- x2[rownames(x_smallUV_M),"NW_X"] * median.UV_M/1000
	x2[rownames(x_smallUV_M),"metaData"] <- paste("impu_outl_smallM",x2[rownames(x_smallUV_M),"metaData"],sep=" ")
  }
	x_bigUV_X <- subset(x_both, UV_X-Max.UV_X> 0)
	x_smallUV_X <- subset(x_both, UV_X-Min.UV_X<= 0)
  if(nrow(x_bigUV_X)>0){
	x2[rownames(x_bigUV_X),"NW_X"] <- x2[rownames(x_bigUV_X),"NW_M"]
	x2[rownames(x_bigUV_X),"TV_X"] <- x2[rownames(x_bigUV_X),"NW_M"] * median.UV_X/1000
	x2[rownames(x_bigUV_X),"metaData"] <- paste("impu_outl_bigX",x2[rownames(x_bigUV_X),"metaData"], sep= " ")
  }
  if(nrow(x_smallUV_X)>0){
	x2[rownames(x_smallUV_X),"NW_X"] <- x2[rownames(x_smallUV_X),"NW_M"]
	x2[rownames(x_smallUV_X),"TV_X"] <- x2[rownames(x_smallUV_X),"NW_M"] * median.UV_M/1000
	x2[rownames(x_smallUV_X),"metaData"] <- paste("impu_outl_smallX", x2[rownames(x_smallUV_X),"metaData"], sep = " ")
  }
 } else {
   x_bigUV_M <- subset(x_both, UV_M/median.UV_M> 3)
   x_smallUV_M <- subset(x_both, UV_M/median.UV_M<1/3)
  if(nrow(x_bigUV_M)>0){
	x2[rownames(x_bigUV_M),"NW_M"] <- x2[rownames(x_bigUV_M),"NW_X"]
	x2[rownames(x_bigUV_M),"TV_M"] <- x2[rownames(x_bigUV_M),"NW_X"] * median.UV_M/1000
	x2[rownames(x_bigUV_M),"metaData"] <- paste("impu_outl_bigM", x2[rownames(x_bigUV_M),"metaData"], sep = " ")
  }
  if(nrow(x_smallUV_M)>0){
	x2[rownames(x_smallUV_M),"NW_M"] <- x2[rownames(x_smallUV_M),"NW_X"]
	x2[rownames(x_smallUV_M),"TV_M"] <- x2[rownames(x_smallUV_M),"NW_X"] * median.UV_M/1000
	x2[rownames(x_smallUV_M),"metaData"] <- paste("impu_outl_smallM", x2[rownames(x_smallUV_M),"metaData"], sep = " ")
  }
   x_bigUV_X <- subset(x_both, UV_X/median.UV_X> 3)
   x_smallUV_X <- subset(x_both, UV_X/median.UV_X<1/3)
  if(nrow(x_bigUV_X)>0){
	x2[rownames(x_bigUV_X),"NW_X"] <- x2[rownames(x_bigUV_X),"NW_M"]
	x2[rownames(x_bigUV_X),"TV_X"] <- x2[rownames(x_bigUV_X),"NW_M"] * median.UV_X/1000
	x2[rownames(x_bigUV_X),"metaData"] <- paste("impu_outl_bigX", x2[rownames(x_bigUV_X),"metaData"], sep = " ")
  }
  if(nrow(x_smallUV_X)>0){
	x2[rownames(x_smallUV_X),"NW_X"] <- x2[rownames(x_smallUV_X),"NW_M"]
	x2[rownames(x_smallUV_X),"TV_X"] <- x2[rownames(x_smallUV_X),"NW_M"] * median.UV_X/1000
	x2[rownames(x_smallUV_X),"metaData"] <- paste("impu_outl_smallX", x2[rownames(x_smallUV_X),"metaData"], sep = " ")
  }
 }
  
  complete_compVal<<-x2
  return(x2)  
}

##################################################
########## trade.AddReliability
##################################################
trade.AddReliability <- function(x, errTol=5, file="")
{

	## select "nochange values" or nonse
#NAC	x<<-x
	z<- x[which(!x$NW_M-x$NW_X==0),]
	if(nrow(z)>0)
	{
		z[(z$NW_M/z$NW_X)> LOWER_TOLERANCE & (z$NW_M/z$NW_X)< UPPER_TOLERANCE | (z$NW_X/z$NW_M)> LOWER_TOLERANCE & (z$NW_X/z$NW_M)< UPPER_TOLERANCE,"metaData"] <- "within tolerance"
		x[rownames(z),] <- z[rownames(z),]
	}

	  x_noch  <-subset(x,  metaData=="noChange" | metaData=="nonse NW_X" | metaData=="nonse NW_M")
	  if(nrow(x_noch)==0)
	  {
		warning("No values tagged 'noChange' in this data, so 'addreliability' operation not performed")
	  } else
	  {

	  ## build error metric: 
		x_noch$errorNW <- 100*abs((x_noch$NW_M - x_noch$NW_X)/x_noch$NW_X)

	  ## build simple index: perc(err <param errTol) by country
		M_index <- ddply(x_noch, .(rtCode), function(x) with(x, sum(NW_M[errorNW<errTol], na.rm=TRUE)/sum(NW_M, na.rm=TRUE)))
		X_index <- ddply(x_noch, .(ptCode), function(x) with(x, sum(NW_X[errorNW<errTol], na.rm=TRUE)/sum(NW_X, na.rm=TRUE)))

	  ## assemble indices:
		indices <- merge(M_index, X_index,by.x="rtCode", by.y="ptCode", all=TRUE)

	  ## build "worst partner" index index:
		x_noch_relab1 <- ddply(x_noch, .(rtCode), function(x) data.frame(x, rt_relab=merge(x, M_index, all.x=TRUE)$V1))
		x_noch_relab <- ddply(x_noch_relab1, .(rtCode), function(x) data.frame(x, pt_relab=merge(x, X_index, all.x=TRUE)$V1))

		M_index_trim <- ddply(x_noch_relab, .(rtCode), function(x) 
		with(x[ifelse(nrow(x)>1, -which.max(x$errorNW*(1-x$pt_relab)),1),], sum(NW_M[errorNW<errTol], na.rm=TRUE)/sum(NW_M, na.rm=TRUE)))

		X_index_trim <- ddply(x_noch_relab, .(ptCode), function(x) 
		with(x[ifelse(nrow(x)>1, -which.max(x$errorNW*(1-x$rt_relab)),1),], sum(NW_X[errorNW<errTol], na.rm=TRUE)/sum(NW_X, na.rm=TRUE)))

		#x_noch_relab<-na.omit(x_noch_relab)
		#x_noch_relaball1 <-na.omit(x_noch_relaball1)

		x_noch_relaball1 <- ddply(x_noch_relab, .(rtCode), function(x) data.frame(x, rt_relab_trim=merge(x, M_index_trim, all.x=TRUE)$V1))
		x_noch_relaball  <- ddply(x_noch_relaball1, .(rtCode), function(x) data.frame(x, pt_relab_trim=merge(x, X_index_trim, all.x=TRUE)$V1))

	  ## select correct flow from relability:
		comparRel <- x_noch_relaball[,"rt_relab_trim"] > x_noch_relaball[,"pt_relab_trim"]
		sameRel <- 100*mean(x_noch_relaball[,"rt_relab_trim"] == x_noch_relaball[,"pt_relab_trim"])
		
		cat("% with same relability index: ", sameRel, "%\n",sep="", file=file, append=TRUE)

		
		for(i in 1:nrow(x_noch_relaball)){
		#cat("i\n",i)
		  keep <-   ifelse(comparRel[i], "NW_M", "NW_X")
		  change <- ifelse(comparRel[i], "NW_X", "NW_M")
		  x_noch_relaball[i,keep] <- x_noch_relaball[i,change]
		  x_noch_relaball[i,"metaData"] <- paste("impu_reliab", ifelse(comparRel[i], "M", "X"),sep="_")
		 }
		 
		#recondition values and unit values 
	x_noch_relaball[x_noch_relaball$metaData=="impu_reliab_X","TV_X" ]<-x_noch_relaball[x_noch_relaball$metaData=="impu_reliab_X","TV_M"]*.9
	x_noch_relaball[x_noch_relaball$metaData=="impu_reliab_M","TV_M" ]<-x_noch_relaball[x_noch_relaball$metaData=="impu_reliab_M","TV_X"]*1.1
	x_noch_relaball$UV_M <- x_noch_relaball$TV_M/x_noch_relaball$NW_M * 1000;
	x_noch_relaball$UV_X <- x_noch_relaball$TV_X/x_noch_relaball$NW_X * 1000;
	#write.csv(x_noch_relaball,"rel.csv", row.names=FALSE)
		 
	  ### Add to big data frame:
		if(!all(x_noch_relaball[,1:2]==x_noch[,1:2])) stop("Problem with function!\n")
		rownames(x_noch_relaball) <- rownames(x_noch)
		x$errorNW <- x$rt_relab <- x$pt_relab <- x$rt_relab_trim <- x$pt_relab_trim <- NA
		x[rownames(x_noch_relaball),] <- x_noch_relaball[rownames(x_noch_relaball),]
	   
	  } 

# opt_noneu_compValRel global variable will contain consolidated trade flow data between countries, excluding intra-EU
# trade if the user selected to exclude it
	if(incEU.INTRA==FALSE)
	{
		opt_noneu_compValRel<<-x[x$rtCode %in% euCodes & x$ptCode %in% euCodes,]
	} else
	{
		opt_noneu_complete_compValRel<<-x
	}
#NAC	if(outREL==TRUE)
#NAC	{
#NAC		write.csv(complete_compValRel, file=paste(paste(commYR,"compCompValRel",sep=""),".csv", sep=ext), row.names=FALSE) 
#NAC	}

# the return value contain consolidated trade flow data between countries, always including intra-EU trade
	return(x)
}

#################################### function ###############################
# trade.ProcessOne() Process one commodity and year
# @param com The commodity ID (HS)
# @param yr The year
#############################################################################
trade.ProcessOne = function (com, yr)
{
# a couple of variables to support Adam's original code
	xmlUNSD <- paste(com,yr,sep=".")
	commYR <- xmlUNSD

	intYR <- as.numeric(yr)
	intHS <- as.numeric(com)
	trade.SetEuCodes (intYR)

	rawDF_raw = trade.GetData (com, yr)
	if (is.null(rawDF_raw))
	{
		print (paste("No data found for commodity",com,"year",yr,sep=" "))
		return(NULL)
	}

#NAC	rawDF_raw <- xmlToDataFrame(xmlUNSD, stringsAsFactors = FALSE,
#NAC							colClass = c("character", rep("numeric", 12)))	
							
#NAC	rawDF = ddply(.data = rawDF_raw, .variable = .(pfCode, yr, rgCode, rtCode, ptCode, cmdCode, qtCode, estCode, htCode),
	rawDF = ddply(.data = rawDF_raw, .variable = .(yr, rgCode, rtCode, ptCode, cmdCode, qtCode),
			.fun = function(x) colSums(x[, c("NetWeight", "TradeValue")])
			);
			
	if (is.null(rawDF))
	{
		print (paste("No raw DF data found for commodity",com,"year",yr,sep=" "))
		return(NULL)
	}
	rawDF$metaData<- rep("",nrow(rawDF));

	print(paste("Processing",xmlUNSD, sep=" "))

#NAC	if(outPRE==TRUE){
#NAC		write.csv(rawDF,file=paste(xmlUNSD,"_pre.csv", sep=""), row.names=FALSE)
#NAC	}

	#non reporting countries
	nonRep<- rawDF[rawDF$ptCode  %in% rawDF$rtCode ==FALSE,]  
	nonRep<-nonRep[!nonRep$ptCode==0,]

	nonRepImp<<- nonRep[nonRep$rgCode==FLOW_IMPORT,] 
	nonRepImpList<<-unique(nonRepImp$ptCode)
	nonRepExp<<- nonRep[nonRep$rgCode==FLOW_EXPORT,] 
	nonRepExpList<<-unique(nonRepExp$ptCode)

	# record re-imports and re-exports
	reMXcodes<-c(FLOW_REEXPORT,FLOW_REIMPORT)
	reMX<-rawDF[rawDF$rgCode %in% reMXcodes,]
	reMX<-reMX[!reMX$ptCode==0,]

#NAC	if(outPRE==TRUE){
# Output re-imports/exports
#NAC		write.csv(reMX,file=paste(paste(commYR, "reXM",sep=""),".csv", sep=ext), row.names=FALSE)
#NAC	}

	#add re-exports to exports
	rawDF$rgCode[rawDF$rgCode==FLOW_REEXPORT]<-FLOW_EXPORT
	#add re-imports to imports
	rawDF$rgCode[rawDF$rgCode==FLOW_REIMPORT]<-FLOW_IMPORT

	errFile=paste(paste(intHS,intYR,sep="_"), "err.txt",sep="")
	if(file.exists(errFile))
	unlink(errFile)

#NACif(max(rawDF$rgCode>1))
	if(max(rawDF$rgCode)>1)
	{
		#############################################
		## Data cleaning
		#############################################

		## case 1: duplicated values
		anyDuplicated(rawDF_raw[,c("rtCode", "ptCode", "rgCode", "cmdCode", "qtCode")])
		dup_below <- duplicated(rawDF_raw[,c("rtCode", "ptCode", "rgCode", "cmdCode", "qtCode")])
		dup_above <- duplicated(rawDF_raw[,c("rtCode", "ptCode", "rgCode", "cmdCode", "qtCode")], fromLast=TRUE)
		allDup <- apply(cbind(dup_above, dup_below), 1, any)

		anyDuplicated(rawDF[,c("rtCode", "ptCode", "rgCode", "cmdCode", "qtCode")])

		## case 2: self trade!
		rawDF <- subset(rawDF, rtCode!=ptCode)

		## case 3: redundant country codes
		AREAS_REDUNDANT = c(0, 97, 158,  412, 527, 568, 577, 637, 837, 838,  899) #840,
		AREAS_UNSPECIFIED = c(839, 899)
		AREAS_PROC_2_DISS = data.frame (
			proc=c(842, 251, 381, 579, 757),
			diss=c(840, 250, 380, 578, 756)
		)

		## get unspecified data before eliminating redundancy 
		unSpecCode=c(839,899)
		unSpecDF<-rawDF[rawDF$ptCode %in% AREAS_UNSPECIFIED,] 

		rawDF<-rawDF[rawDF$rtCode  %in% AREAS_REDUNDANT==FALSE,] 
		rawDF<-rawDF[rawDF$ptCode  %in% AREAS_REDUNDANT==FALSE,] 
		rawDF<-rawDF[rawDF$ptCode  %in% AREAS_UNSPECIFIED==FALSE,] 
		rawDF<-rawDF[rawDF$rtCode  %in% AREAS_UNSPECIFIED==FALSE,] 

		## dissemination versus processing codes
		for (i in 1:nrow(AREAS_PROC_2_DISS))
		{
			rawDF$rtCode[rawDF$rtCode == AREAS_PROC_2_DISS$proc[i]] <- AREAS_PROC_2_DISS$diss[i]
			rawDF$ptCode[rawDF$ptCode == AREAS_PROC_2_DISS$proc[i]] <- AREAS_PROC_2_DISS$diss[i]
		}

		trade.SetEuCodes (intYR)

		if(max(rawDF$rgCode)>1)
		{
			## case 5: missing NW in qtCode==8
			cond_NW8 <- rawDF$qtCode==UNIT_CODE_KG & rawDF$NetWeight==0 | is.na(rawDF$NetWeight)==TRUE
			rawDF[cond_NW8,"qtCode"] <-UNIT_CODE_NONE
			cond_NW5 <- rawDF$qtCode==UNIT_CODE_HEAD & rawDF$NetWeight==0 | is.na(rawDF$NetWeight)==TRUE
			rawDF[cond_NW5,"qtCode"] <-UNIT_CODE_NONE
			cond_NW7 <- rawDF$qtCode==UNIT_CODE_LITRE & rawDF$NetWeight==0 | is.na(rawDF$NetWeight)==TRUE
			rawDF[cond_NW7,"qtCode"] <-UNIT_CODE_NONE

			#############################################
			## Data partitioning [parts not used]
			#############################################
			parts<-unique(unlist(substr(rawDF$cmdCode[!is.na(rawDF$cmdCode)],1,6), use.names = FALSE))
			parts<-paste(paste("^",parts,sep=""), ".*", sep="")
			whole<-paste(sub("^", "^", intHS), ".*", sep="")

			LogFile=paste(paste(intHS,intYR,sep="_"), ".txt",sep="")
			if(file.exists(LogFile)) unlink(LogFile)

# AddReliability sets the global variable opt_noneu_compValRel will contain cleaned, mirrored, balanced trade flow data,
# excluding intra-EU trade if requested by the user.
# AddReliability returns the complete data, always including the intra-EU trade
# If the user did not request to exclude intra-EU trade these two will be identical
			trade.CountUnidirectional(rawData=rawDF)
			data1 <- trade.ToCompareData(data=rawDF, HScode=whole)
			data2 <- trade.CompleteCompareData(data1,file=LogFile)
			complete_compValRel <- trade.AddReliability(x=data2,errTol=5, file=LogFile)
#NAC			complete_compValRel <- trade.AddReliability(x=trade.CompleteCompareData(trade.ToCompareData(data=rawDF, HScode=whole),file=LogFile),errTol=5, file=LogFile)
			trade.SaveCompletedTF (com, yr, complete_compValRel)

			##alerts for significantly missing 
			unSpecDF <- subset(unSpecDF, NetWeight/sum(x$NW_X)>=0.01)
			if(nrow(unSpecDF)>0)
			{
				if(outMISSPEC==TRUE)
				{
					write.csv(unSpecDF,paste(paste(intHS,intYR,sep="_"), "-unSPEC.csv",sep=""), row.names=FALSE)
				}
				cat("ATTENTION, SIGNIFICANT UNSPECIFIED TRADE of: ", unSpecDF$NetWeight, "\n", file=LogFile, append=TRUE)
			}

			missNW <- subset(rawDF,rawDF$qtCode==1)
			missNW <- subset(missNW,missNW[missNW$qtCode==1,"TradeValue"]/sum(rawDF$TradeValue)>=0.001)

			if(nrow(missNW)>0)
			{
				if(outMISSPEC==TRUE)
				{
					write.csv(missNW,paste(paste(intHS,intYR,sep="_"), "-MISS.csv",sep=""), row.names=FALSE)
				}
				cat("ATTENTION, SIGNIFICANT MISSING TRADE associated with value of: ", missNW$TradeValue, "\n", file=LogFile, append=TRUE)
			}


			if(outXML==TRUE)
			{
# aggregate import(M) quantities(Q) and values(V) across partners
				MQ<-aggregate(cbind(NW_M)~rtCode, data=complete_compValRel, FUN=sum)
				colnames(MQ)[colnames(MQ) == "NW_M"] = paste0("MQ", commYR)
				MV<-aggregate(cbind(TV_M)~rtCode, data=complete_compValRel, FUN=sum)
				colnames(MV)[colnames(MV) == "TV_M"] = paste0("MV", commYR)
				M<- merge(MQ,MV,by="rtCode")

# aggregate export(X) quantities(Q) and values(V) across partners
				XQ<-aggregate(cbind(NW_X)~ptCode, data=complete_compValRel, FUN=sum)
				colnames(XQ)[colnames(XQ) == "NW_X"] = paste0("XQ", commYR)
				XV<-aggregate(cbind(TV_X)~ptCode, data=complete_compValRel, FUN=sum)
				colnames(XV)[colnames(XV) == "TV_X"] = paste0("XV", commYR)
				X<- merge(XQ,XV,by="ptCode")

				write.csv(M,file=paste(paste(commYR,"relM",sep="_"),".csv", sep=ext), row.names=FALSE) 
				write.csv(X,file=paste(paste(commYR,"relX",sep="_"),".csv", sep=ext), row.names=FALSE) 

# MX will contain imports and exports aggregated to totals per reporting country
				MX = merge(M,X,by.x="rtCode",by.y="ptCode")
				trade.SaveTotals (com, yr, MX)

				outFile=paste(commYR,"_final.csv",sep="")
				if(file.exists(outFile))
					unlink(outFile)
				for(df in list(M,X))
				{
					if(substr(colnames(df[2]),0,2)=="XQ")
					{
						ix<-c("X")
					}else
					{
						ix<-c("M")
					}
					data <-df
		#			xml <- xmlTree()
		#			xml$addTag("document", close=FALSE)
					for (i in 1:nrow(data))
					{
						write.csv(data[i,],file=outFile, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
		#				xml$addTag("row", close=FALSE)
		#				for (j in names(data))
		#				{
		#					xml$addTag(j, data[i, j])
		#				}
		#				xml$closeTag()
					}
		#			xml$closeTag()
		#			saveXML(xml,paste(paste(commYR, ix),".xml", sep=""))
				}
			}
			 
		} else {
			cat("No EU extra EXPORTS in ", xmlUNSD, "\n", file=errFile, append=TRUE)
		} 

	} else {
		cat("No EXPORTS in ", xmlUNSD, "\n", file=errFile, append=TRUE)
	}

# Clean up memory
	rm(list=purgeList)
	gc(verbose=FALSE)
}

trade.ProcessAll = function()
{
# temporary workaround for R-API issue
	keys <- swsContext.datasets[[1]]@dimensions
	for (i in 1:length(keys))
	{
		if (keys[[i]]@name == "timePointYears")
		{
			years <- as.character(keys[[i]]@keys)
		} else if (keys[[i]]@name == "measuredItemHS")
		{
			comms <- as.character(keys[[i]]@keys)
		}
	}

	if ((length(years) > 0) && (length(comms) > 0))
	{
		for (yr in 1:length(years))
		{
			for (com in 1:length(comms))
			{
				trade.ProcessOne (comms[com], as.numeric(years[yr]))
			}
		}
	}
}

#debug(trade.SaveTotals)
#debug(trade.ProcessOne)
debug(trade.SaveCompletedTF)
debug(trade.CompleteCompareData)
debug(trade.ProcessOne)

trade.ProcessAll()

warn = warnings()
print (warn)
if (!TEST_MODE)
{
	sink (file=NULL,type="message")
	sink (file=NULL,type="output")
	close(consolefile)
}

"Trade processing completed successfully"
