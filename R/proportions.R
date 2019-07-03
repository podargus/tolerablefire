
#' proportions
#'
#' Function allows the tolerable fire interval proportions to be calculated above, below and within predetermined thresholds
#'
#' @param KeithClass Keith Class that is desired (see provided table for list)
#' @param Year.min Minimum year of fire history information
#' @param Year.max Maximum year of fire history information
#' @param sqmetres Resolution of input pixels (square metres)
#' @param TFI TFI Thresholds
#'
#' @return Matrix of percent of total area that is within, below or above TFI thresholds for a given Keith Class
#' @export
#'
#' @examples
proportions <- function(KeithClass, Year.min, Year.max, sqmetres, TFI) {
  KeithClass$OID = NULL
  KeithClass$TSF = year.max - KeithClass$Value
  names(KeithClass) = c("Year","Count","KeithClass","TSF")
  KeithClass[,c(1,3)] <- lapply(KeithClass[,c(1,3)], factor)
  KeithClass$Count = KeithClass$Count / 400 # Convert count to hectares: 1 cell = 25m2 = 400 cells to 1ha
  KeithClass$Count = as.integer(KeithClass$Count+0.5) # round to nearest whole number

  KeithClass.2 <- data.frame(KeithClass$KeithClass, KeithClass$TSF, KeithClass$Count)
  names(KeithClass.2) = c("KeithClass","TSF","Count")
  KeithClass.2 <- KeithClass.2[rep(seq_len(nrow(KeithClass.2)), KeithClass.2$Count), 1:2]

  ## Make burns in 1900 = one year in the future
  KeithClass.3 <- KeithClass.2
  KeithClass.3$TSF[KeithClass.3$TSF == (2019-1900)] <- -1

  ## Create vector of solely TSF cell values
  KClass <- subset(KeithClass.3, KeithClass.3$KeithClass == Class)
  KClass <- as.vector(KClass[,2])

  ##Mean and standard deviation of ideal fire frequency thresholds
  FMinACT = TFI$TFI.min[TFI$KeithClass == Class]
  FMaxACT = TFI$TFI.max[TFI$KeithClass == Class]

  FMean<- (FMinACT+FMaxACT)/2
  Fsd<-(FMaxACT-FMinACT)/4

  ##Maximum observed time since fire
  TSFMax = max(KClass)

  ##Maximum proportion
  TSFDistribution<-hist(KClass,breaks=TSFMax)
  MaximumDensity<-max(TSFDistribution$density)
  MaxProp<-round_any(MaximumDensity, 0.1, f = ceiling)

  ##Calculate percentages obersvation below, within and above the ideal age classes.
  ##ACT
  Below_Threshold <-(NROW(KClass[ which(KClass < FMinACT)])/NROW(KClass))*100
  Above_Threshold <-(NROW(KClass[ which(KClass > FMaxACT)])/NROW(KClass))*100
  Within_Threshold <-(NROW(KClass[ which(KClass > FMinACT & KClass <FMaxACT)])/NROW(KClass))*100

  ### Dataframe of proportion of Keith Class Above/Within/Below threshold
  TFI_thresholds = data.frame(KeithClass=as.vector(Class),
                              Below_Threshold=as.vector(Below_Threshold),
                              Within_Threshold=as.vector(Within_Threshold),
                              Above_Threshold=as.vector(Above_Threshold))

  print(TFI_thresholds)

  write.csv2(TFI_thresholds,paste(Class,".csv"))

    use_package("ggplot2")

}

