#' Create TFI Table
#'
#' @param in.table
#'
#' @return
#'
#' @examples
create_TFI = function(in.table){
TFI <- in.table[,c(1,4:5)]
names(TFI) <- c("KeithClass","TFImin","TFImax")

TFI$TFImax[TFI$TFImax == "No Planned" | TFI$TFImax == "No planned burning" | TFI$TFImax == "No Planned Burning"] <- "0"
TFI$TFI.max[TFI$TFI.max == "40+"] <- "40"
TFI$TFI.min[TFI$TFI.min == "No Planned Burning" | TFI$TFI.min == "No planned burning" | TFI$TFI.min == "No Planned"] <- "0"

TFI$TFI.max <- as.numeric(TFI$TFI.max)
TFI$TFI.min <- as.numeric(TFI$TFI.min)

TFI
}


# Creating a wrapper around an existing function in another package
is.odd = function(x){
  gtools::odd(x)
}

