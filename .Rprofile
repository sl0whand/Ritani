

## Do you want to automatically convert strings to factor variables in a data.frame?
options(stringsAsFactors=FALSE)

options(scipen=10)
options(continue="... ")

.env <- new.env()
## Returns a logical vector TRUE for elements of X not in Y
.env$"%nin%" <- function(x, y) !(x %in% y) 

## Strip row names from a data frame (stolen from plyr)
.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

##omits colums with only NA values
.env$omit_NA_cols=function(tmpfile){
  
  NA_cols=apply(tmpfile,2,function(x){sum(is.na(x))==length(x)})
  tmpfile=tmpfile[,!NA_cols]
  return(tmpfile)
} 

## forces rbind to row bind now variables
.env$rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  if (!is.null(x)){
    if (length(y.diff)>0 & ncol(x)>0) {
      x[, c(as.character(y.diff))] <- NA
    }
    
    if (length(x.diff)>0 & ncol(x)>0) {
      y[, c(as.character(x.diff))] <- NA
    }
  }
  
  return(rbind(x,y))
}

attach(.env)



set.seed(2)
.First <- function() cat("\n   Welcome to R!\n\n")

.Last <- function()  cat("\n   Goodbye!\n\n")



message("\n*** Successfully loaded .Rprofile ***\n")
