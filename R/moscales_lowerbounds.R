##' @title Bounds
##' @param x data frame
##' @return
##' @export
#' @importFrom mokken aisp
moscales_lowerbounds <- function(x, lowerbounds = seq(from = 0.05, to = 0.80, by = 0.05)) {
  ret.value <- NULL
  for (lowerbound in lowerbounds)
  {
    tmp <- aisp(x, lowerbound = lowerbound)
    if (is.null(ret.value)) {
      ret.value <- data.frame("Item" = rownames(tmp), "Scales." = tmp[, 1])
    }
    else {
      ret.value <- cbind(ret.value, "Scales." = tmp[, 1])
    }
    names(ret.value)[ncol(ret.value)] <- sprintf("%.2f", lowerbound)
  }
  rownames(ret.value) <- NULL
  ret.value
}
