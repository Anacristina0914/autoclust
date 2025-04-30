#' Convert p-value to significance label
#'
#' Returns a conventional significance label (e.g., `"*"`, `"**"`, `"***"`, or `"ns"`)
#' based on the provided p-value.
#'
#' @param pval A numeric p-value.
#'
#' @return A character string representing the significance level:
#'   - `"***"` for p < 0.001
#'   - `"**"` for p ≤ 0.01
#'   - `"*"` for p ≤ 0.05
#'   - `"ns"` (not significant) otherwise
#'
#' @examples
#' signif_label(0.0005)  # returns "***"
#' signif_label(0.02)    # returns "*"
#' signif_label(0.3)     # returns "ns"
#'
#' @export
signif_label <- function(pval){
  if(pval < 0.001){
    return("***")
  } else if(pval <= 0.01 && pval > 0.001){
    return("**")
  } else if(pval <= 0.05 && pval > 0.01){
    return("*")
  } else{
    return("ns")
  }
}
