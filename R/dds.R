#' dds function
#'
#' Creates textual summary of data or function output.
#'
#' This function converts graphical output to textual and auditive output.
#'
#' @param x Data or output of a function which needs description
#' @param fun Function that needs to be executed on the data (optional)
#' @param a Auditive output required (default = TRUE)
#' #'
#' @author Yves R. Sagaert
#'
#'
#' @return Textual output and auditive output (when a=TRUE)
#' @export
#'
#' @examples
#'   \dontrun{
#'      library(sonify)
#'      x <- rnorm(100)
#'      dds(x)
#'   }
#'
#' x <- rnorm(100)
#' dds(x,a=FALSE)
#'


dds <- function(x, fun = "", a = TRUE){
  
  # Not the additional function information
  if (a==TRUE){sonify::sonify(x)}
  cat(paste0("The data contains ", length(x), " points with mean ",
             round(mean(x, na.rm = TRUE), 1), ", variance ",
             round(stats::var(x, na.rm = TRUE), 1), ".\n"))
  
  if (!(fun=="")){
    # Additional info on function goes here
    if (fun=="ts"){
      # Description of the time series data
      # Cox and Stuart Trend test
      trend <- dds::condition((trend::cs.test(x)$p.value<0.05),
                              "a monotonic trend",
                              "no visible trend")
      # Seasonal Mann-Kendall trend test (Hirsch-Slack test)
      seas <- dds::condition((trend::smk.test(x)$p.value<0.05),
                             "seasonal",
                             "not seasonal")
      rand <- dds::condition((trend::bartels.test(x)$p.value<0.05),
                             "significantly different from randomness",
                             "not significantly different from randomness")
      # Lanzante's procedure for single change-point detection with
      # Wilcoxon-Mann-Whitney Test
      lz <- trend::lanzante.test(x)
      changepoint <- dds::condition((lz$p.value<0.05),
                                    paste("a probable change point at time t =",lz$estimate),
                                    "no probable change point")
      cat(paste0("The time series has a frequency of ",
                 stats::frequency(x), ", is ", seas, ", has ", trend, " and is ", rand,
                 ". "))
      cat(paste0("Moreover it has ", changepoint,
                 ". "))
    }
  }
}
