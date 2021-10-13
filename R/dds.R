#' dds function
#'
#' Creates textual summary of data or function output.
#'
#' This function converts graphical output to textual and auditive output.
#'
#' @param x Data or output of a function which needs description
#' @param fun Function that needs to be executed on the data (optional).
#'            Argument fun can be: c("ts","res", "acf"). See examples for
#'            individual use of fun (x is changing alongside)
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
#' # Any data
#' set.seed(1010)
#' x <- rnorm(100)
#' dds(x,a=FALSE)
#'
#' # Time series data
#' set.seed(1010)
#' x <- ts(rnorm(100),start=c(2020,1),freq=12)
#' dds(x,a=FALSE,fun="ts")
#'
#' # Residual analysis
#' library(forecast)
#' set.seed(1010)
#' x <- ts(rnorm(100),start=c(2020,1),freq=12)
#' dds(naive(x),a=FALSE,fun="res")


dds <- function(x, fun = "", a = TRUE){

  # Not the additional function information
  if (a==TRUE){sonify::sonify(x)}


  # Do additional stuff if: residuals data
  if (fun=="res"){
    # Make a time series of the residuals
    # x is forecast model here ==> new e is ts
    e <- stats::ts(stats::residuals(x)[-1],start=stats::start(x$x),freq=stats::frequency(x$x))
    cat("Residual analysis. ")
    dds::dds(e,a=FALSE,fun="ts") # getting real recursive here :P
    dds::dds(e,a=FALSE,fun="acf")
  }

  if (!(fun=="")){
    # Additional info on function goes here

    if (fun=="ts"){
      # Description of the time series data
      cat(paste0("The data contains ", length(x), " points with mean ",
                 round(mean(x, na.rm = TRUE), 1), ", variance ",
                 round(stats::var(x, na.rm = TRUE), 1), ".\n"))
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
    if (fun=="acf"){
      # Symbolic representation of ggAcf
      i <- forecast::ggAcf(x,lag.max=18) # max 18 for readability
      acf <- sign(i$data$Freq)*(abs(i$data$Freq)>0.24)
      cat("ACF representation of significant lags: ")
      cat(acf)
    }
  } else {
    cat(paste0("The data contains ", length(x), " points with mean ",
               round(mean(x, na.rm = TRUE), 1), ", variance ",
               round(stats::var(x, na.rm = TRUE), 1), ".\n"))
  }
}
