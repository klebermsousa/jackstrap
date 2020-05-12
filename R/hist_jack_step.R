#' Histogram with Jackstrap Efficiency Indicators:  This function builds a graphic with indicator distributions
#' without outliers and complete sample. The outliers are defined by heaviside step function method.
#'@param efficiency is the jackstrap object created by jackstrap function.
#'@param model_hist_step is the desired graphic model. There are four kinds: 1- Density Histogram of efficiency indicators with
#'complete sample and without outliers by heaviside step function; 2 - Histogram of efficiency with complete sample and without outliers
#'by heaviside step function; 3 - Histogram of efficiency without ouliers by heaviside step function; 4 - Histogram of efficiency with complete sample.
#'@return Return the plot with efficiency indicators with complete sample and/or without outliers by heaviside step function;
#'@export
hist_jack_step <- function(efficiency, model_hist_step) {

  library("fBasics")
  library("Benchmarking")
  library(dplyr)
  library("ggplot2")

  if (model_hist_step !=1 & model_hist_step !=2 & model_hist_step !=3 & model_hist_step !=4) {
    stop("Histogram model must be 1, 2, 3 or 4! Please change parameter: model_hist_step")
  }

  if (model_hist_step == 1) {

    eff_step_func_without_outliers <- data.frame(efficiency[["efficiency_step_func"]]$code,efficiency[["efficiency_step_func"]]$efficiency_withoutoutlier)
    eff_step_func_without_outliers$group <- 'Efficiency_without_outliers'
    colnames(eff_step_func_without_outliers) <- c("codeDMU","efficiency_indicator","group")
    eff_step_func_completo <- data.frame(efficiency[["efficiency_step_func"]]$code,efficiency[["efficiency_step_func"]]$efficiency_complete)
    eff_step_func_completo$group <- 'Efficiency_complete'
    colnames(eff_step_func_completo) <- c("codeDMU","efficiency_indicator","group")
    efficiency_step_function <- rbind(eff_step_func_without_outliers,eff_step_func_completo)
    plot_hist_step <- ggplot(efficiency_step_function, aes(efficiency_indicator, fill = group)) + geom_density(alpha = 0.2) + labs(title="Histogram of efficiency indicators with Step Function", x ="Efficiency Indicator Intensity", y = "Density")

  } else if (model_hist_step == 2) {

    plot_hist_step <- hist(efficiency[["efficiency_step_func"]]$efficiency_withoutoutlier,xlim=c(0,1),col='skyblue',border=F, main = "Histogram of efficiency", xlab = "Efficiency Indicator Intensity")
    plot_hist_step <- hist(efficiency[["efficiency_step_func"]]$efficiency_complete,add=T,col=scales::alpha('red',.5),border=F)

  } else if (model_hist_step == 3) {

    plot_hist_step <- hist(efficiency[["efficiency_step_func"]]$efficiency_withoutoutlier,xlim=c(0,1),col='skyblue',border=F, main = "Histogram of efficiency without Outliers - Step Function", xlab = "Efficiency Indicator Intensity")

  } else if (model_hist_step == 4) {

    plot_hist_step <- hist(efficiency[["efficiency_step_func"]]$efficiency_complete,xlim=c(0,1),col='red',border=F, main = "Histogram of Efficiency complete sample", xlab = "Efficiency Indicator Intensity")
  }

  return(plot_hist_step)
}
