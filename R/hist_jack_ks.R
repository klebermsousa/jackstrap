#' Histogram with Jackstrap Efficiency Indicators:  This function builds graphics with distributions of efficiency indicators
#' without outliers and complete sample. The outliers are defined by K-S Test.
#'@param efficiency is the jackstrap object created by jackstrap function.
#'@param model_hist_ks is the desired graphic model. There are four kinds: 1- Density Histogram of efficiency indicator with
#'complete sample and without outliers by K-S test; 2 - Histogram of efficiency with complete sample and without outliers by K-S test;
#'3 - Histogram of efficiency without ouliers by K-S test; 4 - Histogram of efficiency with complete sample.
#'@return Return the plot with efficiency indicators with complete sample and/or without outliers by combination leverage level and K-S test;
#'@examples
#'  \dontshow{
#'    library(jackstrap)
#'    test_data <- data.frame(mun=c(1:10), cod=c(1:10), y=c(5,7,6,7,4,6,8,9,3,1),
#'                           x=c(7,8,10,22,15,7,22,17,10,5))
#'    effic_test <- jackstrap (data=test_data, ycolumn=1, xcolumn=1, bootstrap=1,
#'                   perc_sample_bubble=1, dea_method="crs", orientation_dea="in",
#'                  n_seed = 2000, repos=FALSE, num_cores=1)
#'    effic_ks <- jackstrap_ks (data=test_data, jackstrap_obj=effic_test,
#'                             num_cores = 1)
#'    plot_ks <- hist_jack_ks(effic_ks, 1)
#'  }
#'  \donttest{
#'     #Build charts with efficiency indicators with jackstrap method and K-S test criterion
#'     hist_jack_ks(efficiency_ks, 1)
#'     hist_jack_ks(efficiency_ks, 2)
#'     hist_jack_ks(efficiency_ks, 3)
#'     hist_jack_ks(efficiency_ks, 4)
#'  }
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_density
#'@importFrom ggplot2 labs
#'@importFrom graphics hist
#'@importFrom stats quantile
#'@export
hist_jack_ks <- function(efficiency, model_hist_ks) {

  if (model_hist_ks !=1 & model_hist_ks !=2 & model_hist_ks !=3 & model_hist_ks !=4) {
    stop("Histogram model must be 1, 2, 3 or 4! Please change parameter: model_hist_ks")
  }

  if (model_hist_ks == 1) {

    eff_ks_without_outliers <- data.frame(efficiency[["efficiency_ks_method"]]$code,efficiency[["efficiency_ks_method"]]$efficiency_withoutoutlier)
    eff_ks_without_outliers$group <- 'Efficiency_without_outliers'
    colnames(eff_ks_without_outliers) <- c("codeDMU","efficiency_indicator","group")
    eff_ks_completo <- data.frame(efficiency[["efficiency_ks_method"]]$code,efficiency[["efficiency_ks_method"]]$efficiency_complete)
    eff_ks_completo$group <- 'Efficiency_complete'
    colnames(eff_ks_completo) <- c("codeDMU","efficiency_indicator","group")
    efficiency_ks <- rbind(eff_ks_without_outliers,eff_ks_completo)
    plot_hist_ks <- ggplot(efficiency_ks, aes(efficiency_indicator, fill = group)) + geom_density(alpha = 0.2) + labs(title="Histogram of efficiency indicators with K-S Test", x ="Efficiency Indicator Intensity", y = "Density")

  } else if (model_hist_ks == 2) {

    plot_hist_ks <- hist(efficiency[["efficiency_ks_method"]]$efficiency_withoutoutlier,xlim=c(0,1),col='skyblue',border=F, main = "Histogram of efficiency", xlab = "Efficiency Indicator Intensity")
    plot_hist_ks <- hist(efficiency[["efficiency_ks_method"]]$efficiency_complete,add=T,col=scales::alpha('red',.5),border=F)

  } else if (model_hist_ks == 3) {

    plot_hist_ks <- hist(efficiency[["efficiency_ks_method"]]$efficiency_withoutoutlier,xlim=c(0,1),col='skyblue',border=F, main = "Histogram of efficiency without Outliers - K-S Test", xlab = "Efficiency Indicator Intensity")

  } else if (model_hist_ks == 4) {

    plot_hist_ks <- hist(efficiency[["efficiency_ks_method"]]$efficiency_complete,xlim=c(0,1),col='red',border=F, main = "Histogram of efficiency with complete sample", xlab = "Efficiency Indicator Intensity")
  }

  return(plot_hist_ks)
}
