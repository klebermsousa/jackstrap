#' Plot Jackstrap KS:  This function plots p-value of Kolmogorov-Smirnov Test in decreasing order of leverage.
#'@param data_plot is the jackstrap object created by jackstrap function.
#'@param model_plot is the desired model. There are two models: 1 - The graphic shows the amount of removed DMU on x axis and p-value of K-S test on y axis;
#'2 - The graphic shows DMU code on x axis and p-value of K-S test on y axis.
#'@return Return the plot with p-value of K-S test and removed DMU or DMU code.
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
#'    plot_jack_ks <- plot_jackstrap_ks(effic_ks, 1)
#'  }
#'  \donttest{
#'    ##Plot the dispersion chart with p value of K-S test and amount of DMU removed.
#'    plot_jackstrap_ks(effic_ks, 1)
#'  }
#'@importFrom graphics plot
#'@importFrom graphics segments
#'@export
plot_jackstrap_ks <- function(data_plot, model_plot) {

  if (model_plot !=1 & model_plot !=2) {
    stop("Plot model must be 1 or 2! Please change parameter: model_plot")
  }

  if (model_plot==1) {

    index <- arrange(data_plot[["result_kstest_method"]], data_plot[["result_kstest_method"]]$p_value)

    nrow_index <- nrow(index)

    index$rank <- 1:nrow_index

    index <- index[1:data_plot[["bootstrap"]],]

    plot_ks <- plot(index$rank,index$p_value, xlab="Number of removed municipalities", ylab="P value of K-S test", type="o")

    high_pvalue <- subset(index, index$p_value>=0.99)

    number_cut <- high_pvalue[1,3]

    segments(number_cut, -1, number_cut, 1.2, col= 'pink')

  } else {

    index <- arrange(data_plot[["result_kstest_method"]], data_plot[["result_kstest_method"]]$codigo)

    plot_ks <- plot(index, xlab="Removed DMUs", ylab="P value of K-S test", type="o")

  }

  return(plot_ks)
}
