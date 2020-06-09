#' Summary Jackstrap:  This function shows the main outcomes with outlier technique developed by
#' Sousa and Stosic(2005).
#'@param object_jackstrap is the jackstrap object created by jackstrap function.
#'@param data is the dataset of research.
#'@return Return the data frame with information as follows: "outliers_by_step_func" are the outliers by heaviside step function criterion;
#'"outliers_by_ks" are the outliers by K-S test; "dmu_efficiency_by_step_func" are DMUs evaluated as efficient by heaviside step function criterion;
#'"dmu_inefficiency_by_step_func" are the DMUs evaluated as maximum inefficient by heaviside step function criterion;
#'"dmu_efficiency_ks" are DMUs evaluated as efficient by K-S test criterion; "dmu_inefficiency_by_ks" are the DMUs evaluated as maximum inefficient by K-S test criterion.
#'@examples
#'  \dontshow{
#'    library(jackstrap)
#'    test_data <- data.frame(mun=c(1:10), cod=c(1:10), y=c(5,7,6,7,4,6,8,9,3,1),
#'                            x=c(7,8,10,22,15,7,22,17,10,5))
#'    effic_test <- jackstrap (data=test_data, ycolumn=1, xcolumn=1, bootstrap=1,
#'                   perc_sample_bubble=1, dea_method="crs", orientation_dea="in",
#'                  n_seed = 2000, repos=FALSE, num_cores=1)
#'    effic_ks <- jackstrap_ks (data=test_data, jackstrap_obj=effic_test,
#'                             num_cores = 1)
#'  }
#'  \donttest{
#'     #Create object with the resume of efficiency measurement.
#'     summary_efficiency <- summary_jackstrap(efficiency_ks, municipalities)
#'  }
#'@export
summary_jackstrap <- function(object_jackstrap, data) {

  dataset_orig <- data

  dataset_sum <- object_jackstrap

  test_obj_ks <- dataset_sum[["type_obj"]]

  summary <- NULL
  summary[["parameters"]] <- dataset_sum[["parameters"]]
  summary[["outliers_by_step_func"]] <- subset(dataset_sum[["mean_leverage"]], dataset_sum[["mean_leverage"]]$outlier==0)
  summary[["outliers_by_step_func"]][,2-3] <- NULL
  colnames(summary[["outliers_by_step_func"]]) <- c("codeDMU")
  if (test_obj_ks=='jackstrap_ks_obj') {
    summary[["outliers_by_ks"]] <- subset(dataset_sum[["result_kstest_method"]], dataset_sum[["result_kstest_method"]]$p_value<0.99)
    summary[["outliers_by_ks"]][,2] <- NULL
    colnames(summary[["outliers_by_ks"]]) <- c("codeDMU")
  }
  summary[["dmu_efficiency_by_step_func"]] <- subset(dataset_sum[["efficiency_step_func"]], dataset_sum[["efficiency_step_func"]]$efficiency_withoutoutlier==1)
  summary[["dmu_efficiency_by_step_func"]][,3] <- NULL
  colnames(summary[["dmu_efficiency_by_step_func"]]) <- c("codeDMU","efficiency_indicator")
  min_efficiency_by_step <- min(dataset_sum[["efficiency_step_func"]]$efficiency_withoutoutlier)
  summary[["dmu_inefficiency_by_step_func"]] <- subset(dataset_sum[["efficiency_step_func"]], dataset_sum[["efficiency_step_func"]]$efficiency_withoutoutlier==min_efficiency_by_step)
  summary[["dmu_inefficiency_by_step_func"]][,3] <- NULL
  colnames(summary[["dmu_inefficiency_by_step_func"]]) <- c("codeDMU","efficiency_indicator")


  if (test_obj_ks=='jackstrap_ks_obj') {
    summary[["dmu_efficiency_by_ks"]] <- subset(dataset_sum[["efficiency_ks_method"]], dataset_sum[["efficiency_ks_method"]]$efficiency_withoutoutlier==1)
    summary[["dmu_efficiency_by_ks"]][,3] <- NULL
    colnames(summary[["dmu_efficiency_by_ks"]]) <- c("codeDMU","efficiency_indicator")
    min_efficiency_by_ks <- min(dataset_sum[["efficiency_ks_method"]]$efficiency_withoutoutlier)
    summary[["dmu_inefficiency_by_ks"]] <- subset(dataset_sum[["efficiency_ks_method"]], dataset_sum[["efficiency_ks_method"]]$efficiency_withoutoutlier==min_efficiency_by_ks)
    summary[["dmu_inefficiency_by_ks"]][,3] <- NULL
    colnames(summary[["dmu_inefficiency_by_ks"]]) <- c("codeDMU","efficiency_indicator")
  }
  nome_dmu <- dataset_orig[,1:2]
  summary[["dmu_efficiency_by_step_func"]] <- merge(summary[["dmu_efficiency_by_step_func"]], nome_dmu, by.x="codeDMU", by.y="cod", all.x=TRUE)
  summary[["dmu_efficiency_by_step_func"]] <- summary[["dmu_efficiency_by_step_func"]] %>% select(codeDMU, everything())
  summary[["dmu_inefficiency_by_step_func"]] <- merge(summary[["dmu_inefficiency_by_step_func"]], nome_dmu, by.x="codeDMU", by.y="cod", all.x=TRUE)
  summary[["dmu_inefficiency_by_step_func"]] <- summary[["dmu_inefficiency_by_step_func"]] %>% select(codeDMU, everything())
  if (test_obj_ks=='jackstrap_ks_obj') {
    summary[["dmu_efficiency_by_ks"]] <- merge(summary[["dmu_efficiency_by_ks"]], nome_dmu, by.x="codeDMU", by.y="cod", all.x=TRUE)
    summary[["dmu_efficiency_by_ks"]] <- summary[["dmu_efficiency_by_ks"]] %>% select(codeDMU, everything())
    summary[["dmu_inefficiency_by_ks"]] <- merge(summary[["dmu_inefficiency_by_ks"]], nome_dmu, by.x="codeDMU", by.y="cod", all.x=TRUE)
    summary[["dmu_inefficiency_by_ks"]] <- summary[["dmu_inefficiency_by_ks"]] %>% select(codeDMU, everything())
  }
  summary[["outliers_by_step_func"]] <- merge(summary[["outliers_by_step_func"]], nome_dmu, by.x="codeDMU", by.y="cod", all.x=TRUE)
  if (test_obj_ks=='jackstrap_ks_obj') {
     summary[["outliers_by_ks"]] <- merge(summary[["outliers_by_ks"]], nome_dmu, by.x="codeDMU", by.y="cod", all.x=TRUE)
  }

  return(summary)
}
