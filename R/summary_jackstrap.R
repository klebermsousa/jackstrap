#' Summary Jackstrap:  This function shows the main outcomes with outlier technique developed by
#' Sousa and Stosic(2005).
#'@param object_jackstrap is the jackstrap object created by jackstrap function.
#'@param data is the dataset of research.
#'@return Return the data frame with follow informations: "parameters" contain the parameters used on function;
#'"outliers_by_step_func" are the outliers by heaviside step function criteria; "outliers_by_ks" are the outliers by K-S test;
#'"dmu_efficiency_by_step_func" are DMUs evaluated as efficients by heaviside step function criteria; "dmu_inefficiency_by_step_func" are the DMUs evaluated as maximum inefficient by heaviside step function criteria;
#'"dmu_efficiency_ks" are DMUs evaluated as efficients by K-S test criteria; "dmu_inefficiency_by_ks" are the DMUs evaluated as maximum inefficient by K-S test criteria.
#'@export
summary_jackstrap <- function(object_jackstrap, data) {

  library("fBasics")
  library("Benchmarking")
  library(dplyr)

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
