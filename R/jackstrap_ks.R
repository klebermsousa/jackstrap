#' Jackstrap KS Method: Tool identifies outliers in Nonparametric Frontier.
#' This function applies the developed technique by Sousa and Stosic (2005)
#' Technical Efficiency of the Brazilian Municipalities: Correcting Nonparametric
#' Frontier Meansurements for Outliers and to use the K-S test with criterion to define outliers.
#'@param data is the dataset with input and output used to measure efficiency; Dataset need to have this form: 1th column: name of DMU (string); 2th column: code of DMU (integer); n columns of output variables; n columns of input variables.
#'@param jackstrap_obj is the object created by the function jackstrap.
#'@param num_cores is the number of cores available to process.
#'@param perc is the percentage of DMU analyzed by K-S test.
#'@return Return the jackstrap object increased with informations as follows: "result_kstest_method" is p-values of K-S test obtained by removing sequencially one by one the high leverage DMU;
#'"efficiency_ks_method" is efficiency indicators obtained by K-S test criterion.
#'@examples
#'  \dontshow{
#'    library(jackstrap)
#'    test_data <- data.frame(mun=c(1:10), cod=c(1:10), y=c(5,7,6,7,4,6,8,9,3,1),
#'                            x=c(7,8,10,22,15,7,22,17,10,5))
#'    effic_test <- jackstrap (data=test_data, ycolumn=1, xcolumn=1, bootstrap=1,
#'                  perc_sample_bubble=1, dea_method="crs", orientation_dea="in",
#'                  n_seed = 2000, repos=FALSE, num_cores=1)
#'    effic_ks <- jackstrap_ks (data=test_data, jackstrap_obj=effic_test,
#'                               num_cores = 1)
#'  }
#'  \donttest{
#'     #Command measures efficiency with jackstrap method and K-S test criterion
#'     efficiency_ks <- jackstrap_ks (data=municipalities, jackstrap_obj=efficiency,
#'                                    num_cores = 4)
#'  }
#'@importFrom stats quantile
#'@importFrom doParallel stopImplicitCluster
#'@export
jackstrap_ks <- function(data, jackstrap_obj, num_cores=1, perc=0.90) {

  data_orig <- data

  data_orig <- arrange(data_orig,data_orig$cod)

  n_linha <- nrow(data_orig)

  bubble = jackstrap_obj[['bootstrap']]
  perc_bubble = jackstrap_obj[['perc_sample_bubble']]
  method_dea = jackstrap_obj[['dea_method']]
  orient_dea = jackstrap_obj[['orientation_dea']]
  efficiency_comp_ks <- jackstrap_obj[['efficiency_comp']]

  size_bubble = n_linha*perc_bubble


  jackstrap_ks <- function (dmu_out_t) {

    u <- dmu_out_t

    cod_dmu_analise <- leverage_dmu_outlier[(u),1]

    dmu_ks <- leverage_dmu_outlier[(1:u),1]

    dmu_ks_before <- leverage_dmu_outlier[(1:u-1),1]

    data_semoutliers_ks <- data_orig

    if (u==1) {
      data_semoutliers_ks_before = data_semoutliers_ks
    } else {
      data_semoutliers_ks_before = subset(data_semoutliers_ks, !(data_semoutliers_ks$cod %in% dmu_ks_before))
    }

    data_semoutliers_ks = subset(data_semoutliers_ks, !(data_semoutliers_ks$cod %in% dmu_ks))

    x <- as.data.frame(data_semoutliers_ks)

    xcol = jackstrap_obj[['ycolumn']]+2

    for (p in 1:xcol) {
      x[,1] <- NULL
    }

    y <- as.data.frame(data_semoutliers_ks)

    for (p in 1:jackstrap_obj[['xcolumn']]) {

      col_del = jackstrap_obj[['ycolumn']]+1

      if (p == 1) {
        y[,1] <- NULL
        y[,1] <- NULL
      }

      y[,col_del] <- NULL

    }

    dea_semoutliers_ks <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

    efficiency_semoutliers_ks <- as.data.frame(data_semoutliers_ks$cod)
    efficiency_semoutliers_ks$efficiency <- dea_semoutliers_ks[["eff"]]
    colnames(efficiency_semoutliers_ks) <- c("codigo","efficiency")

    x <- as.data.frame(data_semoutliers_ks_before)

    xcol = jackstrap_obj[['ycolumn']]+2

    for (p in 1:xcol) {
      x[,1] <- NULL
    }

    y <- as.data.frame(data_semoutliers_ks_before)

    for (p in 1:jackstrap_obj[['xcolumn']]) {

      col_del = jackstrap_obj[['ycolumn']]+1

      if (p == 1) {
        y[,1] <- NULL
        y[,1] <- NULL
      }

      y[,col_del] <- NULL

    }

    dea_semoutliers_ks_before <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

    efficiency_semoutliers_ks_before <- as.data.frame(data_semoutliers_ks_before$cod)
    efficiency_semoutliers_ks_before$efficiency <- dea_semoutliers_ks_before[["eff"]]
    colnames(efficiency_semoutliers_ks_before) <- c("codigo","efficiency")

    efficiency_semoutliers_ks <- merge(efficiency_semoutliers_ks, efficiency_semoutliers_ks_before, by.x = "codigo", by.y = "codigo", all.x = TRUE)
    colnames(efficiency_semoutliers_ks) <- c("code","efficiency_withoutoutlier_ks","efficiency_before")

    result_kstest_ks <- ks.test(efficiency_semoutliers_ks$efficiency_withoutoutlier_ks,efficiency_semoutliers_ks$efficiency_before)

    ks_test_rrow <- data.frame(cod_dmu_analise,result_kstest_ks[["p.value"]])
    ks_test_accrued <- rbind(ks_test_accrued, ks_test_rrow)

  }

  leverage_dmu <- data.frame(jackstrap_obj[['mean_leverage']])

  leverage_dmu_outlier <- subset(leverage_dmu, mean>quantile(leverage_dmu$mean, perc))

  leverage_dmu_outlier <- leverage_dmu_outlier[order(leverage_dmu_outlier$mean, decreasing = TRUE), ]

  leverage_dmu_outlier[,2:7]<- NULL

  n_row_dmu_leverage <- nrow(leverage_dmu_outlier)
  limit_times_takeoff <- n_row_dmu_leverage

  registerDoParallel(num_cores)

  list_dmu_out_ks <- (1:limit_times_takeoff)
  ks_test_accrued <- NULL

  results_ks <- mclapply(list_dmu_out_ks, jackstrap_ks, mc.cores = num_cores, mc.preschedule = FALSE)

  for (l in 1:limit_times_takeoff) {
    if (l == 1){
      linha_pvalue_ks <- NULL
      acum_pvalue_ks <- NULL
    }
    linha_pvalue_ks <- results_ks[[l]]
    acum_pvalue_ks <- rbind(acum_pvalue_ks, linha_pvalue_ks)
  }

  stopImplicitCluster()

    ks_test_accrued <- acum_pvalue_ks
    acum_pvalue_ks <- NULL

    colnames(ks_test_accrued) <- c("codigo","p_value")


    outlier_ks <- subset(ks_test_accrued, ks_test_accrued$p_value < 0.99)

    outlier_ks[,2] <- NULL

    data_semoutliers_ks_acum <- data_orig

    data_semoutliers_ks_acum = subset(data_semoutliers_ks_acum, !(data_semoutliers_ks_acum$cod %in% outlier_ks$codigo))

    x <- as.data.frame(data_semoutliers_ks_acum)

    xcol = jackstrap_obj[['ycolumn']]+2

    for (p in 1:xcol) {
      x[,1] <- NULL
    }

    y <- as.data.frame(data_semoutliers_ks_acum)

    for (p in 1:jackstrap_obj[['xcolumn']]) {

      col_del = jackstrap_obj[['ycolumn']]+1

      if (p == 1) {
        y[,1] <- NULL
        y[,1] <- NULL
      }

      y[,col_del] <- NULL

    }

    dea_semoutliers_ks_acum <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

    efficiency_semoutliers_ks_acum <- as.data.frame(data_semoutliers_ks_acum$cod)
    efficiency_semoutliers_ks_acum$efficiency <- dea_semoutliers_ks_acum[["eff"]]
    colnames(efficiency_semoutliers_ks_acum) <- c("codigo","efficiency")

    efficiency_semoutliers_ks_acum <- merge(efficiency_semoutliers_ks_acum, efficiency_comp_ks, by.x = "codigo", by.y = "codigo", all.x = TRUE)
    colnames(efficiency_semoutliers_ks_acum) <- c("code","efficiency_withoutoutlier","efficiency_complete")

    ks_test_accrued <- unique(ks_test_accrued)
    efficiency_semoutliers_ks_acum <- unique(efficiency_semoutliers_ks_acum)

    jackstrap_ks_obj <- jackstrap_obj
    jackstrap_ks_obj[["type_obj"]] <- 'jackstrap_ks_obj'
    jackstrap_ks_obj[["result_kstest_method"]] <- ks_test_accrued
    jackstrap_ks_obj[["efficiency_ks_method"]] <- efficiency_semoutliers_ks_acum

  return(jackstrap_ks_obj)

}


