#' Jackstrap Method: Tool identifies outliers in Nonparametric Frontier.
#' This function applies the developed technique by Sousa and Stosic (2005)
#' Technical Efficiency of the Brazilian Municipalities: Correcting Nonparametric
#' Frontier Measurements for Outliers.
#'@param data is the dataset with input and output used to measure efficiency; Dataset need to have this form: 1th column: name of DMU (string); 2th column: code of DMU (integer); n columns of output variables; n columns of input variables.
#'@param ycolumn is the quantity of y columns of dataset.
#'@param xcolumn is the quantity of x columns of dataset.
#'@param bootstrap is the quantity of applied resampling.
#'@param perc_sample_bubble is the percentage of sample in each bubble.
#'@param dea_method is the dea method: "crs" is DEA with constant returns to scale (CCR); "vrs" is DEA with variable returns to scale; and "fdh" is Free Disposal Hull (FDH) with variable returns to scale.
#'@param orientation_dea is the direction of the DEA: "in" for focus on inputs; and "out" for focus on outputs.
#'@param n_seed is the code as seed used to get new random samples.
#'@param repos identify if the resampling method is with reposition TRUE or not FALSE.
#'@param num_cores is the number of cores available to process.
#'@return Return the jackstrap object with information as follows: "mean_leverage" is leverage average for each DMU;
#'"mean_geral_leverage" is general average of leverage and step function threshold;
#'"sum_leverage" is accrued leverage on all resampling for each DMU; "count_dmu" is amount of each DMU was selected by bootstrap.
#'"count_dmu_zero" is amount of each DMU was selected by bootstrap but it did not influence in others. "ycolumn" is the number of output variables;
#'"xcolumn" is the number of input variables; "perc_sample_bubble" is the percentage of sample used in each bubble;"dea_method" is the model used in DEA analysis;
#'"orientation_dea" is the orientation of DEA; ""bootstrap" is the amount of bubble used by jackstrap method;
#'"type_obj" is type of object; "size_bubble" is the amount of DMU used in each bubble.
#'@examples
#'  \dontshow{
#'    library(jackstrap)
#'    test_data <- data.frame(mun=c(1:10), cod=c(1:10), y=c(5,7,6,7,4,6,8,9,3,1),
#'                            x=c(7,8,10,22,15,7,22,17,10,5))
#'    effic_test <- jackstrap (data=test_data, ycolumn=1, xcolumn=1, bootstrap=1,
#'                  perc_sample_bubble=1, dea_method="crs", orientation_dea="in",
#'                  n_seed = 2000, repos=FALSE, num_cores=1)
#'  }
#'  \donttest{
#'     # Examples with the municipalities data.
#'     #Load package jackstrap
#'     library(jackstrap)
#'
#'     #Load data example
#'     municipalities <- jackstrap::municipalities
#'
#'     #Command measures efficiency with jackstrap method and heaviside criterion
#'     efficiency <- jackstrap (data=municipalities, ycolumn=2, xcolumn=1, bootstrap=1000,
#'                       perc_sample_bubble=0.20, dea_method="vrs", orientation_dea="in",
#'                       n_seed = 2000, repos=FALSE, num_cores=4)
#'  }
#'@importFrom Benchmarking dea
#'@importFrom dplyr arrange
#'@importFrom doParallel registerDoParallel
#'@importFrom foreach registerDoSEQ
#'@importFrom stats runif
#'@importFrom graphics hist
#'@importFrom foreach %do%
#'@importFrom foreach foreach
#'@importFrom parallel mclapply
#'@importFrom stats ks.test
#'@importFrom dplyr select
#'@importFrom dplyr %>%
#'@importFrom dplyr everything
#'@importFrom dplyr summarise
#'@importFrom dplyr group_by
#'@importFrom plyr desc
#'@importFrom dplyr n
#'@importFrom rlang .data
#'@importFrom doParallel stopImplicitCluster
#'@export
jackstrap <- function(data, ycolumn, xcolumn, bootstrap=1000, perc_sample_bubble=0.1, dea_method='vrs', orientation_dea='in', n_seed=NULL, repos=FALSE, num_cores=1) {

  if (is.null(n_seed)) {

    n_seed <- trunc(runif(n=1, min = 0, max = 1000000))
  }

  dataset_orig <- data

  dataset_orig <- arrange(dataset_orig,dataset_orig$cod)

  n_row <- nrow(dataset_orig)

  bubble = bootstrap
  perc_bubble = perc_sample_bubble

  method_dea = dea_method
  orient_dea = orientation_dea

  size_bubble = n_row*perc_bubble

  n_column_data <- ncol(dataset_orig)

  test_coln = ycolumn+xcolumn+2

  data_system <- Sys.info()
  op_system <- data_system[1]

  if (op_system=="Windows" & num_cores>1) {
    num_cores=1
    warning("The process will work with just one core. The package Jackstrap does not support multicore in Windows.")
  }

  if (ycolumn == 0) {
    stop("Quantity of y columns is zero! Impossible to run the jackstrap. Please checking parameters.")
  }

  if (xcolumn == 0){
    stop("Quantity of x columns is zero! Impossible to run the jackstrap. Please checking parameters.")
  }

  if (n_column_data!=test_coln){
    stop("Quantities of specified columns differ in the database! Please checking parameters.")
  }

  bubble_jacknife <- function(dmu_out) {

    i <- dmu_out

    serial <- serial_bubble

    if (i == 1) {
      coef_effic <- NULL
      coef_effic_base <- NULL
      coef_effic <- as.data.frame(serial)
      colnames(coef_effic) <- c("serial")
      coef_effic_base <- as.data.frame(serial)
      colnames(coef_effic_base) <- c("serial")
      coef_effic <- arrange(coef_effic,coef_effic$serial)
      coef_effic_base <- arrange(coef_effic_base,coef_effic_base$serial)
      row_leverage <- NULL

    }

    x <- as.data.frame(dataset)

    xcol = ycolumn+2

    for (p in 1:xcol) {
      x[,1] <- NULL
    }

    y <- as.data.frame(dataset)

    for (p in 1:xcolumn) {

      col_del = ycolumn+1

      if (p == 1) {
        y[,1] <- NULL
        y[,1] <- NULL
      }

      y[,col_del] <- NULL

    }

    dmu <- serial[i,]

    x <- x[-i,]
    y <- y[-i,]
    serial_bubble_int <- as.data.frame(serial[-i,])

    dea_prov <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

    effic_prov <- data.frame(id=serial_bubble_int,efficiency = dea_prov[["eff"]])
    colnames(effic_prov) <- c("id", "efic_prov")
    if (orient_dea == "out") {
      effic_prov$efic_prov <- 1/effic_prov$efic_prov
    }

    effic_base_serial <-  as.data.frame(dea_base[["eff"]])
    if (orient_dea == "out") {
      effic_base_serial$`dea_base[["eff"]]` <- 1/effic_base_serial$`dea_base[["eff"]]`
    }
    effic_base_serial <- effic_base_serial[-i,]

    lambda_dmu_analise <- data.frame(dataset$cod,dea_base[["lambda"]][,i])
    colnames(lambda_dmu_analise) <- c("coddmu","lambda")
    lambda_dmu_analise <- lambda_dmu_analise[-i,]

    coef_prov_base <- cbind(effic_prov, effic_base_serial)

    coef_prov_base$dif_base_prov <- coef_prov_base$efic_prov-coef_prov_base$effic_base_serial

    coef_prov_base$dif_base_prov2 <- coef_prov_base$dif_base_prov*coef_prov_base$dif_base_prov

    coef_prov_base$dif_base_prov_abs <- abs(coef_prov_base$dif_base_prov)

    coef_prov_base <- merge(coef_prov_base, lambda_dmu_analise, by.x=c("id"), by.y=c("coddmu"), all=TRUE)

    coef_prov_base$dif_prov_pond_lambda <- (coef_prov_base$dif_base_prov_abs*coef_prov_base$lambda)

    res_coef_prov_base <- subset(coef_prov_base, subset=lambda>0)

    mean_leverage_pond <- mean(res_coef_prov_base$dif_prov_pond_lambda)

    sum_dif_effic <- sum(coef_prov_base$dif_base_prov2)

    leverage_unit <- sqrt((sum_dif_effic/(n_row_bubble-1)))

    max_dif_eff <- max(coef_prov_base$dif_base_prov)
    min_dif_eff <- min(coef_prov_base$dif_base_prov)

    cod_dmu_ausent <- paste(dmu)

    row_leverage <- data.frame(coddmu=cod_dmu_ausent, leverage_unit = leverage_unit, max_dif_eff=max_dif_eff, min_dif_eff=min_dif_eff, mean_leverage_pond=mean_leverage_pond)

    if (i == 1){
      row_leverage_acum_knife <- NULL
    }

    row_leverage_acum_knife <- rbind(row_leverage_acum_knife, row_leverage)

  }

  x <- as.data.frame(dataset_orig)

  xcol = ycolumn+2

  for (p in 1:xcol) {
    x[,1] <- NULL
  }

  y <- as.data.frame(dataset_orig)

  for (p in 1:xcolumn) {

    col_del = ycolumn+1

    if (p == 1) {
      y[,1] <- NULL
      y[,1] <- NULL
    }

    y[,col_del] <- NULL

  }

  dea_comp <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

  efficiency_comp <- as.data.frame(dataset_orig$cod)
  efficiency_comp$efficiency <- dea_comp[["eff"]]
  if (orient_dea == "out") {
    efficiency_comp$efficiency <- 1/efficiency_comp$efficiency
  }
  colnames(efficiency_comp) <- c("codigo","efficiency")

  registerDoParallel(num_cores)

  set.seed(n_seed)
  list_seed <- trunc(runif(n=bubble, min = 0, max = 1000000))

  foreach (b=1:bubble) %do% {

    if (b == 1) {
      total_levarage <- NULL
      contagem_bubble_dmu <- NULL
    }

    set.seed(list_seed[b])
    dataset <- dataset_orig[sample(nrow(dataset_orig),size_bubble, replace = repos),]

    dataset <- arrange(dataset,dataset$cod)

    x <- as.data.frame(dataset)

    xcol = ycolumn+2

    for (p in 1:xcol) {
      x[,1] <- NULL
    }

    y <- as.data.frame(dataset)

    for (p in 1:xcolumn) {

      col_del = ycolumn+1

      if (p == 1) {
        y[,1] <- NULL
        y[,1] <- NULL
      }

      y[,col_del] <- NULL

    }

    dea_base <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

    n_row_bubble <- nrow(dataset)

    serial_bubble  <- as.data.frame(dataset$cod)

    list_dmu_out <- (1:n_row_bubble)

    if (b == 1){
      row_leverage_acum <- NULL
      leverage_acum_bubble <- NULL
    }

    row_leverage_acum_knife <- NULL
    results <- mclapply(list_dmu_out, bubble_jacknife, mc.cores = num_cores)

    for (l in 1:n_row_bubble) {
      if (l == 1){
        leverage_acum_bubble <- NULL
      }
      row_leverage_bubble <- results[[l]]
      leverage_acum_bubble <- rbind(leverage_acum_bubble, row_leverage_bubble)
    }

    row_leverage_acum <- rbind(row_leverage_acum, leverage_acum_bubble)

  }

  stopImplicitCluster()

  sum_leverage <- as.data.frame(row_leverage_acum %>%
                                  group_by(coddmu) %>%
                                  summarise(soma = sum(leverage_unit)))
  sum_leverage$coddmu <- as.numeric(as.character(sum_leverage$coddmu))
  sum_leverage <- arrange(sum_leverage, sum_leverage$coddmu)

  sum_contagem_dmu <- as.data.frame(row_leverage_acum %>%
                                      group_by(coddmu) %>%
                                      summarise(sum = n()))
  sum_contagem_dmu$coddmu <- as.numeric(as.character(sum_contagem_dmu$coddmu))
  sum_contagem_dmu <- arrange(sum_contagem_dmu, sum_contagem_dmu$coddmu)

  row_leverage_acum_filter <- subset(row_leverage_acum, leverage_unit==0)

  sum_contagem_dmu_zero <- as.data.frame(row_leverage_acum_filter %>%
                                           group_by(coddmu) %>%
                                           summarise(sum = n()))
  sum_contagem_dmu_zero$coddmu <- as.numeric(as.character(sum_contagem_dmu_zero$coddmu))
  sum_contagem_dmu_zero <- arrange(sum_contagem_dmu_zero, sum_contagem_dmu_zero$coddmu)

  max_dif_eff_mean <- as.data.frame(row_leverage_acum %>%
                                   group_by(coddmu) %>%
                                   summarise(max_dif_eff = mean(max_dif_eff)))
  max_dif_eff_mean$coddmu <- as.numeric(as.character(max_dif_eff_mean$coddmu))
  max_dif_eff_mean$max_dif_eff <- as.numeric(max_dif_eff_mean$max_dif_eff)
  max_dif_eff_mean <- max_dif_eff_mean[order(max_dif_eff_mean$max_dif_eff, decreasing = TRUE), ]

  min_dif_eff_mean <- as.data.frame(row_leverage_acum %>%
                                       group_by(coddmu) %>%
                                       summarise(min_dif_eff = mean(min_dif_eff)))
  min_dif_eff_mean$coddmu <- as.numeric(as.character(min_dif_eff_mean$coddmu))
  min_dif_eff_mean <- min_dif_eff_mean[order(min_dif_eff_mean$min_dif_eff, decreasing = TRUE), ]

  mean_leverage_pond_mean <- as.data.frame(row_leverage_acum %>%
                                      group_by(coddmu) %>%
                                      summarise(mean_leverage_pond = mean(mean_leverage_pond)))
  mean_leverage_pond_mean$coddmu <- as.numeric(as.character(mean_leverage_pond_mean$coddmu))
  mean_leverage_pond_mean <- mean_leverage_pond_mean[order(mean_leverage_pond_mean$mean_leverage_pond, decreasing = TRUE), ]

  mean_leverage <- as.data.frame(row_leverage_acum %>%
                                   group_by(coddmu) %>%
                                   summarise(mean = mean(leverage_unit)))
  mean_leverage$coddmu <- as.numeric(as.character(mean_leverage$coddmu))
  mean_leverage <- arrange(mean_leverage, mean_leverage$coddmu)

  mean_leverage <- merge(mean_leverage, sum_contagem_dmu_zero, by.x=c("coddmu"), by.y=c("coddmu"), all=TRUE)
  mean_leverage <- merge(mean_leverage, sum_contagem_dmu, by.x=c("coddmu"), by.y=c("coddmu"), all=TRUE)
  colnames(mean_leverage) <- c("coddmu","mean_base","hitzero","hittotal")

  mean_leverage$hitzero[is.na(mean_leverage$hitzero)] <- 0
  mean_leverage$hittotal[is.na(mean_leverage$hittotal)] <- 0

  mean_leverage$hitnzero <- mean_leverage$hittotal-mean_leverage$hitzero

  mean_leverage$mean <- mean_leverage$mean_base*((mean_leverage$hitnzero)/mean_leverage$hittotal)

  mean_leverage <- mean_leverage %>% select(coddmu, mean, everything())

  mean_leverage_general <- as.data.frame(mean_leverage %>%
                                           summarise(mean = mean(mean)))

  mean_leverage_general$llogk <- mean_leverage_general[1,1]*(log(n_row))
  limite_crit_log <- mean_leverage_general[1,2]

  mean_leverage$outlier <- 1
  mean_leverage$outlier[mean_leverage$mean>limite_crit_log] <- 0

  mean_leverage <- mean_leverage %>% select(coddmu, mean, outlier, mean_base, hitnzero, everything())

  dmu_out = subset(mean_leverage, outlier==0)

  dataset_semoutliers <- dataset_orig

  dataset_semoutliers = subset(dataset_semoutliers, !(dataset_semoutliers$cod %in% dmu_out$coddmu))

  x <- as.data.frame(dataset_semoutliers)

  xcol = ycolumn+2

  for (p in 1:xcol) {
    x[,1] <- NULL
  }

  y <- as.data.frame(dataset_semoutliers)

  for (p in 1:xcolumn) {

    col_del = ycolumn+1

    if (p == 1) {
      y[,1] <- NULL
      y[,1] <- NULL
    }

    y[,col_del] <- NULL

  }

  dea_semoutliers <- dea(x,y, RTS=method_dea, ORIENTATION=orient_dea)

  efficiency_semoutliers <- as.data.frame(dataset_semoutliers$cod)
  efficiency_semoutliers$efficiency <- dea_semoutliers[["eff"]]
  colnames(efficiency_semoutliers) <- c("codigo","efficiency")
  if (orient_dea == "out") {
    efficiency_comp$efficiency <- 1/efficiency_comp$efficiency
  }


  efficiency_semoutliers <- merge(efficiency_semoutliers, efficiency_comp, by.x = "codigo", by.y = "codigo", all.x = TRUE)
  colnames(efficiency_semoutliers) <- c("code","efficiency_withoutoutlier","efficiency_complete")

  mean_leverage <- arrange(mean_leverage, desc(mean))

  jackstrap_obj <- NULL
  jackstrap_obj[["mean_leverage"]] <- mean_leverage
  jackstrap_obj[["mean_geral_leverage"]] <- mean_leverage_general
  jackstrap_obj[["sum_leverage"]] <- sum_leverage
  jackstrap_obj[["count_dmu"]] <- sum_contagem_dmu
  jackstrap_obj[["count_dmu_zero"]] <- sum_contagem_dmu_zero
  jackstrap_obj[["efficiency_step_func"]] <- efficiency_semoutliers
  jackstrap_obj[["efficiency_comp"]] <- efficiency_comp
  jackstrap_obj[["ycolumn"]] <- ycolumn
  jackstrap_obj[["xcolumn"]] <- xcolumn
  jackstrap_obj[["perc_sample_bubble"]] <- perc_bubble
  jackstrap_obj[["dea_method"]] <- dea_method
  jackstrap_obj[["orientation_dea"]] <- orientation_dea
  jackstrap_obj[["bootstrap"]] <- bubble
  jackstrap_obj[["type_obj"]] <- 'jackstrap_obj'
  jackstrap_obj[["size_bubble"]] <- size_bubble

  return(jackstrap_obj)

}


