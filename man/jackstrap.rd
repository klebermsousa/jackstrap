% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jackstrap.R
\name{jackstrap}
\alias{jackstrap}
\title{Jackstrap Method: Tool identifies outliers in Nonparametric Frontier.
This function applies the developed technique by Sousa and Stosic (2005)
Technical Efficiency of the Brazilian Municipalities: Correcting Nonparametric
Frontier Measurements for Outliers.}
\usage{
jackstrap(
  data,
  ycolumn,
  xcolumn,
  bootstrap = 1000,
  perc_sample_bubble = 0.1,
  dea_method = "vrs",
  orientation_dea = "in",
  n_seed = NULL,
  repos = FALSE,
  num_cores = 1
)
}
\arguments{
\item{data}{is the dataset with input and output used to measure efficiency; Dataset need to have this form: 1th column: name of DMU (string); 2th column: code of DMU (integer); n columns of output variables; n columns of input variables.}

\item{ycolumn}{is the quantity of y columns of dataset.}

\item{xcolumn}{is the quantity of x columns of dataset.}

\item{bootstrap}{is the quantity of applied resampling.}

\item{perc_sample_bubble}{is the percentage of sample in each bubble.}

\item{dea_method}{is the dea method: "crs" is DEA with constant returns to scale (CCR); "vrs" is DEA with variable returns to scale; and "fdh" is Free Disposal Hull (FDH) with variable returns to scale.}

\item{orientation_dea}{is the direction of the DEA: "in" for focus on inputs; and "out" for focus on outputs.}

\item{n_seed}{is the code as seed used to get new random samples.}

\item{repos}{identify if the resampling method is with reposition TRUE or not FALSE.}

\item{num_cores}{is the number of cores available to process.}
}
\value{
Return the jackstrap object with information as follows: "mean_leverage" is leverage average for each DMU;
"mean_geral_leverage" is general average of leverage and step function threshold;
"sum_leverage" is accrued leverage on all resampling for each DMU; "count_dmu" is amount of each DMU was selected by bootstrap.
"count_dmu_zero" is amount of each DMU was selected by bootstrap but it did not influence in others. "ycolumn" is the number of output variables;
"xcolumn" is the number of input variables; "perc_sample_bubble" is the percentage of sample used in each bubble;"dea_method" is the model used in DEA analysis;
"orientation_dea" is the orientation of DEA; ""bootstrap" is the amount of bubble used by jackstrap method;
"type_obj" is type of object; "size_bubble" is the amount of DMU used in each bubble.
}
\description{
Jackstrap Method: Tool identifies outliers in Nonparametric Frontier.
This function applies the developed technique by Sousa and Stosic (2005)
Technical Efficiency of the Brazilian Municipalities: Correcting Nonparametric
Frontier Measurements for Outliers.
}
\examples{
 \dontshow{
   library(jackstrap)
   test_data <- data.frame(mun=c(1:10), cod=c(1:10), y=c(5,7,6,7,4,6,8,9,3,1),
                           x=c(7,8,10,22,15,7,22,17,10,5))
   effic_test <- jackstrap (data=test_data, ycolumn=1, xcolumn=1, bootstrap=1,
                 perc_sample_bubble=1, dea_method="crs", orientation_dea="in",
                 n_seed = 2000, repos=FALSE, num_cores=1)
 }
 \donttest{
    # Examples with the municipalities data.
    #Load package jackstrap
    library(jackstrap)

    #Load data example
    municipalities <- jackstrap::municipalities

    #Command measures efficiency with jackstrap method and heaviside criterion
    efficiency <- jackstrap (data=municipalities, ycolumn=2, xcolumn=1, bootstrap=1000,
                      perc_sample_bubble=0.20, dea_method="vrs", orientation_dea="in",
                      n_seed = 2000, repos=FALSE, num_cores=4)
 }
}
