# Package Jackstrap. Hello everybody!

This package applies the Jackstrap created by Sousa and Stosic (2005) Technical Efficiency of the Brazilian Municipalities: Correcting Nonparametric Frontier Mesurements For Outliers. Journal of Productivity Analysis, 24, 147-181.

The main functions are jackstrap and jackstrap_ks. 

How to install?

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("klebermsousa/jackstrap")
```

The code is an example how to use the package:

```{r}
library(jackstrap)
data(municipalities)

efficiency <- jackstrap (data=municipalities, ycolumn=2, xcolumn=1, bootstrap=1000, perc_sample_bubble=0.10, dea_method="vrs", orientation_dea="in", n_seed = 2000, repos=FALSE, num_cores=4)
```
The function jackstrap_ks is usefull to get efficiency index with criteria K-S test, as follows:

```{r}
efficiency_ks <- jackstrap_ks (data=municipalities, jackstrap_obj=efficiency, num_cores = 4, perc = 0.80)
```
It's relevant to pay attention that the package jackstrap doesn't support multicore in Windows. Besides that, the users need to organize the data in this sequence: dmu_name, cod, output variables and input variables. The field "cod"" must have the exact name "cod" and integer type in second column.

More informations you can get in manual (manual jackstrap.pdf) localized in the vignettes folder.

Enjoy it.






