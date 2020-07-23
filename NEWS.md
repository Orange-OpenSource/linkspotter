# linkspotter

# linkspotter 1.3.0

## Computation

* add `targetVar` parameter for `multiBivariateCorrelation()` and `linkspotterComplete()` to consider only couples including `targetVar`
* add `maxNbBins` parameter for MaxNMI in `multiBivariateCorrelation()` and `linkspotterComplete()`
* print info from `multiBivariateCorrelation()`
* better deal with the parameter `includeNA`
* bug fix on `EFdiscretization()` for weird cases
* better handle non informative variables and couples of variables
* remove `tidyr` and `dplyr` deprecated functions
* give up `Hmisc` dependency

## Visualization

* add a new feature: hide isolated nodes
* add a new feature: flip the bivariate plot
* add busy spinner from `shinybusy` package
* use `ggplot2` for all bivariate plots

# linkspotter 1.2.0 (initial CRAN release)

## Computation

* Calculation of several correlation matrices corresponding to different link coefficients
* Clustering of variables using an unsupervised learning
* Supervised discretization of one or a couple of variables.

## Visualization

* Visualize the links using a graph (the variables correspond to the nodes and the links correspond to the edges)
* Show the distribution of each variable using its histogram or barplot
* Visualize a link between a couple of variables using scatter plots, box plots, etc.
