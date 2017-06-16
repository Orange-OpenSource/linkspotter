# Introduction

Linkspotter is a package of the R software that mainly allows to calculate and visualize using a graph all the bivariate links of a dataset.

Its main features are:

* Calculation of several correlation matrices corresponding to different link coefficients
* Clustering of variables using an unsupervised learning
* Supervised discretization of one or a couple of variables.

It also offers a customizable user interface, allowing to:

* visualize the links using a graph (the variables corresdond to the nodes and the links correspond to the edges)
* show the distribution of each variable using its histogram or barplot
* visualize a link between a couple of variables using scatter plots, box plots, etc.

Available link coefficients are:

* [Pearson's r](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient)
* [Spearman's rho](https://en.wikipedia.org/wiki/Spearman's_rank_correlation_coefficient)
* [Kendall's tau](https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient)
* [the Maximal Information Coefficient (MIC)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/)
* [the distance correlation](https://en.wikipedia.org/wiki/Distance_correlation)
* [the Maximal Normalized Mutual Information (MaxNMI)](https://en.wikipedia.org/wiki/Mutual_information)

# Installation

```{r, echo=TRUE, eval=FALSE}
library(devtools)
install_github("sambaala/linkspotter")
```
Behind a proxy:

```{r, echo=TRUE, eval=FALSE}
library(devtools)
library(httr)
set_config(
  use_proxy(url="<my_proxy>", port=<my_proxy_port>)
)
install_github("sambaala/linkspotter")
```

# Usage

Load the package:

```{r, echo=TRUE}
library(linkspotter)
```

Take a look at the documentation:

```{r, echo=TRUE, eval=FALSE}
help(package="linkspotter")
```

The examples are carried out using "iris" data.

## Calculate the MaxNMI between two variables

```{r, echo=TRUE}
maxNMI(iris$Sepal.Length,iris$Petal.Length)
```

## Calculate all link coefficients for all variable couples

```{r, echo=TRUE}
corCouples<-multiBivariateCorrelation(iris)
print(corCouples)
```

## Extract a correlation matrix from the correlation dataframe

The Pearson correlation matrix:

```{r, echo=TRUE}
corMatrixPearson<-corCouplesToMatrix(x1_x2_val = corCouples[,c('X1','X2',"pearson")])
print(corMatrixPearson)
```

The MaxNMI matrix:

```{r, echo=TRUE}
corMatrixMaxNMI<-corCouplesToMatrix(x1_x2_val = corCouples[,c('X1','X2',"MaxNMI")])
print(corMatrixMaxNMI)
```

## Clustering of variables using a correlation matrix

```{r, echo=TRUE}
cl<-clusterVariables(correlationMatrix = corMatrixMaxNMI)
print(cl)
```

## Visualize the graph using Pearson correlation

```{r, echo=TRUE}
linkspotterGraph(corDF = corCouples, variablesClustering = cl, corMethod = "pearson", minCor = 0.25, smoothEdges = FALSE, dynamicNodes = FALSE)
```

## Visualize the graph using MaxNMI

```{r, echo=TRUE}
linkspotterGraph(corDF = corCouples, variablesClustering = cl, corMethod = "MaxNMI", minCor = 0.25, smoothEdges = F, dynamicNodes = TRUE)
```

## Launch the costumizable user interface

```{r, echo=TRUE, eval=FALSE}
linkspotterUI(dataset = iris, corDF = corCouples, variablesClustering = cl, appTitle = "Linkspotter example")
```

## Additional features

Complete Linkspotter computation:

```{r, echo=TRUE}
lsiris<-linkspotterComplete(iris)
```

Complete Linkspotter computation from an external file:

```{r, echo=TRUE, eval=FALSE}
lsiris<-linkspotterOnFile("iris.csv")
summary(lsiris)
```

```{r, echo=TRUE}
summary(lsiris)
```

Then launch the user interface using:

```{r, echo=TRUE, eval=FALSE}
lsiris$run_it
```

Help:

```{r, echo=TRUE, eval=FALSE}
help(linkspotterComplete)
```

# User interface guide

[Linskpotter UI example on 'iris' data](http://linkspotteririsexample.sigmant.net)

## 'Graphs' tab

### The graph
The variables corresdond to the nodes and their links correspond to the edges.
Node color depends on the clustering. Edge color depends on the correlation direction quantitative couples (blue: positive correlatuion, red: negative correlation).

### First features

* **Correlation coefficient** drop-down list : allows to choose the link coefficient to be visualized.
* **Minimum Correlation** cursor: allows to define the minimum link measurement (from 0 to 1) threshold necessary to plot a edge.
* **Interest variable** drop-down list: allows to choose a variable of interest. If a variable of interest is selected, a new cursor named **Minimum Correlation with interest variable** appears. It gives the possibility of defining a new minimum threshold of correlation specific to this variable of interest, in addition to the general threshold. The upper limit of this specific threshold is the global threshold.

### Checkboxes

* **Highlight variable on click**: if checked, it is possible to focus on a node, by clicking on it or by selecting it from the drop-down list that appears.
* **Variable Clustering**: If checked, the nodes are colored according to the group to which they belong. If it is not checked, all nodes are blue.
* **Color Edges by correlation direction**: if checked, the edges are colored according to the direction of the correlation (blue for a positive correlation, red for a negative correlation and gray for NA) found between the two variables. The NA case corresponds to when at least one of the two variables is qualitative.
* **Smooth edges**: if checked, edges are allowed to bend as needed. Otherwise, they remain straight.
* **Dynamic nodes stabilization**: if checked, the graph is repositioned according to the graph stabilization algorithm after each movement by the user.
* **Re-stabilize**: button that allows to re-stabilize the graph (centering and redistribution of the nodes on plane in "almost optimal" way)

### General information

* **nb. observations**: the number of entries in the dataset
* **nb. variables**: the number of variables in the dataset
* **nb. couples**: the number of couples
* **nb. current edges**: the current number of edges plotted according to the chosen thresolds

### Click on a graph link

It produces the following:

#### Link summary figure (bottom right of the graph)

Its type depends on the nature of the corresponding link:

* Quantitative variable vs quantitative variable: a point cloud
* Variable quantitative vs qualitative variable: a boxplot
* Qualitative variable vs qualitative variable: not yet treated

#### Link summary table (under general information)

It displays all the measurements calculated for the link corresponding to the clicked edge. When at least one of the variables is qualitative, only the MaxNMI has a value.

### Click on a node of the graph

It produces the following:

#### A summary figure for the corresponding variable (bottom left of the graph)

Its type depends on the nature of the corresponding variable:

* Quantitative variable: a histogram
* Qualitative variable: a bar graph

#### A summary table for the corresponding variable (under general information)

Its type depends on the nature of the variable:

* Quantitative variable: a table containing the min, 1st quartile, median, mean, 3rd quartile and max of the variable.
* Qualitative variable: a table containing the frequency of each modality of the variable.


## 'Tables' tab

This tab displays 2 tables:

* A correlation matrix corresponding to the selected correlation coefficient
* A table indicating the group which each variable is assigned to after by the clustering.

The **Correlation coefficient** option allows you to choose the coefficient of correlation to be considered among those calculated initially.


# Sources

Linkspotter uses and combine features coming from several other R packages, namely infotheo, minerva, energy, mclust, shiny, visNetwork et rAmCharts.
