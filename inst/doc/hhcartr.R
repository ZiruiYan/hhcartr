## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(bookdown)
knitr::opts_chunk$set(echo = TRUE)

## ----include = FALSE----------------------------------------------------------
library(captioner)
fig_nums <- captioner(prefix = "Figure")
fig_nums("figure1", "__View Sample Output__. Use the View command to navigate the tree objects returned by the __fit()__ command.")
fig_nums("figure2", "The householder matrix.")

table_nums <- captioner(prefix = "Table")
table_nums("table1", "Default parameters for model HHDecisionTreeClassifier.")
table_nums("table2", "Overview of the datasets used by @Wickramarachchi+Robertson+Reale+Price+Brown:2019 used in the experiments detailed below. They have all been downloaded from the UCI repository [@Newman+Asuncion:2007].")
table_nums("table3", "Overview of the datasets from @Yang+Shen+Gao:2019 used in the experiments detailed below. They have all been downloaded from the UCI repository [@Newman+Asuncion:2007].")
table_nums("table4", "Default parameters for model HHDecisionTreeRegressor.")
table_nums("table5", "hhcartr: Tree Node variables and descriptions.")

## ----geom1, echo=FALSE, out.width = "95%", fig.cap = "Geometry of a Householder Reflection."----
knitr::include_graphics("geometry1.jpg")

## ----library, echo=TRUE-------------------------------------------------------
library("hhcartr")
library("ggplot2")

## ----loaddata, echo=TRUE------------------------------------------------------
data("cancer", package = "hhcartr")
X         <- segment$X
y         <- segment$y
test_data <- segment$test_data

## ----eda1, echo=TRUE----------------------------------------------------------
dim(X)
names(X)

## ----eda2, echo=TRUE----------------------------------------------------------
table(y)

## ----model1, echo=TRUE--------------------------------------------------------
model <- HHDecisionTree(n_folds=10, min_node_impurity = 0.22)

## ----setDataDescription, echo=TRUE--------------------------------------------
setDataDescription("Segmentation")

## ----setseed, echo=TRUE-------------------------------------------------------
set.seed(2020)

## ----modelfit, echo=TRUE------------------------------------------------------
model.output <- model$fit(X, y)

## ----results1, echo=TRUE------------------------------------------------------
res <- results(model.output)

## ----accuracy1, echo=TRUE-----------------------------------------------------
res$accuracy()

## ----margin1, echo=TRUE-------------------------------------------------------
res$margin()

## ----print1, echo=TRUE, fig.align="center", fig.width = 8, fig.height = 6, fig.cap = "Classification accuracy for 10-folds after training on the Segmentation dataset."----
print(model.output)

## ----predict1, echo=TRUE------------------------------------------------------
preds <- predict(model.output, test_data = test_data)

## ----accuracy1b, echo=TRUE----------------------------------------------------
preds$accuracy()

## ----predictions1a, echo=TRUE-------------------------------------------------
options(max.print=50)
preds$predictions()

## ----grviz1, echo=TRUE, fig.align="center", fig.width = 7, fig.height = 6, fig.cap = "Plot of Tree 1 from the HHDecisionTreeClassifier model trained on the Segmentation dataset."----
library(DiagrammeRsvg)
library(rsvg)
outp <- displayTree(1)
grViz(unlist(outp))

