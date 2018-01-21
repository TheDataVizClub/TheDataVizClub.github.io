---
layout: single
title: Clustered vs. Correlation Matrix
excerpt: How to build correlation matrix
author: Michelle
header:
  teaser: /assets/images/SamplePlots/ClusteredCorrelationMatrix.jpeg
---


View the Data: 

![Landsat Data](/assets/RMDFiles/Landsat.txt){: .btn .btn--info }


Data transformations:

``` r
library(corrplot) # corrplot()
landsat = read.table("Landsat.txt",header=TRUE)
classlabel = as.factor(landsat[,37])
land.data = landsat[,-37]

S = cov(land.data)
R = cov2cor(S)
```

Correlation Matrix:
===================

Plotting Using Corrplot package:

``` r
corrplot::corrplot(R,tl.cex=0.7,cl.cex=0.7,
	mar=c(0,1,1.5,2.5),title="Correlation Matrix")
```

![](/assets/RMDFiles/Correlation_Plots_and_Logistic_Regression_files/figure-markdown_github/unnamed-chunk-3-1.png)

Clustered Correlation Matrix:
===================

``` r
corrplot::corrplot(R,order="hclust",tl.cex=0.7,
				   cl.cex=0.7,mar=c(0,1,1.5,2.5),
                   title="Clustered Correlation Matrix")
```

![](/assets/RMDFiles/Correlation_Plots_and_Logistic_Regression_files/figure-markdown_github/unnamed-chunk-4-1.png)






