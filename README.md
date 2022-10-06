---
title: "readme"
author: "jthomas"
date: "27 2 2020"
output: html_document
---
## Summary

This R-script is inspired on the IEEE Conference Proceeding titled: 
"Cluster and calendar based visualization of time series data" (2003)
by Van Wijk, J.J. and Van Selow, E.R.
Available at: https://ieeexplore.ieee.org/abstract/document/801851

# Clustering Method to Visualise Daily Patterns of Demand / Load 
# E.g.: [kW]

Author: Jorge A. Thomas

jorgethomasm@ieee.org

Started:   02.05.2019
Continued: 23.04.2020

## Methods: 

Unsupervised Learning:

1. Hierarchical Agglomerative Clustering (HAC), bottom up approach
2. Gaussian Kernel Density Estimation (KDE)

# Variables notation
**D:** Demand / Load e.g.: [kW]

**T:** Ambient Temperature or exogenous predictor e.g.: [Celsius]

