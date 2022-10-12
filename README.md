# Summary

This R-script is inspired on the IEEE Conference Proceeding titled: 

[Cluster and calendar based visualization of time series data](https://ieeexplore.ieee.org/abstract/document/801851)
by Van Wijk, J.J. and Van Selow, E.R. (1999)

For interpretation of the final plot, please go to the Wiki home-page of this repository:

https://github.com/jorgethomasm/HOME-Timeseries_Clustering_of_Daily_Profiles/wiki 

# Clustering Method to Visualise Daily Patterns of Power Demand
### For Example: timeseries of electrical or heating power in kilowatts [kW]

Author: Jorge A. Thomas (jorgethomasm@ieee.org)

Started:   02.05.2019
Continued: 23.04.2020

## Methods: 

Unsupervised Learning:

1. Hierarchical Agglomerative Clustering (HAC), bottom up approach
2. Gaussian Kernel Density Estimation (KDE)

# Variables notation
**D:** Demand / Load e.g.: [kW]

**T:** Exogenous predictor, e.g. a timeseries of ambient temperature in degree Celsius [°C]
