# autoclust 

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![GitHub release](https://img.shields.io/github/v/release/Anacristina0914/autoclust)](https://github.com/Anacristina0914/autoclust/releases)  
[![DOI](https://zenodo.org/badge/887137273.svg)](https://doi.org/10.5281/zenodo.17569306)



> Automated clustering and association testing for patient stratification using antibody, proteomic, and clinical data.

---

## Overview

`autoclust` is an R package designed for **unsupervised clustering** and **association testing** in heterogeneous biomedical datasets.  
It supports:  
- Clustering with Gower distance, PAM, and other algorithms  
- Cluster evaluation via silhouette, bootstrap stability, and consensus metrics  
- Association testing with clinical variables using frequentist or Bayesian logistic regression  
- Visualization tools for cluster separation, stability, and clinical effect sizes  

Originally developed for patient stratification in **Systemic Lupus Erythematosus (SLE)** cohorts, `autoclust` is applicable to any multi-feature biomedical dataset with mixed data types.

---

## Installation

You can install the latest development version directly from GitHub:

```
# Install devtools if you haven't already
install.packages("devtools")

# Install autoclust
devtools::install_github("Anacristina0914/autoclust")
```
