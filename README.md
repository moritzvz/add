# ALD

This repository contains the implementation of 'Automatic Location of Disparities' (ALD) for conducting algorithmic audits.

# Installation 

```
# install.packages("remotes")
remotes::install_github("https://github.com/moritzvz/ald")
 ```

ALD is dependent on several other packages for handling data, modeling, and generating reports: partykit, assertthat, magrittr, tidyselect, tibble, dplyr, tidyr, readr, rmarkdown, flextable, stringr, ggplot2, ggparty, cowplot, scales, hms

# Usage

The ALD audit:

- is performed on a dataset of your choice that must be provided as a .csv file
- requires notion of fairness to be set to 'statistical parity' or 'equalized odds'
 + in case of 'statistical parity' you must set the outcome_variable argument to the name of the outcome variable in your dataset
 + for 'equalized odds' you must set the prediction_variable and ground_truth_variable arguments to the names of the prediction and ground truth variables in your dataset
- by default all other variables (not outcome, prediction, ground truth) in you dataset will be used as sensitive attributes in the audit. You can use the sensitive_attributes argument to specifically set the sensitive attributes to a subset of your dataset varaiables
- requires a ranking mechanism which must be 'confidence' or 'magnitude'
- requires a maximum number of groups in the report (n_grp)
- requires a number of trees to model in partykit::cforest (ntree)
- requires a alpha argument passed to partykit::cforest (alpha)
- optionally takes a p-value adjustment method to pass to stats::p.adjust (adjust_method), either "BH" (Benjamini-Hochberg, by default) or "bonferroni" (Bonferroni correction).
- optionally takes a random seed number that can be used for reproducibility of results
- writes a report to the directory that you set with the dir argument, with data_name argument used in the name

```
# for example
ald_audit(
  file                  = "my_data.csv",
  prediction_variable   = "prediction",
  ground_truth_variable = "ground_truth",
  notion_of_fairness    = "equalized odds",
  ranking_mechanism     = "confidence",
  data_name             = "data_title",
  dir                   = here::here(""),
  n_grp                 = 3,
  ntree                 = 25,
  alpha                 = 0.1)

ald_audit(
  file                  = "my_data.csv",
  outcome_variable      = "outcome",
  notion_of_fairness    = "statistical parity",
  ranking_mechanism     = "confidence",
  data_name             = "data_title",
  dir                   = here::here(""),
  n_grp                 = 3,
  ntree                 = 25,
  alpha                 = 0.1)
 ```


# Citation
Please consider citing us if you find this helpful for your work:
```
@misc{vonZahn.2023,  
    author       = {von Zahn, Moritz and Hinz, Oliver and Feuerriegel, Stefan},  
    title        = {Locating disparities in machine learning},
    year         = 2022,  
    url          = {https://github.com/moritzvz/ald}  
    }
 ```
