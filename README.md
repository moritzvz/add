# ALD
This repository contains the implementation of 'Automatic Location of Disparities' (ALD) for conducting algorithmic audits.

# Run
1. Run main.R
   1. Specify dataset (default: preprocessed COMPAS dataset).
   2. Select set of sensitive attributes to consider (default: 'race').
   3. Set notion of fairness (default: 'equalized odds').
   4. Set ranking type (default: 'confidence')
3. Audit report including visualizations saved as PDF in output folder.

# Citation
Please consider citing us if you find this helpful for your work:
```
@misc{vonZahn.2022,  
    author       = {von Zahn, Moritz and Hinz, Oliver and Feuerriegel, Stefan},  
    title        = {Locating disparities in machine learning},
    year         = 2022,  
    url          = {https://github.com/moritzvz/ald}  
    }
 ```
