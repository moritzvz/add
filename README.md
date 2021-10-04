# ALD
This repository contains the implementation of 'Automatic Location of Disparities' (ALD) for conducting algorithmic audits.

# Run
1. Download the COMPAS or Adult Income dataset.
2. Run main.R
   1. Specify path to data file.
   2. Set notion of fairness (statistical parity or equalized odds).
   3. Set ranking type (impact or confidence)
3. Audit report including visualizations saved as PDF in working directory.

# Citation
Please consider citing us if you find this helpful for your work:
```
@misc{vonzahn2021,  
    author       = {von Zahn, Moritz and Hinz, Oliver and Feuerriegel, Stefan},  
    title        = {{A data-driven framework for automatic location of disparities in machine learning}},  
    month        = sep,  
    year         = 2021,  
    version      = {1.0},  
    url          = {https://github.com/moritzvz/ald}  
    }
 ```
