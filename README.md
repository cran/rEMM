# Extensible Markov Model for Modelling Temporal Relationships Between Clusters

[![CRAN
version](https://www.r-pkg.org/badges/version/rEMM)](https://cran.r-project.org/package=rEMM)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/rEMM)](https://cran.r-project.org/package=rEMM)

Implements TRACDS (Temporal Relationships 
    between Clusters for Data Streams), a generalization of 
    Extensible Markov Model (EMM). TRACDS adds a temporal or order model
    to data stream clustering by superimposing a dynamically adapting
    Markov Chain. Also provides an implementation of EMM (TRACDS on top of tNN 
    data stream clustering). 

## Installation

**Stable CRAN version:** install from within R with

``` r
install.packages("rEMM")
```

**Current development version:** install from GitHub (needs devtools and
[Rtools for Windows](https://cran.r-project.org/bin/windows/Rtools/)).

``` r
devtools::install_github("mhahsler/rEMM")
```

## Usage

See Examples section in the paper [rEMM: Extensible Markov model for data stream clustering in R.](http://dx.doi.org/10.18637/jss.v035.i05).

    
# References
* Michael Hahsler and Margaret H. Dunham. [rEMM: Extensible Markov model for data stream clustering in R.](http://dx.doi.org/10.18637/jss.v035.i05) _Journal of Statistical Software,_ 35(5):1-31, 2010.
* Michael Hahsler and Margaret H. Dunham. [Temporal structure learning for clustering massive data streams in real-time](https://doi.org/10.1137/1.9781611972818.57). In _SIAM Conference on Data Mining (SDM11),_ pages 664--675. SIAM, April 2011.

# Acknowledgements
    
Development of this 
    package was supported in part by NSF IIS-0948893 and R21HG005912 from 
    the National Human Genome Research Institute.
