# MVPapp 0.2.10 (2024-11-28)

## Features

* Less strict requirements for Data Exploration plots to only require "ID" column to be present (previously would require "ID", "DV", and "TIME").  
* Interactive plot toggle for Simulation and Data page plots.  
* Boxplot functionality for Data Exploration - Plot Output. (#7)  
* Text size option for labels (affects linear regression formulae and boxplot counts (N=x) only).  

## Bugfixes

* Fixed some typos.  
* Most selectizeInputs now sort alphabetically. 

## Other

* Suggests mrgsolve v.1.5.2 to be installed.  

# MVPapp 0.2.9 (2024-10-25)

## Features

* Upgraded Data Input - Plot Output tab, supporting facets, smoothers, and linear regression. (#5)  

## Bugfixes

* Fixed display error when model does not compile for PSA plots. (#2)  
* Fixed wrong behavior for median lines binning. (#4)  
* Consistently include dates for all downloadable items.

# MVPapp 0.2.8 (2024-10-14)

## Features

* More template models (1 CMT lag time).  
* Increasing default decimal places (3 -> 5) for variability quantiles.  
* Supports \$PRED syntax. When model has \$PRED syntax, dosing amounts will be disabled.  
* Using recover = TRUE by default to output more helpful error messages when mrgsolve does not compile.  

## Bugfixes

* Bugfix for WT-based dosing to propagate correctly to Parameter Sensitivity Analysis when WT is a parameter (Note: currently WT-based dosing remains unsupported in Variability simulations). 


# MVPapp 0.2.7 (2024-09-30)

* GitHub release  
* Minor bugfixes and optimizations
