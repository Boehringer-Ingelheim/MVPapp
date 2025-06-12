# MVPapp 0.3.1 (2025-06-12)

## Bugfixes

* Weight-based dosing now correctly apply for batch runs and variability simulations (#6)


# MVPapp 0.3.0 (2025-06-09)

## Features

* Implemented Batch runs (Parameter Sensitivity Analysis Page - All Parameters) to assess all parameters in the model with a defined upper/lower range in a single click (#44)
  * QoL features includes fixing parameters, reset entire table, multiple display scales, and other graphical options

## Bugfixes

* General performance improvements for larger models with long sampling (#43)
* Stability and minor UI fixes


# MVPapp 0.2.19 (2025-06-03)

## Features

* Re-worked median line bins to be based on quantiles (#37, #38)
* Built-in data filtering option to distinct by ID (#40)
* More QoL options for NCA (safeguards and rounding) (#41)
* Minor re-factoring and other QoL updates

## Bugfixes

* Prevent non-sensible parameter estimates from crashing the entire app (#42)
* Bug fixes for General Plot's box plot count labels (#39)
* Better handling of NAs for Quantize X-axis option in General Plot

# MVPapp 0.2.18 (2025-04-21)

## Features

* New option for individual plots to identify outliers (#36)
* New option for individual plots to sort by one or more variables (#35)
* Additional tooltips for several options
* Separate plot titles for general and individual plots
* New template model (Parallel zero/first-order absorption model)

## Bugfixes

* Fixed a bug where NCA would fail if dosing rows are not excluded by default (#33)
* Safeguard for simulating too many samples, with a default value of max 20000 samples (#32)
* NCA is now sorted by the grouping variable, followed by the dose column (#34)

# MVPapp 0.2.17 (2025-03-13)

## Features

* New option to split X-axis into quantiles for Data Page General Plots (#30)
* More template models (QS TMDD, MM TMDD).

## Bugfixes

* Allows age and/or weight range sliders in Variability page to be the same value (#29)
* Re-worded some tooltips to improve clarity.

# MVPapp 0.2.16 (2025-03-04)

## Features

* Implement a short delay for some inputs to avoid frequent computations (#28)

## Bugfixes

* Corrected code for Michaelis-Menten example (#27)

# MVPapp 0.2.15 (2025-02-26)

## Features

* Filter per column in Data Page (#26)

## Bugfixes

* Handle duplicated column names in an uploaded dataset (#23)
* Properly display log y-axis with same scale when dosing info is displayed for Individual Plots (#25)

## Other

* The built-in filter for removing EVID rows now uses `EVID >= 1`, instead of `EVID == 1 | EVID == 4` 

# MVPapp 0.2.14 (2025-02-07)

## Features

* Additional tab for Variability plots to display box plots of exposures (#22)

## Bugfixes

* Minor updates to some tooltips and button placements to increase clarity.

# MVPapp 0.2.13 (2025-02-04)

## Features

* Dosing information, scaling, and LLOQ plotting options for Individual Plots (#18, #21)
* More options for dataset built-in cleaning (#20)

## Bugfixes

* Checking for column validity for "Color by" option in General Plots (#19)

# MVPapp 0.2.12 (2025-01-06)

## Features

* Multiple facets for General Plots (#17)
* Individual plots for exploring uploaded datasets (#15)  
* Mean trend toggle for variability plots (#13)  
* Progress bars for longer computations where a spinner graphic is not feasible (#16)

## Bugfixes

* Allow "Total Doses" for each Dosing Regimen to be 0 (#14)

# MVPapp 0.2.11 (2024-12-04)

## Features

* Support for uploading tab-delimited .txt files on Data Upload, with the option to automatically create ID column. (#9)  
* Uses dose column instead of manually entering dose amount for NCA.  
* Download Options now support non-interactive plots, with a dedicated "Download Non-Interactive Plot" button. (#11)  

## Bugfixes

* NCA Clearance unit corrected to be based on the supplied time unit.  
* Linear regression is center aligned for ggplots (#10)  

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
