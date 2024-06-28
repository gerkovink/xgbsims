[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12586277.svg)](https://doi.org/10.5281/zenodo.12586277)

# Simulation Archive
This repository holds the simulation archive for the manuscript "The XGBoost Paradigm for Missing Data: Is It Worth the Hype" by Huma Shehwana and Gerko Vink. The corresponding manuscript [can be found here](https:/www.gerkovink.com/xgbpaper)

# Requirements
This simulation depends on new `mice` functionality that is still in beta testing. To install the required beta version of the `mice` package with the latest `mice.impute.xgb` and helper functions, use the following code-block:

``` r
install.packages("devtools")
devtools::install_github(repo = "gerkovink/mice@xgb")
```

# License
Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
