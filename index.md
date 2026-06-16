# susometa

The susometa package provides functions to:

- Parse Survey Solutions’ questionnaire metadata from JSON file to
  rectangular data frame
- Extract questionnaire objects and their attributes (e.g., sections,
  rosters, questions, answer options, etc.)

## Installation

Since susometa is not yet available on CRAN, it can be installed from
GitHub as follows:

``` r

if (!require("pak")) install.packages("pak")
pak::pak("lsms-worldbank/susometa")
```
