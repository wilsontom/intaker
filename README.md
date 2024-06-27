# intaker

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![R-CMD-check](https://github.com/aberWARU/tidyIntake24/workflows/R-CMD-check/badge.svg)](https://github.com/aberWARU/tidyIntake24/actions) ![License](https://img.shields.io/badge/license-GNU%20GPL%20v3.0-blue.svg "GNU GPL v3.0")

### Getting Started

`intaker` can be installed directly from GitHub using the `remotes` package.

``` r
remotes::install_github('wilsontom/intaker')
```

Once the package is installed, the `openIntake24` function can be used to open and parse raw data from Intake24 surveys.

``` r
RawIntake24 <- readr::read_csv(system.file('example_data.csv', package = 'tidyIntake24'))
```

``` r
library(tidyIntake24)

Intake24 <- openIntake24(RawIntake24)

Intake24

── Intake24 Object ──────────────────────────────────────────tidyIntake24 v0.1.1 ── 
Object Size: 1.1 Mb 
 
# of Participants: 9 
 
# of Surveys: 34 
 
Start Date: 2018-07-23 
End Date: 2018-10-18 
 
── Top 10 items recorded ──────────────────────────────────────────────────────────
Semi skimmed milk (45)
Coffee (35)
Tea (24)
Water (from tap, including filtered) (24)
White sugar (22)
Toast, white bread (15)
Whole milk (13)
Coca cola, (not diet) (12)
Lamb hot pot / casserole (11)
Butter, salted (8)
```
