# cutlery

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

<!-- badges: end -->

Collection of functions used for performing range of common tasks. Convert columns to factors, get fiscal year from a date, query postgreSQL database, write to clipboard, send email from Outlook, kill a windows process

All contributions are welcome

## Installation

The package is not available on CRAN but can be installed from GitHub 

``` r
devtools::install_github("amit-agni/cutlery")
```

### Currently implemented functions

* DTconvert2factor	: Bulk type conversion of columns to factors
* fiscal_quarter	  : Assign fiscal quarter to a date
* killProcess	      : Kills or ends a windows process
* runSQLinFile	    : Runs the query given in a text file on postgreSQL database
* runSQLinText	    : Runs the query given as sqlText string
* to_na	            : replace NaN and Inf with NA
* write2clip	      : Write to clipboard
* outlookSend       : Send email using outlook (VBA/vbscript)


## Example

Convert multiple columns for a dataframe to factor

``` r
DT <- data.frame(c1=c("a","b","c")
,c11 = c("a","b","c")
,c2 = 1:3
,c3 = c(1.1,1.2,1.3)
,stringsAsFactors = F)

DT <- DTconvert2factor(DT,"int")

```
**Notes**
* The repo folder is stored in Onedrive and sync'd on Mac and Windows. There were issues due to the different line endings on both the platforms. Windows adds CRLF whereas Mac adds LF. So when the repo was commited on Windows and Onedrive was sync'd, on Mac it would recognise the line endings as modified changes. This was solve using ![Source](https://help.github.com/en/github/using-git/configuring-git-to-handle-line-endings#refreshing-a-repository-after-changing-line-endings)

**DISCLAIMER :** Some of these functions are modifications to the functions from the already existing CRAN/github packages. For example the `cutlery::fiscal_quarter()` is a modified version of `lubridate::quarter()`   
