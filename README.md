# matahari

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/matahari)](https://cran.r-project.org/package=matahari)
[![Travis-CI Build Status](https://travis-ci.org/jhudsl/matahari.svg?branch=master)](https://travis-ci.org/jhudsl/matahari)
<!-- badges: end -->

### Spy on Your R Session

A simple package for tidy logging of everything you type into the R console.

## Installation

You can install matahari from CRAN with:

```R
install.packages("matahari")
```

Or from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("jhudsl/matahari")
```

## Getting Started

```R
library(matahari)
library(tidyverse)
library(knitr)

# Start logging your commands
dance_start(value = TRUE)

4 + 4
"wow!"
mean(1:10)

# Pause logging
dance_stop()

# Look at your log as a tibble
dance_tbl()

## # A tibble: 6 x 6
##   expr       value             path      contents  selection dt                 
##   <list>     <list>            <list>    <list>    <list>    <dttm>             
## 1 <language> <S3: sessionInfo> <lgl [1]> <lgl [1]> <lgl [1]> 2018-06-23 15:26:06
## 2 <language> <int [1]>         <lgl [1]> <lgl [1]> <lgl [1]> 2018-06-23 15:26:06
## 3 <language> <dbl [1]>         <lgl [1]> <lgl [1]> <lgl [1]> 2018-06-23 15:26:07
## 4 <chr [1]>  <chr [1]>         <lgl [1]> <lgl [1]> <lgl [1]> 2018-06-23 15:26:08
## 5 <language> <dbl [1]>         <lgl [1]> <lgl [1]> <lgl [1]> 2018-06-23 15:26:08
## 6 <language> <S3: sessionInfo> <lgl [1]> <lgl [1]> <lgl [1]> 2018-06-23 15:26:09

# Do data science
dance_tbl() %>%
  slice(2:(n() - 1)) %>%
  select(expr, value) %>%
  mutate(class = map_chr(expr, class)) %>%
  kable()

## |expr                      |value |class     |
## |:-------------------------|:-----|:---------|
## |dance_start(value = TRUE) |1     |call      |
## |4 + 4                     |8     |call      |
## |wow!                      |wow!  |character |
## |mean(1:10)                |5.5   |call      |
```

## Evaluating files

```R
library(matahari)
library(tidyverse)
library(knitr)

code_file <- system.file("test", "sample_code.R", package = "matahari")

code_file %>%
  dance_recital() %>% 
  kable()
  
## |expr                |result   |error           |output   |warnings |messages |
## |:-------------------|:--------|:---------------|:--------|:--------|:--------|
## |4 + 4               |8        |NULL            |         |         |         |
## |wow!                |wow!     |NULL            |         |         |         |
## |mean(1:10)          |5.5      |NULL            |         |         |         |
## |stop("Error!")      |NULL     |Error!, .f(...) |NULL     |NULL     |NULL     |
## |warning("Warning!") |Warning! |NULL            |         |Warning! |         |
## |message("Hello?")   |NULL     |NULL            |         |         |Hello?   |
## |cat("Welcome!")     |NULL     |NULL            |Welcome! |         |         |

code_string <- "set.seed(42)\nx <- sample(1:10, 5)\nmedian(x)"

code_string %>%
  dance_recital() %>% 
  kable()

## |expr                 |result         |error |output |warnings |messages |
## |:--------------------|:--------------|:-----|:------|:--------|:--------|
## |set.seed(42)         |NULL           |NULL  |       |         |         |
## |x <- sample(1:10, 5) |10, 9, 3, 6, 4 |NULL  |       |         |         |
## |median(x)            |6              |NULL  |       |         |         |
```
