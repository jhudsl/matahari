# matahari

[![Travis-CI Build Status](https://travis-ci.org/jhudsl/matahari.svg?branch=master)](https://travis-ci.org/jhudsl/matahari)

### Spy on Your R Session

A simple package for tidy logging of everything you type into the R console.

## Installation

You can install matahari from github with:

```R
# install.packages("devtools")
devtools::install_github("jhudsl/matahari")
```

## Getting Started

```R
library(matahari)
library(tidyverse)

# Start logging your commands
dance_start()

4 + 4
"wow!"
mean(1:10)

# Pause logging
dance_stop()

# Look at your log as a tibble
dance_tbl()

## # A tibble: 4 x 5
##         expr     value      path  contents selection
##       <list>    <list>    <list>    <list>     <lgl>
## 1 <language> <int [1]> <lgl [1]> <lgl [1]>        NA
## 2 <language> <dbl [1]> <lgl [1]> <lgl [1]>        NA
## 3  <chr [1]> <chr [1]> <lgl [1]> <lgl [1]>        NA
## 4 <language> <dbl [1]> <lgl [1]> <lgl [1]>        NA

# Do data science
dance_tbl() %>%
  select(expr, value) %>%
  mutate(class = map_chr(expr, class))
```
