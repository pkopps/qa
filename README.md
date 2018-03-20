# Overview

Functionality to make QAing data less painful

# Installation

```r
devtools::install_github("pkopps/qa")
```

# Usage

```r
library(tidyverse)
library(qa)

starwars %>% qa()
#Error in qa(.) : Function cannot handle type 'list' columns

starwars %>% select(-films:-starships) %>% qa()
# $Dimensions
# Rows: 87 Cols: 10
# 
# $Preview
# # A tibble: 6 x 10
#   name           height  mass hair_color  skin_color  eye_color birth_year gender homeworld species
#   <chr>           <int> <dbl> <chr>       <chr>       <chr>          <dbl> <chr>  <chr>     <chr>  
# 1 Luke Skywalker    172  77.0 blond       fair        blue            19.0 male   Tatooine  Human  
# 2 C-3PO             167  75.0 NA          gold        yellow         112   NA     Tatooine  Droid  
# 3 R2-D2              96  32.0 NA          white, blue red             33.0 NA     Naboo     Droid  
# 4 Darth Vader       202 136   none        white       yellow          41.9 male   Tatooine  Human  
# 5 Leia Organa       150  49.0 brown       light       brown           19.0 female Alderaan  Human  
# 6 Owen Lars         178 120   brown, grey light       blue            52.0 male   Tatooine  Human  
# 
# $Summary
# # A tibble: 10 x 13
#    names       type      n_rows n_zero p_zero  n_na p_na   n_inf p_inf n_unique min   max   mean   
#    <chr>       <chr>      <int>  <int> <chr>  <int> <chr>  <int> <chr>    <int> <chr> <chr> <chr>  
#  1 name:       character     87      0 0%         0 0%         0 0%          87 NA    NA    NA     
#  2 height:     integer       87      0 0%         6 6.9%       0 0%          45 66    264   174.358
#  3 mass:       numeric       87      0 0%        28 32.18%     0 0%          38 15    1358  97.312 
#  4 hair_color: character     87      0 0%         5 5.75%      0 0%          12 NA    NA    NA     
#  5 skin_color: character     87      0 0%         0 0%         0 0%          31 NA    NA    NA     
#  6 eye_color:  character     87      0 0%         0 0%         0 0%          15 NA    NA    NA     
#  7 birth_year: numeric       87      0 0%        44 50.57%     0 0%          36 8     896   87.565 
#  8 gender:     character     87      0 0%         3 3.45%      0 0%           4 NA    NA    NA     
#  9 homeworld:  character     87      0 0%        10 11.49%     0 0%          48 NA    NA    NA     
# 10 species:    character     87      0 0%         5 5.75%      0 0%          37 NA    NA    NA 
```


