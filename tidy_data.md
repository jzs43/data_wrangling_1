Tidy Data
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## ‘pivot longer’

use pulse data

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names()
pulse_data
```

    ## # A tibble: 1,087 × 7
    ##       id   age sex    bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>         <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male              7             1             2             0
    ##  2 10015  72.5 male              6            NA            NA            NA
    ##  3 10022  58.5 male             14             3             8            NA
    ##  4 10026  72.7 male             20             6            18            16
    ##  5 10035  60.4 male              4             0             1             2
    ##  6 10050  84.7 male              2            10            12             8
    ##  7 10078  31.3 male              4             0            NA            NA
    ##  8 10088  56.9 male              5            NA             0             2
    ##  9 10091  76.0 male              0             3             4             0
    ## 10 10092  74.2 female           10             2            11             6
    ## # ℹ 1,077 more rows

wide format to long format

``` r
pulse_data_tidy=
  pulse_data %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to="bdi"
  )
pulse_data_tidy
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit   bdi
    ##    <dbl> <dbl> <chr> <chr> <dbl>
    ##  1 10003  48.0 male  bl        7
    ##  2 10003  48.0 male  01m       1
    ##  3 10003  48.0 male  06m       2
    ##  4 10003  48.0 male  12m       0
    ##  5 10015  72.5 male  bl        6
    ##  6 10015  72.5 male  01m      NA
    ##  7 10015  72.5 male  06m      NA
    ##  8 10015  72.5 male  12m      NA
    ##  9 10022  58.5 male  bl       14
    ## 10 10022  58.5 male  01m       3
    ## # ℹ 4,338 more rows

rewrite, combine, and extend (to add a mutate)

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to="bdi"
  ) %>% 
relocate(id,visit) %>% 
  mutate(visit=recode(visit,"bl"="00m"))

pulse_data
```

    ## # A tibble: 4,348 × 5
    ##       id visit   age sex     bdi
    ##    <dbl> <chr> <dbl> <chr> <dbl>
    ##  1 10003 00m    48.0 male      7
    ##  2 10003 01m    48.0 male      1
    ##  3 10003 06m    48.0 male      2
    ##  4 10003 12m    48.0 male      0
    ##  5 10015 00m    72.5 male      6
    ##  6 10015 01m    72.5 male     NA
    ##  7 10015 06m    72.5 male     NA
    ##  8 10015 12m    72.5 male     NA
    ##  9 10022 00m    58.5 male     14
    ## 10 10022 01m    58.5 male      3
    ## # ℹ 4,338 more rows

## ‘pivot_wider’

make up some data

``` r
analysis_results=
  tibble(
    group=c("treatment","treatment","placebo","placebo"),
    time=c("pre","post","pre","post"),
    mean=c(4,8,3.5,4)
  )

analysis_results %>% 
  pivot_wider(
    names_from = "time",
    values_from = "mean"
  )
```

    ## # A tibble: 2 × 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

``` r
analysis_results
```

    ## # A tibble: 4 × 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

## Binding rows

use the LotR data first step: import each table

``` r
fellowship_ring=
  readxl::read_excel("./data/LotR_Words.xlsx",range="B3:D6") %>% 
  mutate(movie="fellowship_ring")

two_towers=
  readxl::read_excel("./data/LotR_Words.xlsx",range="F3:H6") %>% 
  mutate(movie="two_towers")

return_king=
  readxl::read_excel("./data/LotR_Words.xlsx",range="J3:L6") %>% 
  mutate(movie="return_king")
```

bind all rows together

``` r
lotr_tidy=
  bind_rows(fellowship_ring, two_towers,return_king) %>% 
  janitor::clean_names() %>% 
  relocate(movie) %>% 
  pivot_longer(
    female:male,
    names_to="gender",
    values_to="words"
  )
```

## joining dataset

import and clean dataset

``` r
pups_df=
  read_csv("./data/FAS_pups.csv",show_col_types = FALSE) %>% 
  janitor::clean_names() %>% 
  mutate(sex=recode(sex,'1'="male",'2'="female"))

pups_df
```

    ## # A tibble: 313 × 6
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #85           male        4      13        7      11
    ##  2 #85           male        4      13        7      12
    ##  3 #1/2/95/2     male        5      13        7       9
    ##  4 #1/2/95/2     male        5      13        8      10
    ##  5 #5/5/3/83/3-3 male        5      13        8      10
    ##  6 #5/5/3/83/3-3 male        5      14        6       9
    ##  7 #5/4/2/95/2   male       NA      14        5       9
    ##  8 #4/2/95/3-3   male        4      13        6       8
    ##  9 #4/2/95/3-3   male        4      13        7       9
    ## 10 #2/2/95/3-2   male        4      NA        8      10
    ## # ℹ 303 more rows

``` r
litters_df=
  read_csv("./data/FAS_litters.csv",show_col_types = FALSE) %>% 
  janitor::clean_names() %>% 
  relocate(litter_number) %>% 
  separate(group,into=c("dose","day_of_tx"),sep=3)

litters_df
```

    ## # A tibble: 49 × 9
    ##    litter_number   dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##    <chr>           <chr> <chr>          <dbl>       <dbl>       <dbl>
    ##  1 #85             Con   7               19.7        34.7          20
    ##  2 #1/2/95/2       Con   7               27          42            19
    ##  3 #5/5/3/83/3-3   Con   7               26          41.4          19
    ##  4 #5/4/2/95/2     Con   7               28.5        44.1          19
    ##  5 #4/2/95/3-3     Con   7               NA          NA            20
    ##  6 #2/2/95/3-2     Con   7               NA          NA            20
    ##  7 #1/5/3/83/3-3/2 Con   7               NA          NA            20
    ##  8 #3/83/3-3       Con   8               NA          NA            20
    ##  9 #2/95/3         Con   8               NA          NA            20
    ## 10 #3/5/2/2/95     Con   8               28.5        NA            20
    ## # ℹ 39 more rows
    ## # ℹ 3 more variables: pups_born_alive <dbl>, pups_dead_birth <dbl>,
    ## #   pups_survive <dbl>

next up, time to join them

``` r
fas_df=
  left_join(pups_df,litters_df,by="litter_number") %>% 
  arrange(litter_number) %>% 
  relocate(litter_number,dose,day_of_tx)
  
fas_df
```

    ## # A tibble: 313 × 14
    ##    litter_number   dose  day_of_tx sex    pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>           <chr> <chr>     <chr>    <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #1/2/95/2       Con   7         male         5      13        7       9
    ##  2 #1/2/95/2       Con   7         male         5      13        8      10
    ##  3 #1/2/95/2       Con   7         female       4      13        7       9
    ##  4 #1/2/95/2       Con   7         female       4      13        7      10
    ##  5 #1/2/95/2       Con   7         female       5      13        8      10
    ##  6 #1/2/95/2       Con   7         female       5      13        8      10
    ##  7 #1/2/95/2       Con   7         female       5      13        6      10
    ##  8 #1/5/3/83/3-3/2 Con   7         male         4      NA       NA       9
    ##  9 #1/5/3/83/3-3/2 Con   7         male         4      NA        7       9
    ## 10 #1/5/3/83/3-3/2 Con   7         male         4      NA        7       9
    ## # ℹ 303 more rows
    ## # ℹ 6 more variables: gd0_weight <dbl>, gd18_weight <dbl>, gd_of_birth <dbl>,
    ## #   pups_born_alive <dbl>, pups_dead_birth <dbl>, pups_survive <dbl>
