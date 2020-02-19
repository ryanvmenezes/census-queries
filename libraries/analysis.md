Analyzing L.A. County’s changing languages in Census microdata
================

``` r
library(ipumsr)
```

    ## Registered S3 methods overwritten by 'ipumsr':
    ##   method                                 from 
    ##   format.pillar_shaft_haven_labelled_chr haven
    ##   format.pillar_shaft_haven_labelled_num haven
    ##   pillar_shaft.haven_labelled            haven

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

The IPUMS-USA extract comes with two files:

1.  A fixed-width data file in a compressed folder
2.  A data codebook in an XML format, which describes the data based on
    the [Data Documentation Initative](https://ddialliance.org/), or
    “DDI”

<!-- end list -->

``` r
ddi = read_ipums_ddi('data/usa_00009.xml')
data = read_ipums_micro(ddi)
```

    ## Use of data from IPUMS-USA is subject to conditions including that users should
    ## cite the data appropriately. Use command `ipums_conditions()` for more details.

# The codebook

The DDI details the variables in the extract.

``` r
info.variables = ddi$var_info
```

Details of the extract

``` r
info.variables
```

    ## # A tibble: 14 x 10
    ##    var_name var_label var_desc val_labels code_instr start   end imp_decim
    ##    <chr>    <chr>     <chr>    <list>     <chr>      <dbl> <dbl>     <dbl>
    ##  1 YEAR     Census y… "YEAR r… <tibble [… <NA>           1     4         0
    ##  2 SAMPLE   IPUMS sa… "SAMPLE… <tibble [… <NA>           5    10         0
    ##  3 SERIAL   Househol… "SERIAL… <tibble [… "\nSERIAL…    11    18         0
    ##  4 CBSERIAL Original… "CBSERI… <tibble [… "\nCBSERI…    19    31         0
    ##  5 HHWT     Househol… "HHWT i… <tibble [… "\nHHWT i…    32    41         2
    ##  6 CLUSTER  Househol… CLUSTER… <tibble [… "\nCLUSTE…    42    54         0
    ##  7 STATEFIP State (F… "STATEF… <tibble [… <NA>          55    56         0
    ##  8 COUNTYF… County (… "COUNTY… <tibble [… "\nCOUNTY…    57    59         0
    ##  9 STRATA   Househol… "STRATA… <tibble [… "\nSTRATA…    60    71         0
    ## 10 GQ       Group qu… "GQ cla… <tibble [… <NA>          72    72         0
    ## 11 PERNUM   Person n… PERNUM … <tibble [… "\n\nPERN…    73    76         0
    ## 12 PERWT    Person w… "PERWT … <tibble [… "\nPERWT …    77    86         2
    ## 13 LANGUAGE Language… LANGUAG… <tibble [… <NA>          87    88         0
    ## 14 LANGUAG… Language… LANGUAG… <tibble [… <NA>          89    92         0
    ## # … with 2 more variables: var_type <chr>, rectypes <lgl>

All the variables included.

``` r
info.variables$var_name
```

    ##  [1] "YEAR"      "SAMPLE"    "SERIAL"    "CBSERIAL"  "HHWT"     
    ##  [6] "CLUSTER"   "STATEFIP"  "COUNTYFIP" "STRATA"    "GQ"       
    ## [11] "PERNUM"    "PERWT"     "LANGUAGE"  "LANGUAGED"

The key column in this extract is LANGUAGED (the more detailed
counterpart of LANGUAGE). More info about the variable is available on
[IPUMS-USA](https://usa.ipums.org/usa-action/variables/LANGUAGE).

The codebook provides all the value for every LANGUAGED code.

``` r
info.variables %>% 
  filter(var_name == 'LANGUAGED') %>% 
  `[[`('val_labels')
```

    ## [[1]]
    ## # A tibble: 495 x 2
    ##      val lbl                                 
    ##    <dbl> <chr>                               
    ##  1     0 N/A or blank                        
    ##  2   100 English                             
    ##  3   110 Jamaican Creole                     
    ##  4   120 Krio, Pidgin Krio                   
    ##  5   130 Hawaiian Pidgin                     
    ##  6   140 Pidgin                              
    ##  7   150 Gullah, Geechee                     
    ##  8   160 Saramacca                           
    ##  9   170 Other English-based Creole languages
    ## 10   200 German                              
    ## # … with 485 more rows

# The data

All of the data in the extract. Most of these come preselected with any
IPUMS extract.

``` r
data %>% head()
```

    ## # A tibble: 6 x 14
    ##    YEAR       SAMPLE SERIAL CBSERIAL  HHWT CLUSTER STATEFIP COUNTYFIP
    ##   <int>    <int+lbl>  <dbl>    <dbl> <dbl>   <dbl> <int+lb> <dbl+lbl>
    ## 1  1980 198001 [198… 192089       NA    20 1.98e12 6 [Cali…      2323
    ## 2  1980 198001 [198… 192089       NA    20 1.98e12 6 [Cali…      2323
    ## 3  1980 198001 [198… 192089       NA    20 1.98e12 6 [Cali…      2323
    ## 4  1980 198001 [198… 192089       NA    20 1.98e12 6 [Cali…      2323
    ## 5  1980 198001 [198… 192089       NA    20 1.98e12 6 [Cali…      2323
    ## 6  1980 198001 [198… 192090       NA    20 1.98e12 6 [Cali…      2323
    ## # … with 6 more variables: STRATA <dbl>, GQ <int+lbl>, PERNUM <dbl>,
    ## #   PERWT <dbl>, LANGUAGE <int+lbl>, LANGUAGED <int+lbl>

Filtering down to L.A. and kepeing the relevant columns

``` r
la.data = data %>% 
  filter(COUNTYFIP == 37) %>% 
  select(YEAR, LANGUAGE, LANGUAGED, PERWT)

la.data %>% head()
```

    ## # A tibble: 6 x 4
    ##    YEAR    LANGUAGE     LANGUAGED PERWT
    ##   <int>   <int+lbl>     <int+lbl> <dbl>
    ## 1  1980 1 [English] 100 [English]    20
    ## 2  1980 1 [English] 100 [English]    20
    ## 3  1980 1 [English] 100 [English]    20
    ## 4  1980 1 [English] 100 [English]    20
    ## 5  1980 1 [English] 100 [English]    20
    ## 6  1980 1 [English] 100 [English]    20

[PERWT](https://usa.ipums.org/usa-action/variables/PERWT#description_section)
is the approxmiation of how many people this line of data represents. In
this case, it would be the approximate amount of people speaking that
language. It needs to be aggregated and summed to get total counts for
the language.

``` r
la.data.agg = la.data %>% 
  group_by(YEAR, LANGUAGED) %>% 
  summarise(PERWT = sum(PERWT))

la.data.agg %>% head()
```

    ## # A tibble: 6 x 3
    ## # Groups:   YEAR [1]
    ##    YEAR               LANGUAGED   PERWT
    ##   <int>               <int+lbl>   <dbl>
    ## 1  1980   0 [N/A or blank]       346500
    ## 2  1980 100 [English]           4888160
    ## 3  1980 110 [Jamaican Creole]       100
    ## 4  1980 120 [Krio, Pidgin Krio]      60
    ## 5  1980 140 [Pidgin]                180
    ## 6  1980 150 [Gullah, Geechee]       420

Reformat the data, separating the labels from the code, plus add a
column for the percent of the population speaking that language in each
year.

``` r
la.languages = la.data.agg %>% 
  group_by(YEAR) %>% 
  mutate(percent = PERWT / sum(PERWT)) %>% 
  ungroup(YEAR) %>% 
  transmute(
    year = YEAR,
    langcode = zap_labels(LANGUAGED),
    language = as_factor(LANGUAGED),
    total = PERWT,
    percent
  )

la.languages %>% head()
```

    ## # A tibble: 6 x 5
    ##    year langcode language            total    percent
    ##   <int>    <int> <fct>               <dbl>      <dbl>
    ## 1  1980        0 N/A or blank       346500 0.0462    
    ## 2  1980      100 English           4888160 0.652     
    ## 3  1980      110 Jamaican Creole       100 0.0000133 
    ## 4  1980      120 Krio, Pidgin Krio      60 0.00000801
    ## 5  1980      140 Pidgin                180 0.0000240 
    ## 6  1980      150 Gullah, Geechee       420 0.0000560
