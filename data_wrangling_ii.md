data_wrangling_ii
================
William Anderson
2022-10-13

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
```

## Extracting tables

This page contains data from the National Survey on Drug Use and Health;
it includes tables for drug use in the past year or month, separately
for specific kinds of drug use. These data are potentially useful for
analysis, and we’d like to be able to read in the first table.

First, let’s make sure we can load the data from the web

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_html = read_html(url)

drug_use_html
```

    ## {html_document}
    ## <html lang="en">
    ## [1] <head>\n<link rel="P3Pv1" href="http://www.samhsa.gov/w3c/p3p.xml">\n<tit ...
    ## [2] <body>\r\n\r\n<noscript>\r\n<p>Your browser's Javascript is off. Hyperlin ...

Rather than trying to grab something using a CSS selector, let’s try our
luck extracting the tables from the HTML.

``` r
drug_use_html %>%
  html_table()
```

    ## [[1]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "12.90… "13.36" "0.002" "13.28… "12.86" "0.063" "31.78" "32.07" "0.369"
    ##  3 "Nor… "13.88… "14.66" "0.005" "13.98" "13.51" "0.266" "34.66… "36.45" "0.008"
    ##  4 "Mid… "12.40… "12.76" "0.082" "12.45" "12.33" "0.726" "32.13" "32.20" "0.900"
    ##  5 "Sou… "11.24… "11.64" "0.029" "12.02" "11.88" "0.666" "28.93" "29.20" "0.581"
    ##  6 "Wes… "15.27" "15.62" "0.262" "15.53… "14.43" "0.018" "33.72" "33.19" "0.460"
    ##  7 "Ala… "9.98"  "9.60"  "0.426" "9.90"  "9.71"  "0.829" "26.99" "26.13" "0.569"
    ##  8 "Ala… "19.60… "21.92" "0.010" "17.30" "18.44" "0.392" "36.47… "40.69" "0.015"
    ##  9 "Ari… "13.69" "13.12" "0.364" "15.12" "13.45" "0.131" "31.53" "31.15" "0.826"
    ## 10 "Ark… "11.37" "11.59" "0.678" "12.79" "12.14" "0.538" "26.53" "27.06" "0.730"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[2]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "7.96a" "8.34"  "0.001" "7.22"  "7.20"  "0.905" "19.32" "19.70" "0.178"
    ##  3 "Nor… "8.58a" "9.28"  "0.001" "7.68"  "7.73"  "0.883" "21.19… "22.64" "0.007"
    ##  4 "Mid… "7.50a" "7.92"  "0.009" "6.64"  "6.80"  "0.530" "19.23" "19.29" "0.873"
    ##  5 "Sou… "6.74a" "7.02"  "0.044" "6.31"  "6.49"  "0.441" "17.20… "17.79" "0.092"
    ##  6 "Wes… "9.84"  "10.08" "0.324" "8.85"  "8.31"  "0.144" "21.30" "20.85" "0.425"
    ##  7 "Ala… "5.57"  "5.35"  "0.510" "4.98"  "5.16"  "0.779" "15.04" "14.33" "0.503"
    ##  8 "Ala… "11.85… "14.38" "0.002" "9.19"  "10.64" "0.204" "21.30… "25.02" "0.020"
    ##  9 "Ari… "8.80"  "8.51"  "0.570" "8.30"  "7.71"  "0.491" "20.04" "18.92" "0.412"
    ## 10 "Ark… "6.70"  "7.17"  "0.240" "6.22"  "6.46"  "0.748" "16.21" "16.93" "0.556"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[3]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "1.91"  "1.95"  "0.276" "5.60"  "5.41"  "0.106" "7.68"  "7.88"  "0.294"
    ##  3 "Nor… "2.01"  "2.04"  "0.634" "5.85b" "5.55"  "0.095" "8.40"  "8.67"  "0.410"
    ##  4 "Mid… "1.95"  "1.96"  "0.854" "5.31"  "5.12"  "0.208" "8.17"  "8.14"  "0.924"
    ##  5 "Sou… "1.69"  "1.75"  "0.137" "5.18"  "5.13"  "0.674" "6.77"  "7.12"  "0.171"
    ##  6 "Wes… "2.20"  "2.21"  "0.868" "6.37b" "6.02"  "0.085" "8.27"  "8.32"  "0.877"
    ##  7 "Ala… "1.42"  "1.49"  "0.383" "4.46"  "4.36"  "0.791" "6.04"  "6.39"  "0.524"
    ##  8 "Ala… "3.01a" "3.54"  "0.012" "6.99"  "7.52"  "0.371" "9.04b" "10.69" "0.079"
    ##  9 "Ari… "2.16"  "2.15"  "0.934" "6.58"  "6.09"  "0.302" "8.00"  "8.36"  "0.620"
    ## 10 "Ark… "1.82"  "1.84"  "0.794" "5.78"  "5.37"  "0.331" "6.63"  "7.08"  "0.471"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[4]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "1.66a" "1.76"  "0.040" "0.60"  "0.64"  "0.393" "4.51a" "4.98"  "0.005"
    ##  3 "Nor… "1.94a" "2.18"  "0.012" "0.60"  "0.66"  "0.374" "5.19a" "6.06"  "0.001"
    ##  4 "Mid… "1.37"  "1.43"  "0.282" "0.48"  "0.54"  "0.288" "3.81a" "4.22"  "0.020"
    ##  5 "Sou… "1.45b" "1.56"  "0.067" "0.53"  "0.57"  "0.462" "3.77a" "4.32"  "0.000"
    ##  6 "Wes… "2.03"  "2.05"  "0.816" "0.82"  "0.85"  "0.806" "5.78"  "5.88"  "0.740"
    ##  7 "Ala… "1.23"  "1.22"  "0.995" "0.42"  "0.41"  "0.883" "3.09"  "3.20"  "0.707"
    ##  8 "Ala… "1.54a" "2.00"  "0.010" "0.51"  "0.65"  "0.200" "3.84a" "4.79"  "0.035"
    ##  9 "Ari… "2.25"  "2.29"  "0.861" "1.01"  "0.85"  "0.262" "6.23"  "6.92"  "0.272"
    ## 10 "Ark… "0.93"  "1.07"  "0.208" "0.41"  "0.48"  "0.380" "2.54"  "2.89"  "0.234"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[5]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "0.30"  "0.33"  "0.217" "0.12"  "0.10"  "0.313" "0.73"  "0.69"  "0.469"
    ##  3 "Nor… "0.43a" "0.54"  "0.007" "0.13"  "0.13"  "0.984" "1.08"  "0.98"  "0.295"
    ##  4 "Mid… "0.30"  "0.31"  "0.638" "0.11"  "0.10"  "0.818" "0.74"  "0.74"  "0.995"
    ##  5 "Sou… "0.27"  "0.26"  "0.444" "0.12"  "0.08"  "0.147" "0.63"  "0.56"  "0.133"
    ##  6 "Wes… "0.25"  "0.29"  "0.152" "0.13"  "0.11"  "0.586" "0.63"  "0.65"  "0.797"
    ##  7 "Ala… "0.22"  "0.27"  "0.171" "0.10"  "0.08"  "0.676" "0.45b" "0.64"  "0.054"
    ##  8 "Ala… "0.70a" "1.23"  "0.044" "0.11"  "0.08"  "0.297" "1.19b" "0.91"  "0.073"
    ##  9 "Ari… "0.32a" "0.55"  "0.001" "0.17"  "0.20"  "0.603" "0.79"  "0.90"  "0.480"
    ## 10 "Ark… "0.19"  "0.17"  "0.398" "0.10"  "0.07"  "0.312" "0.40"  "0.42"  "0.773"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[6]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "52.42" "52.18" "0.337" "11.55… "10.58" "0.000" "59.60… "58.96" "0.088"
    ##  3 "Nor… "57.80… "56.66" "0.009" "13.19" "12.57" "0.130" "63.79" "64.17" "0.554"
    ##  4 "Mid… "55.14… "54.36" "0.032" "11.31… "10.30" "0.001" "63.11… "61.88" "0.014"
    ##  5 "Sou… "48.74" "48.85" "0.759" "10.87… "9.74"  "0.000" "56.75" "56.16" "0.243"
    ##  6 "Wes… "51.67" "52.07" "0.383" "11.71… "10.76" "0.016" "57.82" "56.92" "0.182"
    ##  7 "Ala… "44.72" "43.94" "0.533" "10.53… "8.76"  "0.038" "51.36" "51.99" "0.716"
    ##  8 "Ala… "54.02" "54.98" "0.444" "9.22"  "11.04" "0.209" "59.65" "62.57" "0.107"
    ##  9 "Ari… "51.80" "51.19" "0.613" "11.90… "10.45" "0.099" "58.02" "56.38" "0.333"
    ## 10 "Ark… "42.45" "41.81" "0.588" "9.90"  "9.34"  "0.491" "55.03… "51.60" "0.043"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[7]]
    ## # A tibble: 57 × 4
    ##    State                                                 Alcoh…¹ Alcoh…² Alcoh…³
    ##    <chr>                                                 <chr>   <chr>   <chr>  
    ##  1 "NOTE: State and census region estimates are based o… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Total U.S."                                          "22.76… "21.57" "0.000"
    ##  3 "Northeast"                                           "26.11" "25.98" "0.792"
    ##  4 "Midwest"                                             "23.73… "22.00" "0.000"
    ##  5 "South"                                               "20.68… "19.66" "0.010"
    ##  6 "West"                                                "22.73… "21.01" "0.000"
    ##  7 "Alabama"                                             "19.25" "18.19" "0.305"
    ##  8 "Alaska"                                              "21.47… "23.91" "0.058"
    ##  9 "Arizona"                                             "22.01… "19.25" "0.009"
    ## 10 "Arkansas"                                            "18.07" "16.65" "0.106"
    ## # … with 47 more rows, and abbreviated variable names
    ## #   ¹​`Alcohol Use inPast Month(2013-2014)`,
    ## #   ²​`Alcohol Use inPast Month(2014-2015)`,
    ## #   ³​`Alcohol Use inPast Month(P Value)`
    ## 
    ## [[8]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "25.36… "24.56" "0.000" "7.42a" "6.50"  "0.000" "36.04… "34.02" "0.000"
    ##  3 "Nor… "23.76" "23.30" "0.216" "7.18a" "6.22"  "0.001" "34.61… "32.89" "0.004"
    ##  4 "Mid… "28.57… "27.39" "0.000" "8.58a" "7.68"  "0.001" "40.75… "38.61" "0.000"
    ##  5 "Sou… "26.91… "26.24" "0.034" "7.57a" "6.67"  "0.000" "37.26… "35.33" "0.000"
    ##  6 "Wes… "21.20… "20.29" "0.015" "6.31a" "5.35"  "0.000" "31.04… "28.78" "0.000"
    ##  7 "Ala… "31.62" "30.46" "0.295" "8.43"  "7.78"  "0.458" "43.37" "42.27" "0.494"
    ##  8 "Ala… "29.30… "31.51" "0.048" "10.73" "9.87"  "0.429" "41.11" "42.61" "0.388"
    ##  9 "Ari… "22.14" "22.51" "0.678" "6.81"  "6.18"  "0.354" "32.74" "31.23" "0.329"
    ## 10 "Ark… "35.57" "34.05" "0.188" "10.89" "9.67"  "0.185" "43.57" "41.50" "0.210"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[9]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "21.05… "20.12" "0.000" "5.24a" "4.53"  "0.000" "29.49… "27.54" "0.000"
    ##  3 "Nor… "19.69" "19.13" "0.101" "5.10a" "4.37"  "0.001" "28.36… "26.89" "0.016"
    ##  4 "Mid… "23.84… "22.50" "0.000" "6.16a" "5.55"  "0.007" "32.84… "30.61" "0.000"
    ##  5 "Sou… "22.37… "21.41" "0.001" "5.22a" "4.50"  "0.000" "30.24… "28.57" "0.000"
    ##  6 "Wes… "17.43… "16.66" "0.026" "4.56a" "3.75"  "0.000" "26.21… "23.72" "0.000"
    ##  7 "Ala… "25.90" "24.25" "0.106" "5.66"  "5.34"  "0.612" "34.32" "34.00" "0.836"
    ##  8 "Ala… "22.00… "24.64" "0.008" "6.15"  "6.02"  "0.860" "34.04" "35.38" "0.416"
    ##  9 "Ari… "18.73" "19.23" "0.576" "4.95"  "4.15"  "0.110" "27.64" "25.33" "0.137"
    ## 10 "Ark… "28.51" "27.81" "0.519" "7.13"  "6.35"  "0.273" "34.10" "32.28" "0.244"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[10]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "6.50a" "6.14"  "0.001" "2.76"  "2.62"  "0.160" "12.64… "11.61" "0.000"
    ##  3 "Nor… "6.64"  "6.39"  "0.193" "2.88"  "2.72"  "0.371" "13.12… "12.31" "0.056"
    ##  4 "Mid… "6.59a" "6.25"  "0.033" "2.70"  "2.52"  "0.233" "13.56… "12.45" "0.000"
    ##  5 "Sou… "6.20a" "5.76"  "0.003" "2.65"  "2.52"  "0.315" "11.63… "10.61" "0.000"
    ##  6 "Wes… "6.80"  "6.47"  "0.120" "2.91"  "2.78"  "0.494" "13.03… "11.88" "0.005"
    ##  7 "Ala… "5.76a" "4.64"  "0.007" "2.84b" "2.17"  "0.053" "10.73" "9.65"  "0.234"
    ##  8 "Ala… "6.72"  "7.43"  "0.153" "2.12"  "2.55"  "0.223" "12.66" "12.30" "0.703"
    ##  9 "Ari… "7.60b" "6.66"  "0.062" "3.37"  "2.90"  "0.252" "13.03" "12.36" "0.488"
    ## 10 "Ark… "5.23"  "4.88"  "0.347" "2.63"  "2.76"  "0.703" "10.23" "9.64"  "0.508"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[11]]
    ## # A tibble: 57 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "3.04"  "2.97"  "0.321" "1.01"  "0.95"  "0.409" "5.61a" "5.18"  "0.005"
    ##  3 "Nor… "3.01"  "3.09"  "0.500" "1.02"  "0.94"  "0.402" "5.43"  "5.25"  "0.519"
    ##  4 "Mid… "3.06b" "2.86"  "0.061" "1.03"  "0.93"  "0.209" "5.83a" "5.30"  "0.012"
    ##  5 "Sou… "2.92a" "2.73"  "0.047" "0.96"  "0.92"  "0.594" "5.16a" "4.66"  "0.008"
    ##  6 "Wes… "3.25"  "3.37"  "0.409" "1.05"  "1.05"  "0.945" "6.24"  "5.82"  "0.152"
    ##  7 "Ala… "2.99a" "2.34"  "0.026" "0.98"  "0.80"  "0.264" "4.74"  "3.86"  "0.120"
    ##  8 "Ala… "3.21"  "3.67"  "0.200" "0.77"  "0.87"  "0.560" "6.19"  "6.23"  "0.960"
    ##  9 "Ari… "3.44"  "3.62"  "0.591" "1.11"  "1.10"  "0.969" "5.68"  "5.92"  "0.735"
    ## 10 "Ark… "2.73"  "2.42"  "0.255" "0.96"  "0.96"  "0.986" "5.03"  "4.30"  "0.210"
    ## # … with 47 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`
    ## 
    ## [[12]]
    ## # A tibble: 57 × 10
    ##    State 18+(2…¹ 18+(2…² 18+(P…³ 18-25…⁴ 18-25…⁵ 18-25…⁶ 26+(2…⁷ 26+(2…⁸ 26+(P…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "4.15"  "4.05"  "0.325" "4.52a" "4.92"  "0.004" "4.09"  "3.91"  "0.110"
    ##  3 "Nor… "3.93"  "3.94"  "0.953" "4.67a" "5.15"  "0.023" "3.80"  "3.74"  "0.712"
    ##  4 "Mid… "4.45"  "4.36"  "0.511" "4.93a" "5.33"  "0.036" "4.37"  "4.19"  "0.285"
    ##  5 "Sou… "4.17"  "4.00"  "0.206" "4.18a" "4.54"  "0.027" "4.17b" "3.91"  "0.091"
    ##  6 "Wes… "4.02"  "3.96"  "0.681" "4.56b" "4.98"  "0.065" "3.93"  "3.78"  "0.412"
    ##  7 "Ala… "4.53"  "4.64"  "0.749" "4.30"  "4.97"  "0.137" "4.57"  "4.59"  "0.971"
    ##  8 "Ala… "3.90"  "4.02"  "0.707" "4.60a" "5.75"  "0.044" "3.77"  "3.69"  "0.844"
    ##  9 "Ari… "4.09"  "4.33"  "0.491" "4.45"  "5.06"  "0.244" "4.03"  "4.21"  "0.651"
    ## 10 "Ark… "5.24"  "5.27"  "0.942" "4.49"  "5.16"  "0.213" "5.36"  "5.29"  "0.882"
    ## # … with 47 more rows, and abbreviated variable names ¹​`18+(2013-2014)`,
    ## #   ²​`18+(2014-2015)`, ³​`18+(P Value)`, ⁴​`18-25(2013-2014)`,
    ## #   ⁵​`18-25(2014-2015)`, ⁶​`18-25(P Value)`, ⁷​`26+(2013-2014)`,
    ## #   ⁸​`26+(2014-2015)`, ⁹​`26+(P Value)`
    ## 
    ## [[13]]
    ## # A tibble: 57 × 10
    ##    State 18+(2…¹ 18+(2…² 18+(P…³ 18-25…⁴ 18-25…⁵ 18-25…⁶ 26+(2…⁷ 26+(2…⁸ 26+(P…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "18.29" "18.01" "0.146" "19.75… "20.89" "0.000" "18.05… "17.52" "0.020"
    ##  3 "Nor… "17.87" "17.76" "0.735" "20.80… "22.00" "0.012" "17.39" "17.07" "0.400"
    ##  4 "Mid… "18.61" "18.34" "0.356" "20.45… "21.43" "0.013" "18.29" "17.81" "0.150"
    ##  5 "Sou… "18.01" "17.82" "0.519" "18.22… "19.31" "0.001" "17.97" "17.57" "0.227"
    ##  6 "Wes… "18.79… "18.18" "0.088" "20.70… "22.03" "0.012" "18.46… "17.51" "0.022"
    ##  7 "Ala… "19.51" "18.85" "0.469" "18.09" "18.78" "0.529" "19.75" "18.86" "0.399"
    ##  8 "Ala… "18.12" "18.11" "0.989" "20.33… "24.93" "0.001" "17.70" "16.80" "0.341"
    ##  9 "Ari… "18.59" "18.32" "0.756" "18.76" "19.58" "0.518" "18.56" "18.10" "0.648"
    ## 10 "Ark… "20.00" "19.77" "0.816" "19.99" "21.50" "0.225" "20.00" "19.48" "0.645"
    ## # … with 47 more rows, and abbreviated variable names ¹​`18+(2013-2014)`,
    ## #   ²​`18+(2014-2015)`, ³​`18+(P Value)`, ⁴​`18-25(2013-2014)`,
    ## #   ⁵​`18-25(2014-2015)`, ⁶​`18-25(P Value)`, ⁷​`26+(2013-2014)`,
    ## #   ⁸​`26+(2014-2015)`, ⁹​`26+(P Value)`
    ## 
    ## [[14]]
    ## # A tibble: 57 × 10
    ##    State 18+(2…¹ 18+(2…² 18+(P…³ 18-25…⁴ 18-25…⁵ 18-25…⁶ 26+(2…⁷ 26+(2…⁸ 26+(P…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "3.94"  "3.99"  "0.526" "7.44a" "7.88"  "0.014" "3.34"  "3.34"  "0.994"
    ##  3 "Nor… "3.81"  "3.93"  "0.407" "7.60b" "8.15"  "0.058" "3.19"  "3.24"  "0.746"
    ##  4 "Mid… "4.11"  "4.14"  "0.791" "7.83"  "8.08"  "0.333" "3.48"  "3.48"  "0.993"
    ##  5 "Sou… "3.84"  "3.86"  "0.896" "6.89a" "7.40"  "0.029" "3.33"  "3.27"  "0.680"
    ##  6 "Wes… "4.02"  "4.12"  "0.522" "7.84"  "8.25"  "0.223" "3.35"  "3.41"  "0.749"
    ##  7 "Ala… "3.98"  "4.02"  "0.885" "7.31"  "7.76"  "0.501" "3.41"  "3.40"  "0.973"
    ##  8 "Ala… "4.21"  "4.68"  "0.237" "8.30b" "9.97"  "0.064" "3.43"  "3.66"  "0.613"
    ##  9 "Ari… "4.23"  "4.34"  "0.775" "7.04"  "8.06"  "0.208" "3.75"  "3.70"  "0.907"
    ## 10 "Ark… "4.58"  "4.41"  "0.682" "6.67"  "7.43"  "0.287" "4.23"  "3.90"  "0.515"
    ## # … with 47 more rows, and abbreviated variable names ¹​`18+(2013-2014)`,
    ## #   ²​`18+(2014-2015)`, ³​`18+(P Value)`, ⁴​`18-25(2013-2014)`,
    ## #   ⁵​`18-25(2014-2015)`, ⁶​`18-25(P Value)`, ⁷​`26+(2013-2014)`,
    ## #   ⁸​`26+(2014-2015)`, ⁹​`26+(P Value)`
    ## 
    ## [[15]]
    ## # A tibble: 57 × 13
    ##    State 18+(2…¹ 18+(2…² 18+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 "NOT… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:… "NOTE:…
    ##  2 "Tot… "6.63"  "6.64"  "0.915" "11.01… "11.93" "0.000" "9.00a" "9.79"  "0.000"
    ##  3 "Nor… "6.66"  "6.82"  "0.458" "10.63… "11.68" "0.002" "9.61a" "10.56" "0.009"
    ##  4 "Mid… "6.81"  "6.87"  "0.736" "10.81… "12.12" "0.000" "9.39a" "10.23" "0.003"
    ##  5 "Sou… "6.47"  "6.52"  "0.750" "10.77… "11.51" "0.008" "8.31a" "9.09"  "0.003"
    ##  6 "Wes… "6.67"  "6.47"  "0.353" "11.82… "12.59" "0.046" "9.30b" "9.94"  "0.079"
    ##  7 "Ala… "6.85"  "6.81"  "0.948" "10.74" "10.97" "0.764" "8.24"  "8.61"  "0.648"
    ##  8 "Ala… "6.57"  "6.73"  "0.770" "9.92a" "12.40" "0.012" "9.19a" "11.68" "0.019"
    ##  9 "Ari… "7.32"  "6.77"  "0.362" "13.23" "13.20" "0.970" "8.86"  "8.46"  "0.644"
    ## 10 "Ark… "7.31"  "7.78"  "0.446" "11.95" "12.72" "0.411" "9.52"  "10.27" "0.415"
    ## # … with 47 more rows, 3 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`18+(2013-2014)`, ²​`18+(2014-2015)`, ³​`18+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`

This has extracted all of the tables on the original page; that’s why we
have a list with 15 elements. (We haven’t really talked about lists yet,
but for now you can think of them as a general collection of objects in
R. As we proceed, syntax for extracting individual elements from a list
will become clear, and we’ll talk lots about lists in list columns.)

We’re only focused on the first table for now, so let’s get the contents
from the first list element.

``` r
table_marj = 
  drug_use_html %>%
  html_table() %>%
  first()
```

you’ll notice a problem: the “note” in the table appears in every column
in the first row. We need to remove that…

``` r
table_marj = 
  drug_use_html %>%
  html_table() %>%
  first() %>%
  slice(-1)

table_marj
```

    ## # A tibble: 56 × 16
    ##    State 12+(2…¹ 12+(2…² 12+(P…³ 12-17…⁴ 12-17…⁵ 12-17…⁶ 18-25…⁷ 18-25…⁸ 18-25…⁹
    ##    <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ##  1 Tota… 12.90a  13.36   0.002   13.28b  12.86   0.063   31.78   32.07   0.369  
    ##  2 Nort… 13.88a  14.66   0.005   13.98   13.51   0.266   34.66a  36.45   0.008  
    ##  3 Midw… 12.40b  12.76   0.082   12.45   12.33   0.726   32.13   32.20   0.900  
    ##  4 South 11.24a  11.64   0.029   12.02   11.88   0.666   28.93   29.20   0.581  
    ##  5 West  15.27   15.62   0.262   15.53a  14.43   0.018   33.72   33.19   0.460  
    ##  6 Alab… 9.98    9.60    0.426   9.90    9.71    0.829   26.99   26.13   0.569  
    ##  7 Alas… 19.60a  21.92   0.010   17.30   18.44   0.392   36.47a  40.69   0.015  
    ##  8 Ariz… 13.69   13.12   0.364   15.12   13.45   0.131   31.53   31.15   0.826  
    ##  9 Arka… 11.37   11.59   0.678   12.79   12.14   0.538   26.53   27.06   0.730  
    ## 10 Cali… 14.49   15.25   0.103   15.03   14.11   0.190   33.69   32.72   0.357  
    ## # … with 46 more rows, 6 more variables: `26+(2013-2014)` <chr>,
    ## #   `26+(2014-2015)` <chr>, `26+(P Value)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, `18+(P Value)` <chr>, and abbreviated variable
    ## #   names ¹​`12+(2013-2014)`, ²​`12+(2014-2015)`, ³​`12+(P Value)`,
    ## #   ⁴​`12-17(2013-2014)`, ⁵​`12-17(2014-2015)`, ⁶​`12-17(P Value)`,
    ## #   ⁷​`18-25(2013-2014)`, ⁸​`18-25(2014-2015)`, ⁹​`18-25(P Value)`

## CSS Selectors

Suppose we’d like to scrape the data about the Star Wars Movies from the
IMDB page. The first step is the same as before – we need to get the
HTML.

``` r
star_wars_html = 
  read_html("https://www.imdb.com/list/ls070150896/")
```

For each element, I’ll use the CSS selector in html_elements() to
extract the relevant HTML code, and convert it to text. Then I can
combine these into a data frame.

``` r
title_vec = 
  star_wars_html %>%
  html_elements(".lister-item-header a") %>%
  html_text()

gross_rev_vec = 
  star_wars_html %>%
  html_elements(".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

runtime_vec = 
  star_wars_html %>%
  html_elements(".runtime") %>%
  html_text()

star_wars_df = 
  tibble(
    title = title_vec,
    rev = gross_rev_vec,
    runtime = runtime_vec)

star_wars_df
```

    ## # A tibble: 9 × 3
    ##   title                                          rev      runtime
    ##   <chr>                                          <chr>    <chr>  
    ## 1 Star Wars: Episode I - The Phantom Menace      $474.54M 136 min
    ## 2 Star Wars: Episode II - Attack of the Clones   $310.68M 142 min
    ## 3 Star Wars: Episode III - Revenge of the Sith   $380.26M 140 min
    ## 4 Star Wars                                      $322.74M 121 min
    ## 5 Star Wars: Episode V - The Empire Strikes Back $290.48M 124 min
    ## 6 Star Wars: Episode VI - Return of the Jedi     $309.13M 131 min
    ## 7 Star Wars: Episode VII - The Force Awakens     $936.66M 138 min
    ## 8 Star Wars: Episode VIII - The Last Jedi        $620.18M 152 min
    ## 9 Star Wars: The Rise Of Skywalker               $515.20M 141 min

## Using an API

New York City has a great open data resource, and we’ll use that for our
API examples. Although most (all?) of these datasets can be accessed by
clicking through a website, we’ll access them directly using the API to
improve reproducibility and make it easier to update results to reflect
new data.

As a simple example, this page is about a dataset for annual water
consumption in NYC, along with the population in that year. First, we’ll
import this as a CSV and parse it.

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>%
  content("parsed")
```

    ## Rows: 43 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): year, new_york_city_population, nyc_consumption_million_gallons_per...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Data.gov also has a lot of data available using their API; often this is
available as CSV or JSON as well. For example, we might be interested in
data coming from BRFSS. This is importable via the API as a CSV (JSON,
in this example, is more complicated).

``` r
brfss_smart2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv", query = list("$limit" = 5000)) %>%
  content("parsed")
```

    ## Rows: 5000 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): locationabbr, locationdesc, class, topic, question, response, data...
    ## dbl  (6): year, sample_size, data_value, confidence_limit_low, confidence_li...
    ## lgl  (1): locationid
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

By default, the CDC API limits data to the first 1000 rows. Here I’ve
increased that by changing an element of the API query – I looked around
the website describing the API to find the name of the argument, and
then used the appropriate syntax for GET. To get the full data, I could
increase this so that I get all the data at once or I could try
iterating over chunks of a few thousand rows.

To get a sense of how this becomes complicated, let’s look at the
Pokemon API (which is also pretty nice).

``` r
poke = 
  GET("https://pokeapi.co/api/v2/pokemon/1") %>%
  content()

poke$name
```

    ## [1] "bulbasaur"

``` r
poke$height
```

    ## [1] 7

``` r
poke$abilities
```

    ## [[1]]
    ## [[1]]$ability
    ## [[1]]$ability$name
    ## [1] "overgrow"
    ## 
    ## [[1]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/65/"
    ## 
    ## 
    ## [[1]]$is_hidden
    ## [1] FALSE
    ## 
    ## [[1]]$slot
    ## [1] 1
    ## 
    ## 
    ## [[2]]
    ## [[2]]$ability
    ## [[2]]$ability$name
    ## [1] "chlorophyll"
    ## 
    ## [[2]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/34/"
    ## 
    ## 
    ## [[2]]$is_hidden
    ## [1] TRUE
    ## 
    ## [[2]]$slot
    ## [1] 3

To build a Pokemon dataset for analysis, you’d need to distill the data
returned from the API into a useful format; iterate across all pokemon;
and combine the results.

For both of the API examples we saw today, it wouldn’t be terrible to
just download the CSV, document where it came from carefully, and move
on. APIs are more helpful when the full dataset is complex and you only
need pieces, or when the data are updated regularly
