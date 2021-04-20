Analysis of Surface Data from Friends of Casco Bay Monitoring
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
3/03/2021

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder Reference](#establish-folder-reference)
    -   [Primary Data](#primary-data)
        -   [Remove 2020 only data](#remove-2020-only-data)
    -   [Add Station Names](#add-station-names)
    -   [Address Secchi Censored
        Values](#address-secchi-censored-values)
    -   [Transform Secchi and Chlorophyll
        Data](#transform-secchi-and-chlorophyll-data)
-   [Recent Conditions](#recent-conditions)
    -   [Create Recent Data](#create-recent-data)
    -   [Summary Statistics](#summary-statistics)
    -   [Create Nested Tibble](#create-nested-tibble)
-   [Linear Models](#linear-models)
    -   [Review Model Results](#review-model-results)
        -   [ANOVA](#anova)
        -   [Diagnostic Plots](#diagnostic-plots)
-   [Options for More Complex Models](#options-for-more-complex-models)
    -   [Exploratory Graphics](#exploratory-graphics)
        -   [Create Long Form Data](#create-long-form-data)
        -   [Month to Month Plot](#month-to-month-plot)
    -   [Hierarchical Models](#hierarchical-models)
        -   [Review Model Results](#review-model-results-1)
        -   [Refit the Chlorophyll Model](#refit-the-chlorophyll-model)
        -   [ANOVAS](#anovas)
-   [Associations with Temperature](#associations-with-temperature)
    -   [Hierarchical Model](#hierarchical-model)
    -   [Correlations of Station Medians with
        Temperature](#correlations-of-station-medians-with-temperature)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This Notebook analyzes FOCB’s “Surface” data. These data are pulled from
long term monitoring locations around the Bay.

These are sites visited regularly by FOCB staff, either by boat or on
land. The focus is on warm season sampling (April through October), with
roughly monthly samples. Earlier data from some land-based sites was
collected by volunteers.

This reflects only a small portion of FOCB’s monitoring program, but the
surface data provides consistent sampling history with the deepest
historical record.

# Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.0.5     v dplyr   1.0.3
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.0
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)

library(mgcv)     # For `gam()` and `gamm()` models
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
library(emmeans)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Load Data

## Establish Folder Reference

``` r
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Primary Data

We specify column names because FOCB data has a row of names, a row of
units, then the data. This approach is simpler than reading names from
the first row and correcting them to be R syntactic names.

``` r
fn    <- 'FOCB Surface All Current Sites With BSV Data.xlsx'
fpath <- file.path(sibling,fn)

mynames <- c('station', 'dt', 'time', 'sample_depth',
             'secchi', 'water_depth','temperature', 'salinity',
             'do', 'pctsat', 'pH', 'chl', 
             'month', 'year', 'fdom', 'bga', 
             'turbidity', 'blank', 'clouds', 'wndspd',
             'winddir'
             ) 

the_data <- read_excel(fpath, skip=2, col_names = mynames) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb))

rm(mynames)
```

### Remove 2020 only data

``` r
the_data <- the_data %>%
select(-c(fdom:winddir))
```

## Add Station Names

``` r
fn    <- 'FOCB Monitoring Sites.xlsx'
fpath <- file.path(sibling,fn)
loc_data <- read_excel(fpath) %>%
  select(Station_ID, Station_Name) %>%
  rename(station = Station_ID,
         station_name = Station_Name)

the_data <- the_data %>%
  left_join(loc_data, by = 'station') %>%
  relocate(station_name, .after = station) %>%
  
  relocate(year, .after = dt) %>%
  relocate(month, .after = year)
```

Our data contains two stations that are not associated with locations
that were included in our spatial data. We can see that because when we
`left_join()` by `station`, no `station_name` value is carried over.

``` r
l <- the_data %>%
  group_by(station) %>%
  summarize(missing = sum(is.na(station_name))) %>%
  filter(missing > 0) %>%
  pull(station)
l
#> [1] "CMS3"  "P6CBI"
```

If we look at those records, on is represented by only a single
observation, and the other only by data from 2020. Neither matter for
the current analysis. They will get filtered out when we select data to
describe recent conditions, and trends.

``` r
the_data %>%
  filter(station %in% l)
#> # A tibble: 7 x 15
#>   station station_name dt                   year month time               
#>   <chr>   <chr>        <dttm>              <dbl> <fct> <dttm>             
#> 1 P6CBI   <NA>         2006-09-13 00:00:00  2006 Sep   1899-12-31 11:40:00
#> 2 CMS3    <NA>         2020-06-17 00:00:00  2020 Jun   1899-12-31 11:22:54
#> 3 CMS3    <NA>         2020-07-15 00:00:00  2020 Jul   1899-12-31 11:08:08
#> 4 CMS3    <NA>         2020-07-30 00:00:00  2020 Jul   1899-12-31 11:39:33
#> 5 CMS3    <NA>         2020-08-19 00:00:00  2020 Aug   1899-12-31 11:36:38
#> 6 CMS3    <NA>         2020-09-17 00:00:00  2020 Sep   1899-12-31 11:52:14
#> 7 CMS3    <NA>         2020-10-05 00:00:00  2020 Oct   1899-12-31 11:23:21
#> # ... with 9 more variables: sample_depth <dbl>, secchi <chr>,
#> #   water_depth <dbl>, temperature <dbl>, salinity <dbl>, do <dbl>,
#> #   pctsat <dbl>, pH <dbl>, chl <dbl>
```

## Address Secchi Censored Values

``` r
the_data <- the_data %>%
  mutate(secchi_2 = if_else(secchi == "BSV", water_depth, as.numeric(secchi)),
         bottom_flag = secchi == "BSV") %>%
  relocate(secchi_2, .after = secchi) %>%
  relocate(bottom_flag, .after = secchi_2)
#> Warning: Problem with `mutate()` input `secchi_2`.
#> i NAs introduced by coercion
#> i Input `secchi_2` is `if_else(secchi == "BSV", water_depth, as.numeric(secchi))`.
```

## Transform Secchi and Chlorophyll Data

We create a log plus one transformed version of the Chlorophyll data
here, to facilitate “parallel” construction of statistical models. That
transform regularizes variances and linearizes response fairly well. We
also develop a square root transform of the Secchi data, for similar
purposes, but the results are not as effective.

``` r
the_data <- the_data %>%
  mutate(sqrt_secchi = sqrt(secchi_2),
         log_chl = log(chl),
         log1_chl = log1p(chl)) %>%
  mutate(log_chl = if_else(is.infinite(log_chl) | is.nan(log_chl),
                           NA_real_, log_chl)) %>%
  relocate(sqrt_secchi, .after = secchi_2) %>%
  relocate(log_chl, log1_chl, .after = chl)
#> Warning: Problem with `mutate()` input `log_chl`.
#> i NaNs produced
#> i Input `log_chl` is `log(chl)`.
```

# Recent Conditions

In 2015, we presented marginal means and standard errors for sixteen
different regions of the Bay. This time, we have fewer monitoring
stations, and present results for each monitoring location individually.

As in 2015, we organize results by mean or median temperature, as
offshore sites or sites strongly influenced by offshore waters do not
get as warm in summer months as inshore sites.

## Create Recent Data

We filter to the last five FULL years of data, 2015 through 2019.

``` r
recent_data <- the_data %>%
  filter(year > 2014 & year < 2020) %>%
  mutate(station = fct_reorder(station, temperature, mean, na.rm = TRUE),
         station_name = fct_reorder(station_name, temperature, mean, na.rm = TRUE))
```

## Summary Statistics

We want an exported data table for GIS, containing useful summary
statistics, especially medians and means. We will probably map median
values, as they are less affected by the heavy-tailed distributions. We
also use a portion of these data later to test which station median
characteristics are correlated with median temperature.

``` r
sum_data <- recent_data %>%
  select(-dt, -year, -time, -sample_depth, 
         -secchi, - bottom_flag) %>%
  relocate(water_depth, .after = month) %>%
  group_by(station) %>%
  summarize(across(c(secchi_2:log1_chl), list(mn =  ~ mean(.x, na.rm = TRUE),
                                         med = ~ median(.x, na.rm = TRUE))))

sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)

fn    <- 'station_summary.csv'
fpath <- file.path(sibling,fn)

fpath = 
write_csv(sum_data, fpath, na = '')
```

## Create Nested Tibble

To run parallel analyses, we reorganize the data into a nested tibble.
We also add a list of labels and measurement units to simplify labeling
of plots.

``` r
units <- tibble(parameter = c('secchi_2', 'sqrt_secchi', 'temperature', 
                              'salinity', 'do',
                              'pctsat', 'pH', 
                              'chl', 'log_chl', 'log1_chl'),
                label = c("Secchi Depth", "Sqt Secchi", "Temperature",
                         "Salinity", "Dissolved Oxygen",
                         "Percent Saturation", "pH",
                         "Chlorophyll A", "Log Chlorophyll A", "Log Chlorophyll A plus 1"),
                units = c('m', '', paste0("\U00B0", "C"),
                          'PSU', 'mg/l',
                          '', '',
                          'mg/l', '', ''))

nested_data <- recent_data %>%
  select(-dt, -time, -sample_depth, 
         -secchi, - bottom_flag) %>%
  mutate(year_f = factor(year)) %>%
  relocate(water_depth, .after = month) %>%
  pivot_longer(c(secchi_2:log1_chl), names_to = 'parameter', values_to = 'value') %>%
  filter(! is.na(value)) %>%
  group_by(parameter) %>%
  nest() %>%
  left_join(units, by = 'parameter')
```

# Linear Models

``` r
nested_data <- nested_data %>%
  mutate(lms = map(data, function(df) lm(value ~ station, data = df)))
```

## Review Model Results

### ANOVA

``` r
for (p in nested_data$parameter) {
  cat(p)
  cat('\n')
  print(anova(nested_data$lms[nested_data$parameter == p][[1]]))
  cat('\n\n')
}
#> secchi_2
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df Sum Sq Mean Sq F value    Pr(>F)    
#> station     22 420.11 19.0960  62.888 < 2.2e-16 ***
#> Residuals 1065 323.39  0.3037                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> sqrt_secchi
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df Sum Sq Mean Sq F value    Pr(>F)    
#> station     22 56.045  2.5475  64.995 < 2.2e-16 ***
#> Residuals 1065 41.743  0.0392                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> temperature
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df  Sum Sq Mean Sq F value    Pr(>F)    
#> station     22  3428.3 155.831  9.8645 < 2.2e-16 ***
#> Residuals 1172 18514.3  15.797                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> salinity
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df Sum Sq Mean Sq F value    Pr(>F)    
#> station     22  53237 2419.86  256.87 < 2.2e-16 ***
#> Residuals 1147  10806    9.42                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> do
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df  Sum Sq Mean Sq F value    Pr(>F)    
#> station     22  515.42 23.4280  13.926 < 2.2e-16 ***
#> Residuals 1157 1946.44  1.6823                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> pctsat
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df Sum Sq Mean Sq F value    Pr(>F)    
#> station     22  66006 3000.27   19.19 < 2.2e-16 ***
#> Residuals 1142 178550  156.35                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> pH
#> Analysis of Variance Table
#> 
#> Response: value
#>             Df Sum Sq Mean Sq F value    Pr(>F)    
#> station     22 32.094 1.45882  29.493 < 2.2e-16 ***
#> Residuals 1040 51.442 0.04946                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> chl
#> Analysis of Variance Table
#> 
#> Response: value
#>            Df Sum Sq Mean Sq F value    Pr(>F)    
#> station    21 1323.7  63.032  3.0981 7.247e-06 ***
#> Residuals 391 7955.1  20.346                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> log_chl
#> Analysis of Variance Table
#> 
#> Response: value
#>            Df Sum Sq Mean Sq F value    Pr(>F)    
#> station    21  42.46 2.02188  2.6101 0.0001618 ***
#> Residuals 388 300.56 0.77465                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> log1_chl
#> Analysis of Variance Table
#> 
#> Response: value
#>            Df  Sum Sq Mean Sq F value    Pr(>F)    
#> station    21  21.236 1.01122  3.1136 6.552e-06 ***
#> Residuals 391 126.989 0.32478                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

As suspected, all parameters differ among sites somewhere.

### Diagnostic Plots

``` r
for (p in nested_data$parameter) {
  plot(nested_data$lms[nested_data$parameter == p][[1]],
       sub.caption = p)
}
```

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-2.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-3.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-4.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-5.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-6.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-7.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-8.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-9.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-10.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-11.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-12.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-13.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-14.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-15.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-16.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-17.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-18.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-19.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-20.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-21.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-22.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-23.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-24.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-25.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-26.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-27.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-28.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-29.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-30.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-31.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-32.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-33.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-34.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-35.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-36.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-37.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-38.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-39.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-12-40.png" style="display: block; margin: auto;" />

So…. \* Secchi has moderate heavy tails, with weak evidence of a
scale-location relationship. Square root of Secchi is not much better.
\* Temperature is left skewed.  
\* Salinity has VERY heavy tails, especially at the lower end.  
\* DO has slight skew, with a heavy upper tail. that is, it’s skewed
right.  
\* Percent Saturation is slightly heavy tailed, with weak evidence of
scale-location relationships.  
\* pH is heavy tailed and higher error at lower predicted pH (sites). \*
chlorophyll has a couple of outliers with moderate leverage. It’s
probably heavy-tailed too. We see the best model there is on the log
plus one transformed chlorophyll data.

# Options for More Complex Models

We could in principal adjust models for different sampling histories
(years and months) but there is likely little value to doing so as the
sampling histories are fairly uniform, with the exception of a few sites
added recently.

More seriously, these are time series. The measurements are not strictly
periodic, and the interval between successive observations is large.
Autocorrelation in the raw data is likely substantial because of
seasonal patterns, but observations collected three weeks or a month
apart are unlikely to be mechanistically correlated, once we take into
account station, season, and year.

We should consider interannual and month to month (day of year?)
variability, and whether there is any advantage to modeling those
variations.

## Exploratory Graphics

### Create Long Form Data

We create long-form data to facilitate faceted graphics.

``` r
long_data <- recent_data %>%
  select(-time, -sample_depth, 
         -secchi, - bottom_flag) %>%
  relocate(water_depth, .after = month) %>%
  pivot_longer(c(secchi_2:log1_chl), names_to = 'parameter', values_to = 'value') %>%
  filter(! is.na(value))
```

### Month to Month Plot

``` r
for (p in unique(long_data$parameter)) {
  plt <- long_data %>%
    filter(parameter == p) %>%
    ggplot(aes(x = month, y = value)) +
    geom_jitter(aes(color = year), alpha = 0.5, width = 0.2, height = 0) +
    ylab(p) +
    facet_wrap(~station) +
    theme_cbep() +
    theme(axis.text.x = element_text(angle = 90))
  print(plt)
}
```

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-2.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-3.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-4.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-5.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-6.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-7.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-8.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-9.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-14-10.png" style="display: block; margin: auto;" />

Note the relatively limited data for Chlorophyll.

Several parameters show marked seasonal patterns. Year to year changes
are harder to discern. A few individual sites may show seasonal or year
to year variation even where that is not important across all sites, but
fitting that variation would be pushing our luck, as we are implicitly
reviewing a large number of parameters over a large number of sites,
thus triggering a multiple comparisons problem.

We may be able to use the month by month relations to slightly reduce
standard errors in our models. Such models probably have little value
for parameter estimation here, but they may have significant value in
identifying long-term trends.

## Hierarchical Models

We fit a year to year random factor and seasonal (monthly) terms. This
reflects our belief that each year sets up somewhat differently due to
weather, especially in the spring, but that some seasonal patterns
shuold be robust, and of potential direct interest to us.

``` r
nested_data <- nested_data %>%
  mutate(lmers = map(data, function(df) gam(value ~ station + month + 
                                              s(year_f, bs = 're'), 
                                            data = df)))
```

### Review Model Results

Interestingly, year to year variation is statistically important for all
parameters.

#### Diagnostic Plots

``` r
for (p in nested_data$parameter) {
  gam.check(nested_data$lmers[nested_data$parameter == p][[1]],
       sub = p)
}
```

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 7.988947e-08 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 3.27      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-2.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 1.404362e-07 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 3.09      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-3.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 6 iterations.
    #> The RMS GCV score gradient at convergence was 2.406938e-06 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 3.69      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-4.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 7 iterations.
    #> The RMS GCV score gradient at convergence was 2.990289e-05 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 3.79      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-5.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 7 iterations.
    #> The RMS GCV score gradient at convergence was 5.595035e-06 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 3.85      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-6.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 6 iterations.
    #> The RMS GCV score gradient at convergence was 0.0005747645 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 3.62      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-7.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 6 iterations.
    #> The RMS GCV score gradient at convergence was 2.417341e-07 .
    #> The Hessian was positive definite.
    #> Model rank =  34 / 34 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>            k' edf k-index p-value
    #> s(year_f) 5.0 3.5      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-8.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 5.387926e-07 .
    #> The Hessian was positive definite.
    #> Model rank =  33 / 33 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 1.62      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-9.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 2.061044e-06 .
    #> The Hessian was positive definite.
    #> Model rank =  33 / 33 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 2.61      NA      NA

<img src="Surface_Analysis_Recent_files/figure-gfm/unnamed-chunk-16-10.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 1.732709e-07 .
    #> The Hessian was positive definite.
    #> Model rank =  33 / 33 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>             k'  edf k-index p-value
    #> s(year_f) 5.00 2.88      NA      NA

Most of those look pretty good, with the exception of salinity, which
looks fairly awful, and pH, which is merely bad.

Square root of Secchi depth is marginally better than raw Secchi depth,
but the difference is small, and it complicates interpretation later.

Log 1 Chlorophyll is better than either untransformed chlorophyll or log
of chlorophyll.

### Refit the Chlorophyll Model

We want the transformation in the model object, so we can use the tools
in `emmeans` to extract marginal means. We refit the chlorophyll model,
add it to the nested tibble, and delete the other two chlorophyll data
rows and the square root transformed Secchi depth row.

``` r
df <- nested_data %>%
  filter(parameter == 'chl') %>%
  pull(data)
df <- df[[1]]  # Extract the first item in the list....

mod <- gam(log1p(value) ~ station + month +  s(year_f, bs = 're'), data = df)
```

``` r
nested_data$lmers[nested_data$parameter == 'chl'] <- list(mod)

nested_data <- nested_data %>%
  filter(! parameter %in% c('log_chl', 'log1_chl', 'sqrt_secchi'))
```

### ANOVAS

``` r
for (p in nested_data$parameter) {
  cat(p)
  print(anova(nested_data$lmers[nested_data$parameter == p][[1]]))
  cat('\n\n')
}
#> secchi_2
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df     F p-value
#> station 22 67.24  <2e-16
#> month    6 21.28  <2e-16
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F  p-value
#> s(year_f) 3.273  4.000 3.979 0.000709
#> 
#> 
#> temperature
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df      F p-value
#> station 22  35.58  <2e-16
#> month    6 721.23  <2e-16
#> 
#> Approximate significance of smooth terms:
#>            edf Ref.df     F p-value
#> s(year_f) 3.69   4.00 11.63  <2e-16
#> 
#> 
#> salinity
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df      F p-value
#> station 22 337.52  <2e-16
#> month    6  48.55  <2e-16
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F p-value
#> s(year_f) 3.786  4.000 22.85  <2e-16
#> 
#> 
#> do
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df      F p-value
#> station 22  34.09  <2e-16
#> month    6 232.24  <2e-16
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F p-value
#> s(year_f) 3.849  4.000 29.46  <2e-16
#> 
#> 
#> pctsat
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df     F p-value
#> station 22 22.45  <2e-16
#> month    6 19.25  <2e-16
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F p-value
#> s(year_f) 3.624  4.000 12.15  <2e-16
#> 
#> 
#> pH
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df     F  p-value
#> station 22 31.18  < 2e-16
#> month    6 12.12 3.39e-13
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F p-value
#> s(year_f) 3.495  4.000 11.18  <2e-16
#> 
#> 
#> chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> log1p(value) ~ station + month + s(year_f, bs = "re")
#> 
#> Parametric Terms:
#>         df      F  p-value
#> station 21  3.791 7.68e-08
#> month    6 30.956  < 2e-16
#> 
#> Approximate significance of smooth terms:
#>             edf Ref.df     F p-value
#> s(year_f) 2.884  4.000 3.202 0.00192
```

Note that the conclusions are that ALL parameters vary seasonally and by
location. Year to year variation is also important for all parameters,
which is perhaps less obvious. (Note that this model did NOT fit
interaction terms, (although they are likely to be important for some
parameters).

None of that is likely to come as a surprise. Much of the value here is
to confirm our intuition.

Despite high levels of statistical significance for these models, the
models are not terribly appropriate for either salinity or pH data
because of residuals far from normally distributed

# Associations with Temperature

We emphasize in the text and presentation that temperature acts as an
important surrogate for the relative influence of offshore waters on
local conditions in the Bay. If that is the case, we should be able to
see high correlation with Temperature for the other parameters,
especially after accounting for other known covariates.

We take three approaches to check for the importance of temperature as a
predictor of other water quality parameters:

1.  We examine whether temperature is a significant predictor of other
    parameters in linear models where we treat station and year as
    random factors

2.  

We need to regenerate the long-form data, retaining temperature asa
predictor variable, not breaking it into a separate dataframe.

We create long-form data to facilitate faceted graphics.

``` r
 nested_data_2 <- recent_data %>%
  select(-dt, -time, -sample_depth, 
         -secchi, - bottom_flag) %>%
  mutate(year_f = factor(year)) %>%
  relocate(water_depth, temperature, .after = month) %>%
  pivot_longer(c(secchi_2:log1_chl), names_to = 'parameter', values_to = 'value') %>%
  filter(! is.na(value)) %>%
  group_by(parameter) %>%
  nest() %>%
  left_join(units, by = 'parameter')
 
```

## Hierarchical Model

``` r
nested_data_2 <- nested_data_2 %>%
  mutate(lmers_temp = map(data, function(df) gam(value ~  temperature +
                                              s(year_f, bs = 're') +
                                              s(station, bs = 're'), 
                                            data = df)))
```

``` r
for (parm in nested_data_2$parameter) {
  row = nested_data_2 %>%
    filter(parameter == parm)
  cat('\n\n')
  cat(parm)
  print(anova(row$lmers_temp[[1]]))
  }
#> 
#> 
#> secchi_2
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F  p-value
#> temperature  1 50.83 1.86e-12
#> 
#> Approximate significance of smooth terms:
#>              edf Ref.df     F p-value
#> s(year_f)   3.38   4.00 12.39 0.00742
#> s(station) 21.46  22.00 60.82 < 2e-16
#> 
#> 
#> sqrt_secchi
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F  p-value
#> temperature  1 45.89 2.07e-11
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F p-value
#> s(year_f)   3.245  4.000 11.00 0.00958
#> s(station) 21.472 22.000 62.04 < 2e-16
#> 
#> 
#> salinity
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F p-value
#> temperature  1 92.78  <2e-16
#> 
#> Approximate significance of smooth terms:
#>              edf Ref.df     F p-value
#> s(year_f)   3.67   4.00 217.9  <2e-16
#> s(station) 21.90  22.00 312.2  <2e-16
#> 
#> 
#> do
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F p-value
#> temperature  1 819.2  <2e-16
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F p-value
#> s(year_f)   3.681  4.000 23.48  <2e-16
#> s(station) 20.526 22.000 18.09  <2e-16
#> 
#> 
#> pctsat
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F p-value
#> temperature  1 5.682  0.0173
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F p-value
#> s(year_f)   3.612  4.000 21.10  <2e-16
#> s(station) 20.683 22.000 19.86  <2e-16
#> 
#> 
#> pH
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F  p-value
#> temperature  1 43.74 5.99e-11
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F  p-value
#> s(year_f)   3.495  4.000 17.70 1.18e-06
#> s(station) 20.626 22.000 24.12  < 2e-16
#> 
#> 
#> chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F  p-value
#> temperature  1 23.01 2.29e-06
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F  p-value
#> s(year_f)   1.818  4.000 2.071  0.00764
#> s(station) 14.144 21.000 1.893 8.32e-05
#> 
#> 
#> log_chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F  p-value
#> temperature  1 58.42 1.64e-13
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F  p-value
#> s(year_f)   2.449  4.000 2.875 0.007197
#> s(station) 13.495 21.000 1.732 0.000239
#> 
#> 
#> log1_chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ temperature + s(year_f, bs = "re") + s(station, bs = "re")
#> 
#> Parametric Terms:
#>             df     F  p-value
#> temperature  1 66.48 4.79e-15
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df    F  p-value
#> s(year_f)   2.613  4.000 3.65  0.00483
#> s(station) 15.090 21.000 2.45 6.35e-06
```

So all parameters show significant correlations with temperature,
treating Years and Stations as random factors.

But that’s not exactly the question we want to ask. We want to ask if
stations are structured by temperature, not if temperature correlates
with other parameters within (random) stations and years.

## Correlations of Station Medians with Temperature

We use Kendall’s Tau as a robust / resistant alternative to Pearson
Correlation Coefficient. A Spearman rank correlation would also be
appropriate.

``` r
tmp <- sum_data %>%
  select(station, contains('_med')) %>%
  rename_all(~sub('_med', '', .)) %>%
  relocate(temperature, .after = station)
  
for (parm in names(tmp)[3:11]) {
  cat('\n\n')
  cat(parm, '\n')
  print(cor.test(tmp$temperature, tmp[[parm]], method = "kendall"))
}
#> 
#> 
#> secchi_2
#> Warning in cor.test.default(tmp$temperature, tmp[[parm]], method = "kendall"):
#> Cannot compute exact p-value with ties
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> z = -2.968, p-value = 0.002998
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.4507781 
#> 
#> 
#> 
#> sqrt_secchi
#> Warning in cor.test.default(tmp$temperature, tmp[[parm]], method = "kendall"):
#> Cannot compute exact p-value with ties
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> z = -2.9652, p-value = 0.003025
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>       tau 
#> -0.448942 
#> 
#> 
#> 
#> salinity 
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> T = 102, p-value = 0.2074
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.1936759 
#> 
#> 
#> 
#> do 
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> T = 62, p-value = 0.0004437
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.5098814 
#> 
#> 
#> 
#> pctsat 
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> T = 78, p-value = 0.01008
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.3833992 
#> 
#> 
#> 
#> pH
#> Warning in cor.test.default(tmp$temperature, tmp[[parm]], method = "kendall"):
#> Cannot compute exact p-value with ties
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> z = -2.7586, p-value = 0.005805
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.4185797 
#> 
#> 
#> 
#> chl 
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> T = 173, p-value = 0.0008745
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>       tau 
#> 0.4978355 
#> 
#> 
#> 
#> log_chl 
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> T = 170, p-value = 0.001713
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>       tau 
#> 0.4718615 
#> 
#> 
#> 
#> log1_chl 
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp$temperature and tmp[[parm]]
#> T = 173, p-value = 0.0008745
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>       tau 
#> 0.4978355
```

-   Warmer waters have:
    -   Lower water clarity  
    -   Lower dissolved oxygen and percent saturation  
    -   Lower pH  
    -   Higher chlorophyll
