Analysis of Censored Secchi Depth Data from Casco Bay Monitoring
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
3/03/2021

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder Reference](#establish-folder-reference)
    -   [Primary Data](#primary-data)
        -   [Remove Data Only Available in
            2020](#remove-data-only-available-in-2020)
    -   [Add Station Names](#add-station-names)
    -   [Address Secchi Censored
        Values](#address-secchi-censored-values)
    -   [Create Recent Data](#create-recent-data)
    -   [Create Trend Data](#create-trend-data)
-   [Managing Censored Data with Maximum
    Likelihood](#managing-censored-data-with-maximum-likelihood)
    -   [Log Likelihood Function](#log-likelihood-function)
    -   [Chose a Station with Censoring to test this
        on](#chose-a-station-with-censoring-to-test-this-on)
-   [Applying to all ponds with “Secchi on Bottom”
    data](#applying-to-all-ponds-with-secchi-on-bottom-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This Notebook analyzes Secchi depth data received from FOCB’s “Surface”
data. These data are pulled from long term monitoring locations around
the Bay. This reflects only a small portion of FOCB’s monitoring
program, but it is the program with the deepest historical record.

Secchi Depth data provides a good estimate of water clarity, but the
data can be biased by inability to observe Secchi depths when the water
is shallower than where the Secchi disk disappears in the water column.
Is is important to realize that a Secchi disk on the bottom is NOT the
same as a lack of data - -after all, you know the water was at least
clear enough to see the disk that far down.

# Load Libraries

``` r
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.0.5
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.1     v dplyr   1.0.5
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> Warning: package 'tibble' was built under R version 4.0.5
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> Warning: package 'forcats' was built under R version 4.0.5
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)

library(maxLik)
#> Warning: package 'maxLik' was built under R version 4.0.5
#> Loading required package: miscTools
#> 
#> Please cite the 'maxLik' package as:
#> Henningsen, Arne and Toomet, Ott (2011). maxLik: A package for maximum likelihood estimation in R. Computational Statistics 26(3), 443-458. DOI 10.1007/s00180-010-0217-1.
#> 
#> If you have questions, suggestions, or comments regarding the 'maxLik' package, please use a forum or 'tracker' at maxLik's R-Forge site:
#> https://r-forge.r-project.org/projects/maxlik/

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

the_data <- read_excel(fpath, skip=2, col_names = mynames)
rm(mynames)
```

### Remove Data Only Available in 2020

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
  rename(station = Station_ID)

the_data <- the_data %>%
  left_join(loc_data, by = 'station') %>%
  rename(station_name = Station_Name) %>%
  relocate(station_name, .after = station) %>%
  relocate(year, .after = dt) %>%
  relocate(month, .after = year)
```

## Address Secchi Censored Values

``` r
the_data <- the_data %>%
  mutate(secchi_2 = if_else(secchi == "BSV", water_depth, as.numeric(secchi)),
         bottom_flag = secchi == "BSV") %>%
  filter(! is.na(secchi)) %>%
  relocate(secchi_2, .after = secchi) %>%
  relocate(bottom_flag, .after = secchi_2)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

## Create Recent Data

We filter to the last five FULL years of data, 2015 through 2019.

``` r
recent_data <- the_data %>%
  filter(year > 2014 & year < 2020) %>%
  mutate(station = factor(station))
```

## Create Trend Data

``` r
years_data <- the_data %>%
  group_by(station, year) %>%
  summarize(yes = ! all(is.na(temperature)),
            .groups = 'drop_last') %>%
  summarize(years = sum(yes, na.rm = TRUE),
            recent_years =  sum(yes & year > 2014, na.rm = TRUE),
            .groups = 'drop')
  
```

``` r
selected_stations <- years_data %>%
  filter(years> 9, recent_years >2) %>%
  pull(station)
```

``` r
trend_data <- the_data %>%
  filter(station %in% selected_stations)
rm(selected_stations, years_data)
```

``` r
ggplot(recent_data, 
       aes(x = fct_reorder(station_name, secchi_2, .fun = mean, na.rm = TRUE),
           y = secchi_2)) +
  #geom_violin() +
  geom_jitter(aes(color = bottom_flag ), width = 0.2, height = 0, 
              alpha = 0.5, size = 2) +
  scale_color_manual(values = cbep_colors(), name = '', labels = c('Observed', 'Disk on Bottom')) +
 
  coord_flip() +
  theme_cbep(base_size = 12) +
  ylab('Secchi Depth (m)') +
  xlab ('') +
  theme(legend.position = c(.805,.15),
        legend.title = element_blank())
```

<img src="Secchi_Censored_Analysis_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

So, the complication we run into here, compared to dealing with this in
lakes, is that the tides are a factor. Censored values are infrequent,
except at a couple of sites, where water is shallow anyway.

# Managing Censored Data with Maximum Likelihood

There are several locations with a high proportion of Secchi
observations censored. We can try to make use of the FACT that
observations are censored to better estimate typical water clarity
conditions.

For most sampling stations, it’s not clear what the advantage of a full
censored analysis may be over simply reporting medians might be. for a
FEW sites, censored observations are not observed under shallow Secchi
conditions, suggesting Secchi depth may be related to tides. If that’s
the case, reporting medians may still be inappropriate.

Estimated average Secchi values are only a little more informative than
the medians, except that they can give “credit” to sites where higher
values are not possible, if one is willing to assume the data are (more
or less) normally distributed within each site.

The violin plots, above, suggest that’s normality is not a dreadful
assumption for most Stations, although some show inordinately long tails
and a few show some skew.

Still, it is possible to use maximum likelihood methods to estimate what
the Secchi depth “would have been” if they had not been censored.

## Log Likelihood Function

We start by defining a log likelihood function that: 1. Assumes normally
distributed data, and 2. Calculates the log likelihood for each
observation as a. the probability associated with being ABOVE the
current observation (if data is censored); and b. The probability of the
observed value, under an assumption of normality, (if the data is not
censored).

``` r
secchi.loglik <-function (params, value, flag)
    {
    mu    <- params[[1]]
    sigma <- params[[2]]
    
    if (sigma<0) return(NA)
    
    ll <- sum(if_else(flag,
                  pnorm(value, mu,sigma, log.p = TRUE, lower.tail = FALSE),  # Total density above DL
                  dnorm(value, mu, sigma, log=TRUE)) )     # Density for other obs
    return(ll)
}
```

## Chose a Station with Censoring to test this on

We’ll go with ‘EEB18’, because it has a history of censored
observations. A bit more than a third of all recent observations are
censored.

``` r
ee.data <- recent_data %>%
  filter(station == 'EEB18') %>%
  filter(! is.na(secchi_2))
summary(ee.data$secchi_2)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.350   1.100   1.500   1.527   1.800   3.500
sum(ee.data$bottom_flag, na.rm = TRUE)
#> [1] 28
length(ee.data[[1]])
#> [1] 69
```

``` r
test <- maxLik(secchi.loglik, start=c(mu=3,sigma=2), value=ee.data$secchi_2, flag=ee.data$bottom_flag)
test
#> Maximum Likelihood estimation
#> Newton-Raphson maximisation, 6 iterations
#> Return code 1: gradient close to zero (gradtol)
#> Log-Likelihood: -38.35818 (2 free parameter(s))
#> Estimate(s): 1.870003 0.5111821
```

So that increases the estimated mean Secchi depth by about 0.35 meters,
or about one foot. That’s probably enough to matter.

# Applying to all ponds with “Secchi on Bottom” data

``` r
flagged_stations <- recent_data %>%
  filter(! is.na(secchi_2)) %>%
  group_by(station) %>%
  summarize(pctCensored = sum(bottom_flag)/ sum(! is.na(secchi_2)))  %>%
  filter(pctCensored>0)
flagged_stations
#> # A tibble: 12 x 2
#>    station pctCensored
#>    <fct>         <dbl>
#>  1 BMR02        0.0286
#>  2 EEB18        0.406 
#>  3 HR2          0.0152
#>  4 HR4          0.25  
#>  5 MPL86        0.0351
#>  6 NMM79        0.0299
#>  7 OBY35        0.0577
#>  8 PRV70        0.0465
#>  9 PYC43        0.227 
#> 10 RRC46        0.116 
#> 11 SMT50        0.141 
#> 12 STR54        0.217
```

So, for most stations, censoring is rare. it only really matters at a
handful of stations. (Here, stations where at least 10% of observations
were censored).

``` r
flagged_stations %>% 
  filter(pctCensored > 0.1) %>%
  pull(station)
#> [1] EEB18 HR4   PYC43 RRC46 SMT50 STR54
#> 23 Levels: BMR02 CMS1 EEB18 HR1 HR2 HR4 KVL84 MPL86 NMM79 OBY35 P5BSD ... STR54
```

For most of those, correcting estimated mean Secchi depths for censored
values (as opposed to simply using observed depth as a surrogate for
Secchi depth) has little impact on estimated means.

As censored values are consistently less than 50% of observations, the
impact of censoring on medians is likely to be small. Unlike in lakes,
however, we can not guarantee that it has no impact on medians, as
observations are collected over a wide tidal range.

``` r
flagged_stations <- flagged_stations %>%
  pull(station)
```

``` r
for (site in flagged_stations) {
  cat('\n')
  cat(loc_data$Station_Name[loc_data$station == site])
  cat('\n')
  
  ld <- recent_data[recent_data$station == site,]
  ld <- ld[! is.na(ld$secchi_2),]
  cat(paste('Sample =', length(ld[[1]]), '\n'))
  cat(paste('Number Censored =', sum(ld$bottom_flag), '\n'))
  
  cat(paste('Median =', median(ld$secchi_2), '\n'))
  cat(paste('Mean =', mean(ld$secchi_2), '\n'))
  cat(paste('SD =', sd(ld$secchi_2), '\n'))
  
  
  test <- maxLik(secchi.loglik, start=c(mu=3,sigma=2), 
                 value=ld$secchi_2, flag=ld$bottom_flag)
  cat(paste('Adj. Mean =', test$estimate[1], '\n'))
  cat(paste('Adj. SD =', test$estimate[2], '\n'))
  cat(paste('Change =', test$estimate[1] - mean(ld$secchi_2), '\n'))
  
}
#> 
#> B&M Railroad /Back Cove
#> Sample = 70 
#> Number Censored = 2 
#> Median = 2.1 
#> Mean = 2.07928571428571 
#> SD = 0.500035713010295 
#> Adj. Mean = 2.08626767506768 
#> Adj. SD = 0.508155411069726 
#> Change = 0.00698196078196966 
#> 
#> East End Beach
#> Sample = 69 
#> Number Censored = 28 
#> Median = 1.5 
#> Mean = 1.52717391304348 
#> SD = 0.588324814160105 
#> Adj. Mean = 1.87000294747469 
#> Adj. SD = 0.511182092296595 
#> Change = 0.342829034431208 
#> 
#> Harraseeket-South Freeport
#> Sample = 66 
#> Number Censored = 1 
#> Median = 1.55 
#> Mean = 1.64848484848485 
#> SD = 0.447296984200198 
#> Adj. Mean = 1.65939718003243 
#> Adj. SD = 0.439191577170926 
#> Change = 0.0109123315475845 
#> 
#> Upper Harraseeket
#> Sample = 20 
#> Number Censored = 5 
#> Median = 0.8 
#> Mean = 0.96 
#> SD = 0.986434301704349 
#> Adj. Mean = 1.25144904674048 
#> Adj. SD = 1.01917881611053 
#> Change = 0.291449046740485 
#> 
#> Mere Point Landing
#> Sample = 57 
#> Number Censored = 2 
#> Median = 1.5 
#> Mean = 1.60701754385965 
#> SD = 0.452319280851009 
#> Adj. Mean = 1.6302810632612 
#> Adj. SD = 0.441381496923687 
#> Change = 0.0232635194015471 
#> 
#> New Meadows Marina
#> Sample = 67 
#> Number Censored = 2 
#> Median = 1.4 
#> Mean = 1.41791044776119 
#> SD = 0.414482288664381 
#> Adj. Mean = 1.42518777739629 
#> Adj. SD = 0.420353564550961 
#> Change = 0.00727732963509742 
#> 
#> Orrs Bailey Yacht Club
#> Sample = 52 
#> Number Censored = 3 
#> Median = 2.7 
#> Mean = 2.74038461538462 
#> SD = 0.504978980492322 
#> Adj. Mean = 2.75536940524575 
#> Adj. SD = 0.524412652207969 
#> Change = 0.0149847898611322 
#> 
#> Presumpscot River
#> Sample = 43 
#> Number Censored = 2 
#> Median = 1.6 
#> Mean = 1.54418604651163 
#> SD = 0.391153559896509 
#> Adj. Mean = 1.5593136827182 
#> Adj. SD = 0.39549664420578 
#> Change = 0.0151276362065766 
#> 
#> Falmouth Town Landing
#> Sample = 66 
#> Number Censored = 15 
#> Median = 2.5 
#> Mean = 2.44090909090909 
#> SD = 0.676638131381288 
#> Adj. Mean = 2.63880767981375 
#> Adj. SD = 0.632420467983376 
#> Change = 0.197898588904663 
#> 
#> Lower Royal River
#> Sample = 69 
#> Number Censored = 8 
#> Median = 1.1 
#> Mean = 1.21666666666667 
#> SD = 0.42014236616073 
#> Adj. Mean = 1.25816602093347 
#> Adj. SD = 0.442436644790748 
#> Change = 0.0414993542668036 
#> 
#> SMCC Pier
#> Sample = 92 
#> Number Censored = 13 
#> Median = 2 
#> Mean = 1.99239130434783 
#> SD = 0.441583985926631 
#> Adj. Mean = 2.05838466165997 
#> Adj. SD = 0.456708042852155 
#> Change = 0.0659933573121432 
#> 
#> Upper Fore River
#> Sample = 46 
#> Number Censored = 10 
#> Median = 0.8 
#> Mean = 0.91304347826087 
#> SD = 0.54553081006878 
#> Adj. Mean = 1.06273628765411 
#> Adj. SD = 0.544563330432886 
#> Change = 0.149692809393236
```

Looking at those results, the differences are quantitatively important
(more than a 10cm difference) at only the sites with the highest levels
of censoring:

| Station               | Station Code |
|-----------------------|--------------|
| East End Beach        | EEB18        |
| Upper Harraseeket     | HR4          |
| Falmouth Town Landing | PYC43        |
| Upper Fore River      | STR54        |
