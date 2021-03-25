Analysis of Historic Chlorophyll Data from Friends of Casco Bay
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
3/25/2021

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder Reference](#establish-folder-reference)
    -   [Load Data](#load-data-1)
    -   [Add Day of Year Value](#add-day-of-year-value)
    -   [Transformations](#transformations)
-   [Analysis of Trends](#analysis-of-trends)
    -   [Construct Nested Tibble](#construct-nested-tibble)
    -   [Sanity Checks](#sanity-checks)
-   [Hierarchical models](#hierarchical-models)
    -   [Diagnostic Plots](#diagnostic-plots)
    -   [Compare Hierarchical Models](#compare-hierarchical-models)
        -   [Interaction Plots](#interaction-plots)
-   [GAM models](#gam-models)
-   [Clean Up `nested_data`](#clean-up-nested_data)
-   [Predictions with `emmeans()`](#predictions-with-emmeans)
    -   [Handling Transformations](#handling-transformations)
    -   [Lets see if this works…](#lets-see-if-this-works)
-   [Final Model Review](#final-model-review)
    -   [ANOVAs](#anovas)
    -   [Slopes](#slopes)
-   [Build Graphics](#build-graphics)
    -   [Handling Transformations](#handling-transformations-1)
    -   [Create Plotting Function](#create-plotting-function)
-   [Alternative Approaches to Addressing “Zero”
    Values](#alternative-approaches-to-addressing-zero-values)
    -   [Rebuild Data Frame](#rebuild-data-frame)
-   [Discussion](#discussion)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This Notebook analyzes a small subset of FOCB’s “Surface” data,
specifically looking at levels of chlorophyll A. Additional details of
these analyses are presented in the “Surface\_Analysis\_Trends.md”
RNotebook.

In developing those analyses, we ran into a problem generating usable
graphics for State of Casco BAy, when the Y axis is a transformed
variable.

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
# dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Load Data

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
  mutate(month = factor(month, levels = 1:12, labels = month.abb)) %>%
  select(station, dt, month, year, chl)

rm(mynames)
```

Frequent chlorophyll data is available since 2001 from three sites. We
restrict attention to them.

``` r
the_data <- the_data %>% 
  filter(station %in% c('P5BSD', 'P6FGG', 'P7CBI')) %>%
  mutate(station = factor(station))
```

## Add Day of Year Value

``` r
the_data <- the_data %>%
  mutate(doy = as.numeric(format(dt, '%j'))) %>%
  relocate(doy, .after = dt)
```

## Transformations

We create several generalized log transforms of the chloride data.

``` r
the_data <- the_data %>%
  mutate(log_chl = log(chl),
         log1_chl = log1p(chl),
         log5_chl = log(chl + 0.5),
         log25_chl = log(chl + 0.25)) %>%
  mutate(log_chl = if_else(is.infinite(log_chl) | is.nan(log_chl),
                           NA_real_, log_chl))
#> Warning: Problem with `mutate()` input `log_chl`.
#> i NaNs produced
#> i Input `log_chl` is `log(chl)`.
```

# Analysis of Trends

Our goal here is to identify whether there are long-term trends in
chlorophyll. This is problematic…

## Construct Nested Tibble

``` r
units <- tibble(parameter = c('chl', 'log_chl', 'log1_chl',
                              'log5_chl', 'log25_chl'),
                label = c("Chlorophyll A", 
                          "Log(Chlorophyll A)",
                          "Log(Chlorophyll A + 1)", 
                          "Log(Chlorophyll A + 0.5)", 
                          "Log(Chlorophyll A + 0.25)"),
                units = c('mg/l', 
                          'mg/l',
                          'mg/l',
                          'mg/l',
                          'mg/l'))

nested_data <- the_data %>%
  mutate(year_f = factor(year)) %>%
  
  pivot_longer(c(chl:log25_chl), names_to = 'parameter', 
               values_to = 'value') %>%
  filter(! is.na(value)) %>%
  
  # This allows us to ensure the order of the rows in the nested tibble
  mutate(parameter = factor(parameter,
                            levels = c('chl', 'log_chl', 'log1_chl', 
                                       'log5_chl', 'log25_chl'))) %>%

  group_by(parameter) %>%
  nest() %>%
  arrange(parameter) %>%
  left_join(units, by = 'parameter')
```

## Sanity Checks

``` r
min(nested_data$data[[1]]$value)
#> [1] -0.02
```

``` r
map(nested_data$data, function(df) length(df$station))
#> [[1]]
#> [1] 424
#> 
#> [[2]]
#> [1] 409
#> 
#> [[3]]
#> [1] 424
#> 
#> [[4]]
#> [1] 424
#> 
#> [[5]]
#> [1] 424
```

# Hierarchical models

We treat stations as random exemplars of possible stations, and thus
rely on hierarchical models. We could run simple regressions based on
summary statistics of the trend data, but a nested model will better
address station by station uncertainty.

We use a GAM model with a random factor smoothing term. We could just as
well use `lmer()` or `lme()`. The GAM framework makes it easier to
evaluate smoothers for the year to year variation. We restrict ourselves
to linear trends by year, but explore several ways of modeling
seasonality, including a polynomial model by day of the year, a simple
model my month, and an interaction model by month.

The primary purpose of modeling seasonality here is to remove data
variability, but it introduces complexity because the long-term trends
are expected to vary by season.

``` r
nested_data <- nested_data %>%
  mutate(lmers = map(data, function(df) gam(value ~ year + 
                                              month + 
                                              s(station, bs = 're'), 
                                            data = df))) %>%
  mutate(lmers_2 = map(data, function(df) gam(value ~ year + 
                                              month + year:month +
                                              s(station, bs = 're'), 
                                            data = df)))  %>%
  mutate(polys = map(data, function(df) gam(value ~ year + 
                                            poly(doy,3) +
                                            s(station, bs = 're'), 
                                          data = df)))
```

## Diagnostic Plots

We focus on the simpler model. Others should be similar or slightly
better.

``` r
for (p in nested_data$parameter) {
  cat('\n')
  cat(p)
  cat('\n')
  gam.check(nested_data$lmers[nested_data$parameter == p][[1]],
       sub = p)
}
#> 
#> chl
```

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/diagnostics-1.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 10 iterations.
    #> The RMS GCV score gradient at convergence was 2.815052e-05 .
    #> The Hessian was positive definite.
    #> Model rank =  11 / 11 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>                  k'      edf k-index p-value
    #> s(station) 3.00e+00 9.21e-09      NA      NA
    #> 
    #> log_chl

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/diagnostics-2.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 9 iterations.
    #> The RMS GCV score gradient at convergence was 2.976352e-07 .
    #> The Hessian was positive definite.
    #> Model rank =  11 / 11 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>                  k'      edf k-index p-value
    #> s(station) 3.00e+00 4.48e-09      NA      NA
    #> 
    #> log1_chl

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/diagnostics-3.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 9 iterations.
    #> The RMS GCV score gradient at convergence was 2.263978e-07 .
    #> The Hessian was positive definite.
    #> Model rank =  11 / 11 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>                  k'      edf k-index p-value
    #> s(station) 3.00e+00 1.61e-08      NA      NA
    #> 
    #> log5_chl

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/diagnostics-4.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 1.813705e-06 .
    #> The Hessian was positive definite.
    #> Model rank =  11 / 11 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>               k'   edf k-index p-value
    #> s(station) 3.000 0.532      NA      NA
    #> 
    #> log25_chl

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/diagnostics-5.png" style="display: block; margin: auto;" />

    #> 
    #> Method: GCV   Optimizer: magic
    #> Smoothing parameter selection converged after 4 iterations.
    #> The RMS GCV score gradient at convergence was 8.279634e-10 .
    #> The Hessian was positive definite.
    #> Model rank =  11 / 11 
    #> 
    #> Basis dimension (k) checking results. Low p-value (k-index<1) may
    #> indicate that k is too low, especially if edf is close to k'.
    #> 
    #>               k'   edf k-index p-value
    #> s(station) 3.000 0.863      NA      NA

Log of chlorophyll A , not log of chlorophyll A plus one provides a
slightly better distribution of residuals. The other two transforms do
even better.  
All four transforms should provide fairly similar results.

However, some chlorophyll observations have a nominal value of zero, and
one has a (very slightly) negative value. Those samples get dropped from
the log-transformed data. Since those observations are the lowest
observations, dropping them biases results. But because they are
nominally zero, they strongly influence all model fits.

## Compare Hierarchical Models

``` r
nested_data <- nested_data %>%
  mutate(compare = list(anova(polys[[1]], 
                              lmers[[1]], lmers_2[[1]], test = 'LRT')))
names(nested_data$compare) <- nested_data$parameter
nested_data$compare
#> $chl
#> Analysis of Deviance Table
#> 
#> Model 1: value ~ year + poly(doy, 3) + s(station, bs = "re")
#> Model 2: value ~ year + month + s(station, bs = "re")
#> Model 3: value ~ year + month + year:month + s(station, bs = "re")
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1       419      92592                          
#> 2       416      91203  3   1388.7 0.0837378 .  
#> 3       410      85552  6   5651.9 0.0001395 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> $log_chl
#> Analysis of Deviance Table
#> 
#> Model 1: value ~ year + poly(doy, 3) + s(station, bs = "re")
#> Model 2: value ~ year + month + s(station, bs = "re")
#> Model 3: value ~ year + month + year:month + s(station, bs = "re")
#>   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#> 1       404     411.38                          
#> 2       401     405.90  3   5.4818 0.1293885    
#> 3       395     382.58  6  23.3234 0.0005047 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> $log1_chl
#> Analysis of Deviance Table
#> 
#> Model 1: value ~ year + poly(doy, 3) + s(station, bs = "re")
#> Model 2: value ~ year + month + s(station, bs = "re")
#> Model 3: value ~ year + month + year:month + s(station, bs = "re")
#>   Resid. Df Resid. Dev     Df Deviance  Pr(>Chi)    
#> 1    419.00     262.94                              
#> 2    416.00     257.17 3.0000   5.7764   0.01977 *  
#> 3    409.77     240.05 6.2339  17.1179 6.804e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> $log5_chl
#> Analysis of Deviance Table
#> 
#> Model 1: value ~ year + poly(doy, 3) + s(station, bs = "re")
#> Model 2: value ~ year + month + s(station, bs = "re")
#> Model 3: value ~ year + month + year:month + s(station, bs = "re")
#>   Resid. Df Resid. Dev     Df Deviance  Pr(>Chi)    
#> 1    418.00     347.51                              
#> 2    415.08     339.67 2.9204   7.8418 0.0165040 *  
#> 3    408.83     317.71 6.2441  21.9553 0.0001032 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> $log25_chl
#> Analysis of Deviance Table
#> 
#> Model 1: value ~ year + poly(doy, 3) + s(station, bs = "re")
#> Model 2: value ~ year + month + s(station, bs = "re")
#> Model 3: value ~ year + month + year:month + s(station, bs = "re")
#>   Resid. Df Resid. Dev     Df Deviance  Pr(>Chi)    
#> 1    417.61     435.96                              
#> 2    414.65     425.83 2.9639   10.138 0.0151239 *  
#> 3    408.51     399.59 6.1350   26.232 0.0001737 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The month by month model is often better than the polynomial model, and
the interaction model is always a better still.

That poses significant challenges for presentation in State of the Bay.
Given the high level of variability in the data, we can’t present
multiple trend lines in a single plot. So the question is, how do we
present the complexity of seasonal changes without overwhelming our
readers?

### Interaction Plots

We need to look dig into these patterns with interaction plots and
decide how to simplify our findings for State of Casco Bay.

``` r
nested_data <- nested_data %>%
  mutate(emmi = map(lmers_2, function(mod) emmip(mod, month ~ year, 
                                                  at = list(year = 1993:2020)))) %>%
  mutate(emmi = list(emmi[[1]] + ggtitle(parameter)))

nested_data$emmi
#> [[1]]
```

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/interaction_plots-1.png" style="display: block; margin: auto;" />

    #> 
    #> [[2]]

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/interaction_plots-2.png" style="display: block; margin: auto;" />

    #> 
    #> [[3]]

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/interaction_plots-3.png" style="display: block; margin: auto;" />

    #> 
    #> [[4]]

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/interaction_plots-4.png" style="display: block; margin: auto;" />

    #> 
    #> [[5]]

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/interaction_plots-5.png" style="display: block; margin: auto;" />
Not surprisingly, the pattern is the same regardless of model
selected.  
Basically, chlorophyll levels have declined in spring, held more or less
steady in mid summer, and increased in fall.

# GAM models

We may can also examine seasonal patterns with an two dimensional GAM
models using a tensor smooth by day of year and year.

Using interaction tensor terms showed that the interaction term was
significant in all cases. But the results are hard to interpret. Here we
show only a fairly low dimension 2-D GAM fit, to figure out what is
going on seasonally.

``` r
nested_data <- nested_data %>%
  mutate(gams = map(data, function(df) gam(value ~ te(year, doy, k = 4) +
                                              s(station, bs = 're'), 
                                            data = df)))
```

``` r
for (p in nested_data$parameter) {
  plot(nested_data$gams[nested_data$parameter == p][[1]])
  title(sub = p)
}
```

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-1.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-2.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-3.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-4.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-5.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-6.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-7.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-8.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-9.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_gam_smoothers-10.png" style="display: block; margin: auto;" />
Chlorophyll shows a fairly steady decline in spring, and on increase in
fall, with little change in summer. That general result is robust to the
transformation used, but the untransformed fit shows the effect of a
handful of both high and low values in the earlier record.

# Clean Up `nested_data`

``` r
nested_data <- nested_data %>%
  select(-lmers_2, -polys, -gams, -compare, -emmi)
```

# Predictions with `emmeans()`

## Handling Transformations

Many of the problems we had presenting model results of the chlorophyll
analysis stemmed from the need for special handling on the transforms we
used. Challenges arise both with extracting model predictions and with
plotting data on transformed axes.

We want to use the tools in `emmeans` to extract marginal means. The
challenge here is that constructing the reference grid should occur in
the space where the response is linear.

`emmeans` recognizes some standard transformations automatically. It
also recognizes many more transformations by name, with possible
parameters. Or you can construct a transformation manually, basically by
passing a function and its inverse to a constructor function.

The process is slightly different, depending on whether the transform
was included as part of the model or the data was transformed before the
model was run. Since we have transformed the data first, we need to use
the second method here.

`emmeans` has no way of knowing that the response variables in our
chlorophyll models have already been transformed, so we need to tell it
so.

We would like to be able to pass the transform object to each
`emmeans()` call.  
This means we probably can’t run the analysis with a call to `map()`,
but will need to assemble results in a list via a `for` loop.

First, what transforms do we need? Lets put them in a named list.

``` r
trans_list <- list(chl = "identity",
     log_chl = "log",
     log1_chl = make.tran("genlog", 1),
     log5_chl = make.tran("genlog", 0.5),
     log25_chl = make.tran("genlog", 0.25)
)
```

## Lets see if this works…

The only trick involved is explicitly embedding the object returned from
`emmeans()` (which is an S4 object) in a list before adding it to the
larger list.

``` r
preds <- list()
for (p in nested_data$parameter) {
  row = nested_data[nested_data$parameter == p ,]
  mod <- row$lmers[[1]]
  preds[p] <- list(emmeans(mod, ~year, at = list(year = 2001:2020), 
                          tran = trans_list[[p]], type = 'response'))
}

nested_data$preds <- preds
rm(preds)
```

``` r
for (p in nested_data$parameter) {
  preds <- nested_data$preds[nested_data$parameter == p][[1]]
  print(plot(preds) + 
            xlab(p) +
            theme(axis.text.x = element_text(angle = 90, size = 9,
                                             vjust = 0.25,
                                             hjust = 1)) +
            coord_flip())
}
```

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_emmeans-1.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_emmeans-2.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_emmeans-3.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_emmeans-4.png" style="display: block; margin: auto;" /><img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_emmeans-5.png" style="display: block; margin: auto;" />

# Final Model Review

## ANOVAs

``` r
for (p in nested_data$parameter) {
  cat(p)
  print(anova(nested_data$lmers[nested_data$parameter == p][[1]]))
    cat('\n\n')
}
#> chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ year + month + s(station, bs = "re")
#> 
#> Parametric Terms:
#>       df      F  p-value
#> year   1 12.494 0.000454
#> month  6  1.988 0.066125
#> 
#> Approximate significance of smooth terms:
#>                  edf    Ref.df F p-value
#> s(station) 9.213e-09 3.000e+00 0   0.386
#> 
#> 
#> log_chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ year + month + s(station, bs = "re")
#> 
#> Parametric Terms:
#>       df      F  p-value
#> year   1 12.102 0.000559
#> month  6  2.196 0.042558
#> 
#> Approximate significance of smooth terms:
#>                  edf    Ref.df F p-value
#> s(station) 4.477e-09 2.000e+00 0   0.457
#> 
#> 
#> log1_chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ year + month + s(station, bs = "re")
#> 
#> Parametric Terms:
#>       df     F p-value
#> year   1 4.990  0.0260
#> month  6 2.535  0.0202
#> 
#> Approximate significance of smooth terms:
#>                 edf   Ref.df F p-value
#> s(station) 1.61e-08 3.00e+00 0   0.398
#> 
#> 
#> log5_chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ year + month + s(station, bs = "re")
#> 
#> Parametric Terms:
#>       df     F p-value
#> year   1 3.161  0.0761
#> month  6 2.598  0.0175
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F p-value
#> s(station) 0.5319 2.0000 0.362   0.257
#> 
#> 
#> log25_chl
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> value ~ year + month + s(station, bs = "re")
#> 
#> Parametric Terms:
#>       df     F p-value
#> year   1 1.789  0.1817
#> month  6 2.592  0.0178
#> 
#> Approximate significance of smooth terms:
#>               edf Ref.df     F p-value
#> s(station) 0.8627 2.0000 0.761   0.173
```

Some models show significant long-term trends, others don’t. That does
not offer great confidence in model results, but we know these models
overlook strong seasonal interactions, so perhaps these are not
appropriate models.

## Slopes

``` r
nested_data <- nested_data %>%
 mutate(slopes = map(lmers, function(mod) coef(mod)[[2]]))
cbind(nested_data$parameter, nested_data$slopes)
#>      [,1]        [,2]       
#> [1,] "chl"       -0.4447178 
#> [2,] "log_chl"   -0.03055734
#> [3,] "log1_chl"  -0.01492378
#> [4,] "log5_chl"  -0.01366211
#> [5,] "log25_chl" -0.01151513
```

At least all slopes are negative….

# Build Graphics

## Handling Transformations

We will need to specify the transformations applied to the Y axis to
plot these models correctly. We use a similar strategy as we used for
specifying transformations for ggplot.

We can make our own transform objects as follows, using the
`trans_new()` function from the `scales` package.

``` r
forward5  <- function(x) log(x + 0.5)
backward5 <- function(z) exp(z) - 0.5
mytran5   <- scales::trans_new('log0.5p', forward5, backward5,
                            format = scales::label_comma(),
                            domain = c(-0.4999999, Inf))
```

``` r
forward25  <- function(x) log(x + 0.25)
backward25 <- function(z) exp(z) - 0.25
mytran25 <- scales::trans_new('log0.25p', forward25, backward25,
                            format = scales::label_comma(),
                            domain = c(-0.24999999, Inf))
```

``` r
trans_list <- list(chl = "identity",
     log_chl = "log",
     log1_chl = "log1p",
     log5_chl = mytran5,
     log25_chl = mytran25
)
nested_data$trans <- trans_list
```

## Create Plotting Function

Note the mismatch here – I have TRANSFORMED

``` r
my_plot_fxn <- function(dat, preds, label = '', units = '', transf = 'identity') {
  preds <- summary(preds)
  
  p <- ggplot(dat, aes(x = year)) +
    geom_jitter(aes(y = value), 
                width = 0.25, height = 0,
                color = cbep_colors()[1], alpha = 0.2) +
    xlim(1993,2020) +
    ylab(paste0(label,
                if_else(nchar(units)> 0, ' (', ' '),
                units, 
                if_else(nchar(units)> 0,')', ''))) +
    xlab('') +
    
   geom_ribbon(data = preds, mapping = aes(x = year, 
                                        ymin = lower.CL,
                                        ymax = upper.CL),
               fill = 'blue',
               alpha = 0.1) +
   geom_line(data = preds, mapping = aes(x = year, y = response),
             color = cbep_colors()[2], size  = 1) +
    scale_y_continuous(trans = transf, 
                       breaks = c(1,20,50, 100, 200),
                       labels =  scales::label_number(accuracy = 1))
  return(p)
}
```

``` r
for (p in nested_data$parameter) {
  row <- nested_data[nested_data$parameter == p,] 
  d <- nested_data$data[[1]]   # we always plot the "raw" data because 
  p <- row$preds[[1]]          # predictions are all on the "response" scale
  l <- row$label
  u <- row$units
  t <- row$trans[[1]]
  
  print(my_plot_fxn(d,p,l,u, t))
}
#> Warning: Removed 14 rows containing missing values (geom_point).
```

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/make_plots-1.png" style="display: block; margin: auto;" />

    #> Warning in self$trans$transform(x): NaNs produced
    #> Warning: Transformation introduced infinite values in continuous y-axis
    #> Warning: Removed 14 rows containing missing values (geom_point).

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/make_plots-2.png" style="display: block; margin: auto;" />

    #> Warning: Removed 13 rows containing missing values (geom_point).

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/make_plots-3.png" style="display: block; margin: auto;" />

    #> Warning: Removed 9 rows containing missing values (geom_point).

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/make_plots-4.png" style="display: block; margin: auto;" />

    #> Warning: Removed 20 rows containing missing values (geom_point).

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/make_plots-5.png" style="display: block; margin: auto;" />

# Alternative Approaches to Addressing “Zero” Values

We refit the chlorophyll log transform models somewhat more carefully,
specifically regarding how we handle observations of “zero” (0.00)
chlorophyll. In the prior modeling of log-transformed data, we
implicitly dropped all observations with reported zero chlorophyll (they
were dropped as `NA` after the log transform).

## Rebuild Data Frame

Here, we pull the raw chlorophyll data from the nested tibble, and
create working data sets to explore.

``` r
df_three <- nested_data %>%
  filter(parameter == 'chl') %>%
  pull(data)           # Returns a list
df_three <- df_three[[1]]  # Extract the first item....  df is now a dataframe
df_three <- df_three %>%
  filter(! is.na(value))
```

We replace fourteen zero chlorophyll observations (and one negative one)
with either a non-zero replacement value (starting at half of the lowest
positive reported value), or with NA.

``` r
df_three2 <- df_three %>% mutate(value = if_else(value <= 0, 0.05, value))
df_three3 <- df_three %>% mutate(value = if_else(value <= 0, 0.01, value))
df_three4 <- df_three %>% mutate(value = if_else(value <= 0, 0.00001, value))
df_three5 <- df_three %>% mutate(value = if_else(value <= 0, NA_real_, value))
```

``` r
ggplot(df_three2, aes(year, value)) + 
  geom_point(aes(color = value < 0.1), alpha = 0.25) +
  scale_y_log10()
```

<img src="Surface_Analysis_Chlorophyll_Trends_files/figure-gfm/plot_adjusted_data-1.png" style="display: block; margin: auto;" />
Since most zero values are older, the nominal zero values have a large
influence on estimated slopes. The lower we set the replacement value we
assign to include these observations in the regression, the more
influence they will have, eventually making the slope not significant.

So, if we fit multiple models replacing those points with different
replacement values, we can “turn on” and “turn off” statistical
significance of the trend as well as influence other model terms.

Here we pull information for the long-term trend from the parameter
table (ANOVAs and other details not shown).

``` r
cat('Zero -> 0.05\n')
#> Zero -> 0.05
mod2 <- gam(log(value) ~ year + month + s(station, bs = 're'),  data = df_three2)
summary(mod2)$p.table[2,]
#>     Estimate   Std. Error      t value     Pr(>|t|) 
#> -0.004902278  0.010718346 -0.457372626  0.647642682
cat('\nZero -> 0.01\n')
#> 
#> Zero -> 0.01
mod3 <- gam(log(value) ~ year + month + s(station, bs = 're'),  data = df_three3)
summary(mod3)$p.table[2,]
#>    Estimate  Std. Error     t value    Pr(>|t|) 
#> 0.004409346 0.012415185 0.355157489 0.722652158
cat('\nZero -> 0.00001\n')
#> 
#> Zero -> 0.00001
mod4 <- gam(log(value) ~ year + month + s(station, bs = 're'),  data = df_three4)
summary(mod4)$p.table[2,]
#>   Estimate Std. Error    t value   Pr(>|t|) 
#> 0.04443280 0.02152187 2.06454203 0.03958765
cat('\nZero -> NA\n')
#> 
#> Zero -> NA
mod5 <- gam(log(value) ~ year + month + s(station, bs = 're'),  data = df_three5)
summary(mod5)$p.table[2,]
#>      Estimate    Std. Error       t value      Pr(>|t|) 
#> -0.0305573396  0.0087839953 -3.4787518130  0.0005589701
```

So, if we replace the zero values with `NA`, we see a significantly
significant DECREASE in log(chlorophyll) over time. If we replace them
with a a sufficiently small value (here 0.0001), the model suggests a
statistically significant INCREASE in log(chlorophyll).

# Discussion

None of that engenders much confidence in the models. We saw earlier
that our selection of the transform determines whether we see a
nominally “significant” trend. Here we see that the same is true if we
use a different approach to handling those nominal “zero” values. Not
only does our selection of how to deal with zero observations determine
whether the regression is statistically significant, it controls the
apparent sign of the relationship.
