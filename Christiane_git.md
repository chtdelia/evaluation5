Evaluation\_5
================
Christiane
2019-04-04

1.  Créer une série temporelle du CO2 à partir des données de hawai.csv.
2.  Séparer la série en parties d'entraînement (environ 70% des données) et en partie test.
3.  Créer un modèle ETS sur les données d'entraînement, puis projeter la prévision de CO2 atmosphérique pour comparer aux données test.
4.  Effectuer une analyse des résidus. Commenter: le modèle est-il fiable? Comment pourrait-il être amélioré? Selon le Test Ljung\_box la série temporelle n'a pas été générée à partir d'un bruit blanc (valeur de la p-value faible) et les résidus ne sont pas correlés donc pas dérivés d'un bruit blanc et donc il faudrait tester un autre modèle.

Vous devez me remettre un lien vers un répertoire git de votre choix (GitHub, GitLab, etc.) comprenant un code reproductible de votre démarche en format R-markdown.

[Evaluation5\_Christiane](%22https://github.com/chtdelia/evaluation5/blob/master/Christiane_git.Rmd%22)

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages -------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0       v purrr   0.3.2  
    ## v tibble  2.1.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.3       v stringr 1.3.1  
    ## v readr   1.3.1       v forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## -- Conflicts ----------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.5.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(forecast)
```

    ## Warning: package 'forecast' was built under R version 3.5.3

``` r
library(fpp2)
```

    ## Warning: package 'fpp2' was built under R version 3.5.3

    ## Loading required package: fma

    ## Warning: package 'fma' was built under R version 3.5.3

    ## Loading required package: expsmooth

    ## Warning: package 'expsmooth' was built under R version 3.5.3

``` r
library(agridat)
```

    ## Warning: package 'agridat' was built under R version 3.5.3

``` r
library(expsmooth)


setwd("C:/Users/Utilisateur/Dropbox/Analyse") 
hawai <- readr::read_csv("CodeR/_Evaluation5/hawai.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   time = col_double(),
    ##   CO2 = col_double()
    ## )

``` r
ggplot(data = hawai, mapping = aes(x = time, y = CO2)) +
  geom_line() 
```

![](Christiane_git_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
hawai_ts <- ts(hawai %>% select(-time),
           start = c(hawai$time[3] %>% time(), 1),
           frequency = 12)

ggplot(data = hawai, mapping = aes(x = time, y = CO2)) +
  geom_line(colour ="red", lwd=0.7) + 
  scale_x_continuous(breaks=c(1958:2001)) +
  theme(axis.text=element_text(colour="black", angle=45, hjust=1.1))
```

![](Christiane_git_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
hawai_ts_train <- window(hawai_ts, start = 1, end = 30)
hawai_ts_test <- window(hawai_ts, start = 31)

hm_naive <- snaive(hawai_ts_train, h = 44)
autoplot(hm_naive) +
  forecast::autolayer(fitted(hm_naive)) +
  forecast::autolayer(hawai_ts_test, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Time", y = "CO2")
```

    ## Warning: Removed 12 rows containing missing values (geom_path).

![](Christiane_git_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
hawai_model <- ets(hawai_ts_train) 
hawai_model
```

    ## ETS(M,Ad,M) 
    ## 
    ## Call:
    ##  ets(y = hawai_ts_train) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.5702 
    ##     beta  = 0.0301 
    ##     gamma = 1e-04 
    ##     phi   = 0.9797 
    ## 
    ##   Initial states:
    ##     l = 315.1769 
    ##     b = -0.0234 
    ##     s = 1.0017 0.9998 0.9972 0.9939 0.9907 0.9913
    ##            0.9964 1.0024 1.0069 1.0086 1.0071 1.004
    ## 
    ##   sigma:  0.001
    ## 
    ##      AIC     AICc      BIC 
    ## 1314.099 1316.171 1383.490

``` r
autoplot(hawai_model)
```

![](Christiane_git_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
hawai_ets <- hawai_ts_train %>% ets()
hawai_fc <- hawai_ets %>% forecast()
hawai_fc %>% autoplot()
```

![](Christiane_git_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
checkresiduals(hawai_ets) 
```

![](Christiane_git_files/figure-markdown_github/unnamed-chunk-1-6.png)

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ETS(M,Ad,M)
    ## Q* = 51.07, df = 7, p-value = 8.899e-09
    ## 
    ## Model df: 17.   Total lags used: 24
