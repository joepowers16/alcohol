Alcohol consumption as function of latitude and wealth
================
JP
July 7, 2017

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(readr)
library(stringr)
library(forcats)
```

``` r
# https://github.com/fivethirtyeight/data/tree/master/alcohol-consumption
data_drinks <- "../data/drinks.csv"

# "https://developers.google.com/public-data/docs/canonical/countries_csv"
data_longlat <- "../data/long_lat.csv"

# http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
data_ppp <- "../data/ppp/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2.csv"
```

Poking around:

``` r
d %>% arrange(desc(total))
```

    ## # A tibble: 193 x 10
    ##               country beer_servings spirit_servings wine_servings total
    ##                 <chr>         <int>           <int>         <int> <dbl>
    ##  1            Belarus           142             373            42  14.4
    ##  2          Lithuania           343             244            56  12.9
    ##  3            Andorra           245             138           312  12.4
    ##  4            Grenada           199             438            28  11.9
    ##  5     Czech Republic           361             170           134  11.8
    ##  6             France           127             151           370  11.8
    ##  7 Russian Federation           247             326            73  11.5
    ##  8            Ireland           313             118           165  11.4
    ##  9         Luxembourg           236             133           271  11.4
    ## 10           Slovakia           196             293           116  11.4
    ## # ... with 183 more rows, and 5 more variables: abbr.x <chr>,
    ## #   latitude <dbl>, longitude <dbl>, abbr.y <chr>, ppp <dbl>

``` r
d %>% filter(str_detect(country, "R")) %>% arrange(desc(country))
```

    ## # A tibble: 8 x 10
    ##                    country beer_servings spirit_servings wine_servings
    ##                      <chr>         <int>           <int>         <int>
    ## 1                   Rwanda            43               2             0
    ## 2       Russian Federation           247             326            73
    ## 3                  Romania           297             122           167
    ## 4                 DR Congo            32               3             1
    ## 5       Dominican Republic           193             147             9
    ## 6           Czech Republic           361             170           134
    ## 7               Costa Rica           149              87            11
    ## 8 Central African Republic            17               2             1
    ## # ... with 6 more variables: total <dbl>, abbr.x <chr>, latitude <dbl>,
    ## #   longitude <dbl>, abbr.y <chr>, ppp <dbl>

``` r
d %>% filter(country == "Russian Federation")
```

    ## # A tibble: 1 x 10
    ##              country beer_servings spirit_servings wine_servings total
    ##                <chr>         <int>           <int>         <int> <dbl>
    ## 1 Russian Federation           247             326            73  11.5
    ## # ... with 5 more variables: abbr.x <chr>, latitude <dbl>,
    ## #   longitude <dbl>, abbr.y <chr>, ppp <dbl>

``` r
d %>% 
  mutate(
    lat_abs = abs(latitude),
    ns = ifelse(latitude > 0, "North",
      ifelse(latitude <= 0, "South", NA))
  ) %>% 
  filter(complete.cases(ns)) %>% 
  ggplot(aes(x = lat_abs, y = total, color = ns)) + 
  geom_point() + 
  geom_smooth() + 
  scale_color_discrete("Hemisphere") + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) + 
  labs(
    title = "Drinking spikes when you are farther than 30-degrees north from the equator,\nespecially so in the northern hemisphere.",
    x = "Absolute Value for Latitude",
    y = "Liters of alcohol consumed per person per year"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](drinks_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

``` r
d %>% 
  ggplot(aes(x = latitude, y = total)) + 
  geom_point(alpha = .75) + 
  geom_smooth() + 
  geom_vline(xintercept = 0, color = "green") + 
  scale_color_discrete("Hemisphere") + 
  scale_x_continuous(breaks = seq(-45, 65, by = 5)) + 
  labs(
    title = "Drinking spikes when you are farther than 30-degrees north from the equator,\nespecially so in the northern hemisphere.",
    x = "Latitude",
    y = "Liters of alcohol consumed per person per year"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](drinks_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
d %>% 
  ggplot(aes(x = latitude, y = total)) + 
  geom_boxplot(aes(group = cut_width(latitude, 5))) + 
  scale_x_continuous(breaks = seq(-45, 65, by = 5)) + 
  geom_vline(xintercept = 0, color = "blue") + 
  labs(
    title = "Drinking spikes when you are farther than 30-degrees north from the equator,\nespecially so in the northern hemisphere.",
    x = "Latitude",
    y = "Liters of alcohol consumed per person per year"
  )
```

    ## Warning: Removed 15 rows containing non-finite values (stat_boxplot).

![](drinks_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
d %>% 
  ggplot(aes(x = ppp, y = total)) + 
  geom_point(alpha = .75) + 
  geom_smooth() 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 39 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 39 rows containing missing values (geom_point).

![](drinks_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

``` r
d %>% 
  filter(ppp < 5e4) %>% 
  ggplot(aes(x = ppp, y = total)) + 
  geom_point(alpha = .75) + 
  geom_smooth() + 
  labs(
    title = "The poor drink less.",
    x = "GDP per Person (PPP) in USD",
    y = "Liters of alcohol consumed per person per year"
  ) 
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](drinks_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-2.png)

``` r
d %>% 
  mutate(
    ppp_f = ifelse(ppp <= 15000, "low",
      ifelse(ppp > 15000 & ppp < 30000, "medium",
      ifelse(ppp >= 30000, "high", NA)
    )),
    ppp_f = factor(ppp_f, levels = c("low", "medium", "high"))
  ) %>% 
  filter(complete.cases(ppp_f)) %>% 
  ggplot(aes(x = latitude, y = total, color = fct_rev(ppp_f))) + 
  geom_point(alpha = .75) + 
  geom_smooth() + 
  geom_vline(xintercept = 0, color = "black") + 
  scale_color_discrete("PPP") + 
  scale_x_continuous(breaks = seq(-45, 65, by = 5)) + 
  labs(
    title = "Wealthy countries buck the trend of higher drinking in the far north.",
    x = "Latitude",
    y = "Liters of alcohol consumed per person per year"
  )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 5 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 5 rows containing missing values (geom_point).

![](drinks_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

TBD: Add in religion as a variable.
