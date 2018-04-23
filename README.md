
tidyfec
-------

The tidyfec package aims to make campaign finance data more accessible in R and friendly with tidy workflows. It works by accessing data through the [OpenFEC API](https://api.open.fec.gov/developers/), which requires an [API key](https://api.data.gov/signup/) to run.

Installation
------------

Though I'm developing out in the open, this is very much pre-alpha release status. Installation is through the devtools package

``` r
install.packages("devtools")
devtools::install_github("stephenholzman/tidyfec")
```

Examples
--------

``` r
library(tidyfec)
library(tidyverse)
#> ── Attaching packages ───────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
#> ✔ tibble  1.4.2     ✔ dplyr   0.7.4
#> ✔ tidyr   0.8.0     ✔ stringr 1.3.0
#> ✔ readr   1.1.1     ✔ forcats 0.3.0
#> Warning: package 'tibble' was built under R version 3.4.3
#> Warning: package 'tidyr' was built under R version 3.4.3
#> Warning: package 'stringr' was built under R version 3.4.3
#> Warning: package 'forcats' was built under R version 3.4.3
#> ── Conflicts ──────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

#data_gov_api_key("api-key-string")
#install api key

df <- search_candidates(state = "VA", district = "10", office = "H", election_year = "2018") %>%
  get_candidate_totals() %>%
  filter(type_of_funds %in% tidyfec_filters$top_level, cycle == "2018") %>%
  separate(name, c('last_name', 'first_name'), sep = ', ', extra = "drop", fill = "right")

df  %>%
  ggplot() +
  geom_bar(aes(x = last_name, y = amount), stat = "identity") +
  facet_wrap(~type_of_funds) +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip() +
  theme_bw()
```

![](README-unnamed-chunk-3-1.png)
