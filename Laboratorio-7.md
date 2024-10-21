Laboratorio 7
================
Andrea Soto
2024-10-20

``` r
library(readr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ purrr     1.0.2
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(stringr)
library(tidytext)
library(lubridate)
library(stopwords)
library(wordcloud)
```

    ## Cargando paquete requerido: RColorBrewer

``` r
library(highcharter)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo 
    ## Highcharts (www.highcharts.com) is a Highsoft software product which is
    ## not free for commercial and Governmental use

``` r
df <- read_csv("Health_and_Personal_Care.csv")
```

    ## Rows: 494121 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): title, text, product_id, parent_id, user_id
    ## dbl (2): rating, timestamp
    ## lgl (1): verified_purchase
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df %>% head()
```

    ## # A tibble: 6 × 8
    ##   rating title    text  product_id parent_id user_id timestamp verified_purchase
    ##    <dbl> <chr>    <chr> <chr>      <chr>     <chr>       <dbl> <lgl>            
    ## 1      4 12 mg i… "Thi… B07TDSJZMR B07TDSJZ… AFKZEN…   1.58e12 TRUE             
    ## 2      5 Save th… "Lov… B08637FWWF B08637FW… AEVWAM…   1.60e12 TRUE             
    ## 3      5 Fantast… "I h… B07KJVGNN5 B07KJVGN… AHSPLD…   1.56e12 TRUE             
    ## 4      4 It hold… "It'… B007HY7GC2 B092RP73… AEZGPL…   1.66e12 TRUE             
    ## 5      1 Not for… "Did… B08KYJLF5T B08KYJLF… AEQAYV…   1.64e12 TRUE             
    ## 6      5 Every h… "I h… B09GBMG83Z B09GBMG8… AFSKPY…   1.65e12 FALSE

``` r
meta <- read_csv("Health_and_Personal_Care_metadata.csv")
```

    ## Rows: 60293 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): main_category, title, store, details, parent_id
    ## dbl (3): average_rating, rating_number, price
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
meta %>% head()
```

    ## # A tibble: 6 × 8
    ##   main_category title average_rating rating_number price store details parent_id
    ##   <chr>         <chr>          <dbl>         <dbl> <dbl> <chr> <chr>   <chr>    
    ## 1 Health & Per… Sili…            3.9             7  NA   Rzoe… 15 x 3… B07V346G…
    ## 2 Health & Per… iPho…            3.8             2  NA   ZHXIN ZHXIN,… B075W927…
    ## 3 Health & Per… Zig …            3.9             7  NA   <NA>  4.1 x … B01FB26V…
    ## 4 Health & Per… Stin…            4.1             6  21.4 Stin… Sting-… B01IAI29…
    ## 5 Health & Per… Heat…            3.3             8  NA   BiBO… 6.1 x … B08CMN38…
    ## 6 Health & Per… Ball…            4.6            19  NA   Tikt… Bachel… B07YJ5JB…

``` r
#Health_and_Personal_Care
#Health_and_Personal_Care_metadata
```

``` r
#Ejercicio 1 
palabras <- c("love", "recommend", "enjoy")
reviews <- df%>%
    filter(str_detect(text,paste(palabras, collapse = "|")))
  
meta_reviews <- reviews %>%
  left_join(meta, by = "parent_id")

productos <- reviews %>%
  summarise(conteo_productos = n_distinct(product_id))
productos
```

    ## # A tibble: 1 × 1
    ##   conteo_productos
    ##              <int>
    ## 1            23180

``` r
#Ejercicio 2 
top_5_tiendas <- meta_reviews %>%
  group_by(store) %>%
  summarise(productos = n_distinct(product_id)) %>%
  arrange(desc(productos)) %>%
  slice_head(n = 5)

top_5_tiendas
```

    ## # A tibble: 5 × 2
    ##   store       productos
    ##   <chr>           <int>
    ## 1 <NA>              769
    ## 2 HAARBB            173
    ## 3 Eyekepper         135
    ## 4 US Organic         78
    ## 5 Andaz Press        76

``` r
#Ejercicio 3 

stop_vec <- c(stopwords(language = "en"), stopwords(language = "es"))

vec_words <- str_split(df$text[1:50], boundary("word")) %>% unlist() #

no_stopwords <- vec_words %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_vec)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords$value, no_stopwords$freq, max.words = 100, random.order = FALSE)
```

![](Laboratorio-7_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#Ejercicio 4 
top_5_tiendas <- meta_reviews %>%
  group_by(store) %>%
  summarise(productos = n_distinct(product_id)) %>%
  arrange(desc(productos)) %>%
  slice_head(n = 5)

reviews_5_tiendas <- meta_reviews %>%
  filter(store %in% top_5_tiendas$store)

vec_words <- str_split(reviews_5_tiendas$text[1:50], boundary("word")) %>% unlist() 

no_stopwords <- vec_words %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_vec)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords$value, no_stopwords$freq, max.words = 100, random.order = FALSE)
```

![](Laboratorio-7_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#EJERCICIO 5 
vec_words <- str_split(df$text, boundary("word")) %>% unlist() 

stop_vec <- c(stopwords(language = "en"), stopwords(language = "es")) 
word_frequencies <- vec_words %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_vec)) %>%  
  group_by(value) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) 

top_25_words <- word_frequencies %>% slice_head(n = 25) 
top_25_words
```

    ## # A tibble: 25 × 2
    ##    value     freq
    ##    <chr>    <int>
    ##  1 I       590693
    ##  2 br      136733
    ##  3 product 100611
    ##  4 The      94876
    ##  5 It       81424
    ##  6 use      80636
    ##  7 like     76352
    ##  8 great    71485
    ##  9 This     69662
    ## 10 one      64936
    ## # ℹ 15 more rows
