Laboratorio 3
================
Andrea Soto
2024-09-08

``` r
library(dplyr)
```

    ## 
    ## Adjuntando el paquete: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
actors <- read_csv("actors.csv", show_col_types = FALSE)
directors_genres <- read_csv("directors_genres.csv", show_col_types = FALSE)
directors <- read_csv("directors.csv", show_col_types = FALSE)
movies_directors <- read_csv("movies_directors.csv", show_col_types = FALSE)
movies_genres <- read_csv("movies_genres.csv", show_col_types = FALSE)
movies <- read_csv("movies.csv", show_col_types = FALSE)
roles <- read_csv("roles.csv", show_col_types = FALSE)



#Información general sobre la base de datos:
##  a.  ¿Cuántas películas existen en la base de datos? 

num_peliculas <- movies %>% summarise(Total_Peliculas = n_distinct(id))
print(num_peliculas)
```

    ## # A tibble: 1 × 1
    ##   Total_Peliculas
    ##             <int>
    ## 1          388269

``` r
##  b.  ¿Cuántos directores?
num_directores <- directors %>% summarise(Total_Directores = n_distinct(id))
print(num_directores)
```

    ## # A tibble: 1 × 1
    ##   Total_Directores
    ##              <int>
    ## 1            86880

``` r
#Número promedio de géneros por director 
promedio_generos_por_director <- directors_genres %>%
  group_by(director_id) %>%
  summarise(num_generes = n_distinct(genre)) %>%
  summarise(promedio_generos_por_director = mean(num_generes))

promedio_generos_por_director
```

    ## # A tibble: 1 × 1
    ##   promedio_generos_por_director
    ##                           <dbl>
    ## 1                          2.41

``` r
#3.Reporte por “Role” que contiene : Número de películas, Número de actores , Número de actrices, Número de directores
conteo_peliculas <- movies_directors %>% summarise(num_peliculas= n_distinct(movie_id))
conteo_directores <- movies_directors %>% summarise(num_directores = n_distinct(director_id))


Conteo_ACT <- actors %>% 
  group_by(gender) %>%
  summarise(Conteo = n_distinct(id))

conteo_actores <- Conteo_ACT %>% filter(gender == "M") %>% pull(Conteo)
conteo_actrices <- Conteo_ACT %>% filter(gender == "F") %>% pull(Conteo)

# Crear el reporte
Reporte <- tibble(
  Número_Peliculas = conteo_peliculas$num_peliculas,
  Número_directores = conteo_directores$num_directores, 
  Número_actores = conteo_actores,
  Número_actrices = conteo_actrices
)


print(Reporte)
```

    ## # A tibble: 1 × 4
    ##   Número_Peliculas Número_directores Número_actores Número_actrices
    ##              <int>             <int>          <int>           <int>
    ## 1           315702             85794         513306          304412

``` r
#4.Reporte con la siguiente información: Información del director (ID, nombre, apellido),Número de películas que ha dirigido,Número de actores con los que ha trabajado,Género más común de sus películas

Informacion_directores <- directors %>%
  left_join(movies_directors, by = c("id" = "director_id"),relationship = "many-to-many") %>%
  left_join(directors_genres, by = c("id" = "director_id"),relationship = "many-to-many")


peliculas_por_director <- Informacion_directores %>%
  group_by(id, first_name, last_name) %>%
  summarise(Numero_Peliculas = n_distinct(movie_id), .groups = 'drop')


actores_por_director <- Informacion_directores %>%
  left_join(roles, by = "movie_id", relationship = "many-to-many") %>%
  group_by(id, first_name, last_name) %>%
  summarise(Numero_Actores = n_distinct(actor_id), .groups = 'drop')

genero_mas_comun <- Informacion_directores %>%
  group_by(id, first_name, last_name, genre) %>%
  summarise(Conteo = n(), .groups = 'drop_last') %>%
  arrange(desc(Conteo)) %>%
  slice(1) %>%
  select(id, first_name, last_name, genre)


Reporte_final <- peliculas_por_director %>%
  left_join(actores_por_director, by = c("id", "first_name", "last_name")) %>%
  left_join(genero_mas_comun, by = c("id", "first_name", "last_name"))

print(head(Reporte_final, 10))
```

    ## # A tibble: 10 × 6
    ##       id first_name         last_name   Numero_Peliculas Numero_Actores genre   
    ##    <dbl> <chr>              <chr>                  <int>          <int> <chr>   
    ##  1     1 Todd               1                          1              1 <NA>    
    ##  2     2 Les                12 Poissons                1              2 Short   
    ##  3     3 Lejaren            a'Hiller                   2             15 Drama   
    ##  4     4 Nian               A                          1              1 <NA>    
    ##  5     5 Khairiya           A-Mansour                  1              1 Documen…
    ##  6     6 Ricardo            A. Solla                   1              3 Drama   
    ##  7     8 Kodanda Rami Reddy A.                        35             86 Action  
    ##  8     9 Nageswara Rao      A.                         1              1 <NA>    
    ##  9    10 Yuri               A.                         1              1 Comedy  
    ## 10    11 Swamy              A.S.A.                     1              2 Drama

``` r
#5. Encuentre la distribución de “Roles” por las siguientes dimensiones:Película, Director

## distribución de roles por Película

roles_por_pelicula <- roles %>%
  group_by(movie_id) %>%
  summarise(n_roles = n())


distribucion_peliculas <- roles_por_pelicula %>%
  group_by(n_roles) %>%
  summarise(n_movies = n()) %>%  
  arrange(n_roles)


print(head(distribucion_peliculas,10))
```

    ## # A tibble: 10 × 2
    ##    n_roles n_movies
    ##      <int>    <int>
    ##  1       1    39558
    ##  2       2    24985
    ##  3       3    20928
    ##  4       4    19987
    ##  5       5    19551
    ##  6       6    15850
    ##  7       7    14405
    ##  8       8    13143
    ##  9       9    12014
    ## 10      10    11422

``` r
#Distribución de roles por Director


roles_por_director <- roles %>%
  left_join(movies_directors, by = "movie_id", relationship = "many-to-many") %>%  
  left_join(directors, by = c("director_id" = "id")) %>%  
  group_by(director_id, first_name, last_name) %>% 
  summarise(n_roles = n(), .groups = 'drop') %>%  
  arrange(desc(n_roles))




distribucion_directores <- roles_por_director %>%
  group_by(n_roles) %>%
  summarise(n_directors = n()) %>%
  arrange(n_roles)


print(head(distribucion_directores,10))
```

    ## # A tibble: 10 × 2
    ##    n_roles n_directors
    ##      <int>       <int>
    ##  1       1        5046
    ##  2       2        4265
    ##  3       3        4009
    ##  4       4        3846
    ##  5       5        3747
    ##  6       6        2847
    ##  7       7        2507
    ##  8       8        2128
    ##  9       9        2125
    ## 10      10        2112
