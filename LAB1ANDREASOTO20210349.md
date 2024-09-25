Lab1 DW AS
================
Andrea Soto
2024-08-05

``` r
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(modeest)
##Importar todos los datos de excel 
##Definir la unicación del archivo 
#setwd("C:\\Users\\Andrea Soto Gonzalez\\Desktop\\PROGRAMAS DE DATA\\Lab1")

 
##leer todos los archivos de excel dentro de la carpeta que contengan al final -2023
files = list.files(pattern = "*-2023")

##funcion de para leer los archivos sin su fila de nombres puesto a que todos poseen el mismo 
##Funcion para extraer la fecha de la creación del archivo 
ASNFilas <-function(file){
  data<- read_xlsx(file, skip = 1)
  Fecha_archivo = file.info(file)$ctime
  data = data%>%
   mutate(Fecha_archivo = Fecha_archivo)
  return(data)
  
}


#Todos los archivos tienen el mismo nombre de columnas, por lo que se lo establecemos a la tabla combinada 
##aplicar a todos los archivos correspondientes la lectura, con bind_rows se hacen las observaciones por filas 
TABLA_COMBINADA = lapply(files , ASNFilas) %>% bind_rows()
```

    ## New names:
    ## • `` -> `...9`
    ## • `` -> `...10`

``` r
##Necesitamos que la nueva tabla tenga nombres de columnas por lo que leemos la del primer archivo
headers = read_xlsx(files[1], n_max = 0) %>% names()

#Se asignan al nuevo dataframe 
colnames(TABLA_COMBINADA) <- c(headers,"Fecha del archivo")



# Lista de vectores 
lista_vectores <- list(
  c(1, 1, 1, 7, 2, 7),
  c(6, 6, 9, 8, 7, 6),
  c(90, 1, 1, 1, 10, 11)
)

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Moda por vectores 
modas <- lapply(lista_vectores, moda)

# Mostrar las modas
modas 
```

    ## [[1]]
    ## [1] 1
    ## 
    ## [[2]]
    ## [1] 6
    ## 
    ## [[3]]
    ## [1] 1

``` r
#ver la tabla combinada 
print(head(TABLA_COMBINADA))
```

    ## # A tibble: 6 × 59
    ##   COD_VIAJE CLIENTE               UBICACION CANTIDAD PILOTO     Q CREDITO UNIDAD
    ##       <dbl> <chr>                     <dbl>    <dbl> <chr>  <dbl>   <dbl> <chr> 
    ## 1  10000002 TAQUERIA EL CHINITO …     76002     1433 Hecto… 358.       90 Camio…
    ## 2  10000003 TIENDA LA BENDICION …     76002     1857 Pedro… 464.       60 Camio…
    ## 3  10000004 TAQUERIA EL CHINITO       76002      339 Angel…  84.8      30 Panel 
    ## 4  10000005 CHICHARRONERIA EL RI…     76001     1644 Juan … 411        30 Camio…
    ## 5  10000006 UBIQUO LABS |||FALTA…     76001     1827 Luis … 457.       30 Camio…
    ## 6  10000007 CHICHARRONERIA EL RI…     76002     1947 Ismae… 487.       90 Camio…
    ## # ℹ 51 more variables: `Fecha del archivo` <dttm>, `` <dbl>, `` <dbl>,
    ## #   `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>,
    ## #   `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>,
    ## #   `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>,
    ## #   `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>,
    ## #   `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>,
    ## #   `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, `` <dbl>, …

``` r
# Imprimir el data frame con las modas 


##Se guaran los registros en un archivo de excel 
```

``` r
## EJERCICIO 3
data <- read_delim("C://Users//Andrea Soto Gonzalez//Desktop/DATA WRANLING//INE_PARQUE_VEHICULAR_080219.txt", delim = "|")
```

    ## New names:
    ## • `` -> `...11`

    ## Warning: One or more parsing issues, call `problems()` on your data frame for details,
    ## e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 2435294 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "|"
    ## chr (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_...
    ## dbl (2): ANIO_ALZA, CANTIDAD
    ## lgl (1): ...11
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
 head(data)
```

    ## # A tibble: 6 × 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ℹ 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
