dw-2024-parcial-1
================
Tepi
25/09/2024

# Examen parcial

Indicaciones generales:

- Usted tiene el período de la clase para resolver el examen parcial.

- La entrega del parcial, al igual que las tareas, es por medio de su
  cuenta de github, pegando el link en el portal de MiU.

- Pueden hacer uso del material del curso e internet (stackoverflow,
  etc.). Sin embargo, si encontramos algún indicio de copia, se anulará
  el exámen para los estudiantes involucrados. Por lo tanto, aconsejamos
  no compartir las agregaciones que generen.

## Sección 0: Preguntas de temas vistos en clase (20pts)

- Responda las siguientes preguntas de temas que fueron tocados en
  clase.

1.  ¿Qué es una ufunc y por qué debemos de utilizarlas cuando
    programamos trabajando datos? Una función universal pertenece a la
    librería de Numpy de Python, son utilizadas para trabajar con arrays
    de manera mucho más eficiente. Se permite realizar operaciones entre
    arrys sin escribir explicitamente las funciones

2.  Es una técnica en programación numérica que amplía los objetos que
    son de menor dimensión para que sean compatibles con los de mayor
    dimensión. Describa cuál es la técnica y de un breve ejemplo en R.
    Es BROADCASTING lo que permite realizar operaciones entre los arrays
    para realizar operaciones entre los de menor y mayor dimensión.

``` r
vector <- c(2, 2, 9)
matriz <- matrix(1:9, nrow = 3, byrow = TRUE)

resultado <- matriz + vector

print(resultado)
```

    ##      [,1] [,2] [,3]
    ## [1,]    3    4    5
    ## [2,]    6    7    8
    ## [3,]   16   17   18

3.  ¿Qué es el axioma de elegibilidad y por qué es útil al momento de
    hacer análisis de datos?

Es el rocedimiento por medio el cual nos aseguramos que los datos
cumplan con los criterios dados para la investigación o análisis y es
útil porque nos garantiza la correcta veracidad de los datos al momento
de analizarlos y hacer interpretaciones.

4.  Cuál es la relación entre la granularidad y la agregación de datos?
    Mencione un breve ejemplo. Luego, exploque cuál es la granularidad o
    agregación requerida para poder generar un reporte como el
    siguiente:

| Pais | Usuarios |
|------|----------|
| US   | 1,934    |
| UK   | 2,133    |
| DE   | 1,234    |
| FR   | 4,332    |
| ROW  | 943      |

La granualidad hace referencia al detalle de los datos, que tna
expecíficos son para utilizar en el análisis, la agrgación es de que
manera voy a combinar mis datos para realizar un resumen o análisis
final. La relación depende en que tan detallado necesito mi análisis
para poder realizar la recolección de mis datos, en este caso el proceso
de agragación estaría desde la recolección de datos, agrupar por país y
posterior formatear el dataframe para poder obtener el reporte como tal,
lleva una serie de operaciones como sumas para poder obtener el número
total de usuarios por país.

## Sección I: Preguntas teóricas. (50pts)

- Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
  deberá responder 5. Las 5 a responder estarán determinadas por un
  muestreo aleatorio basado en su número de carné.

- Ingrese su número de carné en `set.seed()` y corra el chunk de R para
  determinar cuáles preguntas debe responder.

``` r
set.seed(20210349) 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 2, 4, 5, 6, 8"

### Listado de preguntas teóricas

2.  Al momento de filtrar en SQL, ¿cuál keyword cumple las mismas
    funciones que el keyword `OR` para filtrar uno o más elementos una
    misma columna? La cláusula “IN” cumple funciones similares que ‘OR’
    SIEMPRE y cuando se cumplan las condiciones especificadas.

3.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

‘=’ se usa para asignar valores, cumple la misma función ‘\<-’ donde
coloco los elementos del array, variables, etc ‘==’ se usa para
comparar, o verificar si dos valores son exactamente iguales, símbolo de
igualdad en R.

5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

Es necesario someter los datos a un proceso de limpueza donde se leen
las funciones de Read.csv() o read.table() y se le coloca el sep(:) para
especificar la lectura de los datos

6.  ¿Qué es un vector y en qué se diferencia en una lista en R? Un
    vector contiene objetos de única dimensión pero tienen que ser del
    mismo tipo para que pueda ser ejecutado a diferencia de la lista que
    puede tener objetos de diferente tipo, combinaciones de numeros y
    caracteres. Las listas si se pueen combinar.

7.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?

    - El nuevo elemento
    - `NA`

Automáticamente se crea un nuevo elemento y se asigna al nivel
existente, esto sucede porqe en el proceso se busca no perder la mayor
cantidad de información posible, R está diseñado para ser más amigable
con la integración de datos. En el caso de N/A no puede ser aplicable
porque implica la pérdida de información

## Sección II Preguntas prácticas. (30pts)

- Conteste las siguientes preguntas utilizando sus conocimientos de R.
  Adjunte el código que utilizó para llegar a sus conclusiones en un
  chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas practicas

``` r
tabla <- readRDS("C:/Users/Andrea Soto Gonzalez/Desktop/DATA WRANLING/parcial_anonimo.rds")
head(tabla)
```

    ##         DATE Codigo Material Descripcion     Pais Distribuidor Territorio
    ## 1 2018-12-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 2 2018-11-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 3 2018-10-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 4 2018-09-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 5 2018-08-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ## 6 2018-07-01        637caff5    0cf3ec3d 4046ee34     9a47575c   69c1b705
    ##    Cliente    Marca Canal Venta Unidades plaza  Venta
    ## 1 9d6e1d65 61d7fbfc    7b48292e              2  26.50
    ## 2 9d6e1d65 61d7fbfc    7b48292e              0   0.00
    ## 3 9d6e1d65 61d7fbfc    7b48292e              3  39.75
    ## 4 9d6e1d65 61d7fbfc    7b48292e              3  39.75
    ## 5 9d6e1d65 61d7fbfc    7b48292e              8 106.00
    ## 6 9d6e1d65 61d7fbfc    7b48292e              3  39.75

## A

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
clientes_mas_de_un_pais <- tabla %>%
  group_by (Cliente) %>%
  summarize(total_px_cliente = n_distinct(Pais)) %>%
  filter(total_px_cliente>1)


clientes_rentabilidad <- tabla %>%
  group_by(Cliente) %>%
  summarize(rentabilidad_total = sum(Venta)) %>%
  right_join(clientes_mas_de_un_pais)
```

    ## Joining with `by = join_by(Cliente)`

``` r
clientes_rentabilidad
```

    ## # A tibble: 7 × 3
    ##   Cliente  rentabilidad_total total_px_cliente
    ##   <chr>                 <dbl>            <int>
    ## 1 044118d4              9436.                2
    ## 2 a17a7558             19818.                2
    ## 3 bf1e94e9                 0                 2
    ## 4 c53868a0             13813.                2
    ## 5 f2aab44e               400.                2
    ## 6 f676043b              3635.                2
    ## 7 ff122c3f             15359.                2

El cliente más rentable es ID = ‘a17a7558’ con un ingreso total de
‘Q19,817.70’

## B
