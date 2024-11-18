Laboratorio 9
================
Andrea Soto
2024-11-13

``` r
Titanic.Data <- read.csv("titanic_MD.csv")
```

``` r
Titanic.Data <- Titanic.Data %>%
  mutate(Sex = ifelse(Sex == "?", NA, Sex))
```

``` r
missing_data_report <- Titanic.Data %>%
  summarise(across(everything(), list(
    count = ~ sum(is.na(.) | . == "")
  ))) %>%
  pivot_longer(cols = everything(), names_to = c("column", ".value"), names_sep = "_") %>%
  mutate(percent = (count / nrow(Titanic.Data)) * 100) %>%
  filter(percent > 0) %>%  
  arrange(desc(percent))

print(missing_data_report)
```

    ## # A tibble: 6 × 3
    ##   column   count percent
    ##   <chr>    <int>   <dbl>
    ## 1 Sex         51   27.9 
    ## 2 Age         25   13.7 
    ## 3 Parch       12    6.56
    ## 4 Embarked    12    6.56
    ## 5 Fare         8    4.37
    ## 6 SibSp        3    1.64

- Age (Edad) - Desviación estándar La edad es una variable numérica
  continua. Al utilizar la desviación estándar, se asume que los datos
  faltantes se distribuyen de manera similar al resto de los datos. Se
  calcula la media y la desviación estándar de la edad existente, y
  luego se reemplazan los valores faltantes con un valor aleatorio
  generado a partir de una distribución normal con la media y desviación
  estándar calculadas. Esto es útil cuando se asume una distribución
  normal y se quiere mantener la variabilidad de los datos.

- Sex (Sexo) - Imputación general  
  se calcula por medio de la moda puesto a que cualquier otro método no
  contamos con las variables necesarias para calcularlo, por otro lado,
  esta variable no afecta directamente puesto a que tenemos la columna
  que especifica el nombre del pasajero a bordo.

-Parch (Padres/Hijos) - Percentil Variable numérica discreta que
representa un conteo. El percentil es una buena opción para variables
numéricas, especialmente cuando hay valores atípicos. Al utilizar el
percentil, se reemplazan los valores faltantes con el valor
correspondiente a un percentil específico . Esto es útil cuando se
quiere preservar la distribución general de los datos y evitar que los
valores atípicos influyan demasiado en la imputación.

- Fare (Tarifa) - Imputación general La tarifa es una variable numérica
  continua. La imputación general puede referirse a diferentes métodos,
  como la media, la mediana o un valor modal. La elección dependerá de
  la distribución de la variable. Si la distribución es sesgada, la
  mediana podría ser una mejor opción que la media. La imputación
  general es una opción simple y rápida, pero puede introducir sesgos si
  la cantidad de datos faltantes es muy grande.

- SibSp (Hermanos/Cónyuge) - Desviación estándar Similar a la edad,
  SibSp es una variable numérica discreta que representa un conteo. Al
  utilizar la desviación estándar, se asume una distribución normal
  aproximada. Esta elección es razonable si se espera que la cantidad de
  hermanos/cónyuges se distribuya de manera similar a una distribución
  normal.

- Embarked (Puerto de embarque) - imputación por tarifa generalmente las
  tarifas más altas están asociadas con C donde emparcaron más pasajeros
  de primera clase y los pasajeros de tercera clase suelen embarcar en
  Q.

``` r
complete_rows_report <- function(data) {
  col_counts <- colSums(!is.na(data) & data != "")
  col_types <- sapply(data, class)
  complete_cols_df <- data.frame(
    complete_cases = col_counts,
    data_type = col_types
  )
  complete_cols_df <- complete_cols_df[complete_cols_df$complete_cases == nrow(data), ]
  
  return(complete_cols_df)
}
```

``` r
complete_columns <- complete_rows_report(Titanic.Data)
print(complete_columns)
```

    ##             complete_cases data_type
    ## PassengerId            183   integer
    ## Survived               183   integer
    ## Pclass                 183   integer
    ## Name                   183 character
    ## Ticket                 183 character
    ## Cabin                  183 character

``` r
#Columna SEX
##Método de Imputación General 
get_mode <- function (v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
Titanic.Data$Sex[is.na(Titanic.Data$Sex)] <- get_mode(Titanic.Data$Sex)
```

``` r
#Columna Age 
##Método:Standard Deviation Approach

media_Age <- mean(Titanic.Data$Age, na.rm = TRUE)
DE_Age <- sd(Titanic.Data$Age, na.rm = TRUE)
media_entero <- round(media_Age)
DE_entero <- round(DE_Age)

missing_age_indices <- which(is.na(Titanic.Data$Age))
imputed_age <- round(rnorm(length(missing_age_indices), media_entero, DE_entero))
Titanic.Data$Age[missing_age_indices] <- imputed_age
```

``` r
#Columna Pach
##Método de Imputación con la media 
media_parch <- mean(Titanic.Data$Parch, na.rm = TRUE)
entero <- round(media_parch)
Titanic.Data$Parch[is.na(Titanic.Data$Parch)] <- entero
```

``` r
# Columna FARE
# Percentile approach
fare_medians <- tapply(Titanic.Data$Fare, Titanic.Data$Embarked, median, na.rm = TRUE)

Titanic.Data$Fare[is.na(Titanic.Data$Fare) & Titanic.Data$Embarked == "S"] <- fare_medians["S"]
Titanic.Data$Fare[is.na(Titanic.Data$Fare) & Titanic.Data$Embarked == "C"] <- fare_medians["C"]
Titanic.Data$Fare[is.na(Titanic.Data$Fare) & Titanic.Data$Embarked == "Q"] <- fare_medians["Q"]
```

``` r
#Columna Embarkerd
##Imputación relacionada por columnas 
Titanic.Data$Embarked[Titanic.Data$Embarked == ""] <- NA
Titanic.Data$Embarked [is.na(Titanic.Data$Embarked) & Titanic.Data$Fare > 80 ] <- 'C'
Titanic.Data$Embarked [is.na(Titanic.Data$Embarked) & Titanic.Data$Fare <= 80 ] <- 'S'
```

``` r
#Columna SibSP
##Método de regresión lineal 
complete_data <- Titanic.Data[!is.na(Titanic.Data$SibSp), ]
missing_data <- Titanic.Data[is.na(Titanic.Data$SibSp), ]

modelo <- lm(SibSp ~ Parch + Fare, data = complete_data)
missing_data$predicted_SibSp <- predict(modelo, newdata = missing_data)
missing_data$predicted_SibSp <- round(missing_data$predicted_SibSp)
Titanic.Data$SibSp[is.na(Titanic.Data$SibSp)] <- missing_data$predicted_SibSp
```

Como manejamos diferentes tipos de datos, no todos los métodos son
aplicables para todas las columnas donde tenemos missing data, sin
embargoconsidero que el método de regresión lineal es el más apegado a
la realidad, puesto a que podemos utilizar variables ya conocidas para
predecir los missing values de una manera más objetiva basándonos en
datos reales que ya tenemos.

Como conclusiones: los datos no únicamente son numéricos, si no también
depende del uso que tenga cada columna para posteriormente relizar un
análisis predictivo adecuado, es necesario mantener la integridad de los
datos para mantener consistencia. Aunque los métodos simples como moda y
mediana son efectivos y fáciles de implementar, se ajustaron los datos
faltantes para mejorar su calidad, siempre teniendo en cuenta que los
valores imputados son estimaciones, no datos reales.

``` r
col_numericas <- Titanic.Data %>%
  select(PassengerId, Survived, Pclass, Age, SibSp, Parch, Fare)

Standarization <- col_numericas %>%
  mutate(across(everything(), ~ (mean(.)) / sd(.)))
MinMaxScaling <- col_numericas %>%
  mutate(across(everything(), ~ (. - min(.))/ (max(.)-min(.))))
MaxAbsScaler <- col_numericas %>%
  mutate(across(everything(), ~ . / max(abs(.))))
```

``` r
datos_normalizados <- Titanic.Data %>%
  bind_cols(
    Standarization %>% rename_with( ~ paste0(., "_std")),
    MinMaxScaling %>% rename_with( ~ paste0(., "_minmax")),
    MaxAbsScaler %>% rename_with( ~ paste0(., "_maxabs")),
    
  )
```

``` r
Titanic.Data2 <- read.csv("titanic.csv")
head(Titanic.Data2)
```

    ##   PassengerId Survived Pclass
    ## 1           2        1      1
    ## 2           4        1      1
    ## 3           7        0      1
    ## 4          11        1      3
    ## 5          12        1      1
    ## 6          22        1      2
    ##                                                  Name    Sex Age SibSp Parch
    ## 1 Cumings, Mrs. John Bradley (Florence Briggs Thayer) female  38     1     0
    ## 2        Futrelle, Mrs. Jacques Heath (Lily May Peel) female  35     1     0
    ## 3                             McCarthy, Mr. Timothy J   male  54     0     0
    ## 4                     Sandstrom, Miss. Marguerite Rut female   4     1     1
    ## 5                            Bonnell, Miss. Elizabeth female  58     0     0
    ## 6                               Beesley, Mr. Lawrence   male  34     0     0
    ##     Ticket    Fare Cabin Embarked
    ## 1 PC 17599 71.2833   C85        C
    ## 2   113803 53.1000  C123        S
    ## 3    17463 51.8625   E46        S
    ## 4  PP 9549 16.7000    G6        S
    ## 5   113783 26.5500  C103        S
    ## 6   248698 13.0000   D56        S

``` r
col_numericas2 <- Titanic.Data2 %>%
  select(PassengerId, Survived, Pclass, Age, SibSp, Parch, Fare)

Standarization2 <- col_numericas2 %>%
  mutate(across(everything(), ~ (mean(.)) / sd(.)))
MinMaxScaling2 <- col_numericas2 %>%
  mutate(across(everything(), ~ (. - min(.))/ (max(.)-min(.))))
MaxAbsScaler2 <- col_numericas2 %>%
  mutate(across(everything(), ~ . / max(abs(.))))

datos_normalizados2 <- Titanic.Data2 %>%
  bind_cols(
    Standarization2 %>% rename_with( ~ paste0(., "_std")),
    MinMaxScaling2 %>% rename_with( ~ paste0(., "_minmax")),
    MaxAbsScaler2 %>% rename_with( ~ paste0(., "_maxabs")),
    
  )
```

Al evaluar los métodos de normalización empleados en ambas bases de
datos, es fundamental considerar la naturaleza de cada variable. En el
caso de variables categóricas como ‘PassengerId’, ‘Survived’ y ‘Pclass’,
aunque expresadas numéricamente, su función es principalmente
identificativa o de clasificación. Por tanto, la aplicación de técnicas
de escalamiento no resulta apropiada, ya que estas variables no
representan mediciones continuas susceptibles de análisis estadístico.
En cuanto a las variables numéricas como ‘Age’, ‘SibSp’, ‘Parch’ y
‘Fare’, sí son candidatas para la normalización. La elección del método
dependerá del objetivo del análisis y de las características de los
datos. Por ejemplo, para la variable ‘Age’, Min-Max Scaling puede ser
una opción adecuada para acotar los valores a un rango específico y
facilitar su interpretación en modelos que son sensibles a la escala. En
el caso de ‘Fare’, la estandarización podría ser más apropiada si la
distribución de los datos es aproximadamente normal, permitiendo así
centrar los datos y escalarlos a una desviación estándar. Al comparar
ambas bases de datos, es crucial destacar las diferentes estrategias
empleadas para el manejo de datos faltantes en variables como ‘Sex’ y
‘SibSp’. Estas decisiones pueden influir significativamente en los
resultados del análisis, especialmente si la proporción de valores
faltantes es considerable. Es recomendable explorar los motivos detrás
de estas elecciones y evaluar sus implicaciones en términos de sesgos y
pérdida de información.
