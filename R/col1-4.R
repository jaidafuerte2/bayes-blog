################################
##                            ##
## Colección 1 - Entrada 4    ##
##                            ##
################################

## 1. Cargar los paquetes

options(scipen = 999) # evitar la notación científica
library(tidyverse)
library(lubridate)

## 2. Importar el conjunto de datos

superstore <- read_csv(
  "blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# 3. Conocer las ventas por año y mes

# Crear variables para el año y el mes
ventas_mensuales <-
  superstore |>
  mutate(
    Order_Date = mdy(`Order Date`),
    Year = year(Order_Date),
    Month = month(Order_Date),
    Mes = floor_date(Order_Date, "month")
  )
ventas_mensuales[,-20:-1] # produce:
# A tibble: 9,994 × 5
#   Profit Order_Date  Year Month Mes       
#    <dbl> <date>     <dbl> <dbl> <date>    
#1   41.9  2016-11-08  2016    11 2016-11-01
#2  220.   2016-11-08  2016    11 2016-11-01
#3    6.87 2016-06-12  2016     6 2016-06-01
#4 -383.   2015-10-11  2015    10 2015-10-01
#...
#8   90.7  2014-06-09  2014     6 2014-06-01
#9    5.78 2014-06-09  2014     6 2014-06-01
# ℹ 9,984 more rows
# ℹ Use `print(n = ...)` to see more rows

# Agrupar por mes y años
ventas_mensuales <- ventas_mensuales %>%
  group_by(Mes) %>%
  summarise(
    Ventas = sum(Sales),
    .groups = "drop"
  )
ventas_mensuales # produce:
# A tibble: 48 × 2
#  Mes        Ventas
#  <date>      <dbl>
#1 2014-01-01 14237.
#2 2014-02-01  4520.
#3 2014-03-01 55691.
#4 2014-04-01 28295.
#...
#8 2014-08-01 27909.
#9 2014-09-01 81777.
# ℹ 38 more rows
# ℹ Use `print(n = ...)` to see more rows

# 4. Gráfico de las ventas por mes y año

# Gráfico de líneas que muestra las ventas por mes
ggplot(
  ventas_mensuales,
  aes(
    x = Mes,
    y = Ventas
  )
) + 
  geom_line(linewidth = 0.8) +
  labs(
    title = "Ventas Mensuales y Anuales",
    x = "NULL",
    Y = "Ventas"
  ) +
  theme_minimal()

# 5.  Ventas anuales 

# Crear una variable con el año en la tabla superstore
ventas_anuales <-
  superstore %>%
  mutate(
    Order_Date = mdy(`Order Date`),
    Year = lubridate::year(Order_Date)
  ) 
ventas_anuales[,-20:-1] # produce:
# A tibble: 9,994 × 3
#  Profit Order_Date  Year
#   <dbl> <date>     <dbl>
#1   41.9  2016-11-08  2016
#2  220.   2016-11-08  2016
#3    6.87 2016-06-12  2016
#4 -383.   2015-10-11  2015
#...
#8   90.7  2014-06-09  2014
#9    5.78 2014-06-09  2014
# ℹ 9,984 more rows

# Resumir la tabla superstore por ventas anuales
ventas_anuales <- ventas_anuales |>
  group_by(Year) %>%
  summarise(
    Ventas = sum(Sales),
    .groups = "drop"
  )
ventas_anuales # produce:
# A tibble: 4 × 2
#   Year  Ventas
#   <dbl>   <dbl>
#1  2014 484247.
#2  2015 470533.
#3  2016 609206.
#4  2017 733215.

# 6.  Gráfico de las ventas anuales

# Gráfico de columnas que relaciona el año con las ventas
ggplot(
  ventas_anuales,
  aes(
    x = factor(Year),
    y = Ventas
  )
) + geom_col(fill = "#4C78A8") +
  labs(
    title = "Ventas totales por año",
    x = "Año",
    y = "Ventas"
  ) +
  theme_minimal()



# 7. Ventas mensuales

# Crear la variable Mes en la tabla superstore
ventas_mes <- 
  superstore |>
  mutate(
    Order_Date = mdy(`Order Date`),
    Mes = lubridate::month(
      Order_Date,
      label = TRUE, # Para poner los nombres de cada mes 
      abbr = TRUE # creo que es para abreviar el mes
    )
  )
ventas_mes[, -20:-1] # produce:
# A tibble: 9,994 × 3
#  Profit Order_Date Mes  
#   <dbl> <date>     <ord>
#1   41.9  2016-11-08 Nov  
#2  220.   2016-11-08 Nov  
#3    6.87 2016-06-12 Jun  
#4 -383.   2015-10-11 Oct  
#... 
#8   90.7  2014-06-09 Jun  
#9    5.78 2014-06-09 Jun  
# ℹ 9,984 more rows
# ℹ Use `print(n = ...)` to see more rows

# Agrupar por mes y resumir por ventas
ventas_mes <- ventas_mes |>
  group_by(Mes) |>
  summarise(
    Ventas = sum(Sales),
    .groups = "drop"
  )
ventas_mes # produce:
# A tibble: 12 × 2
#  Mes    Ventas
#  <ord>   <dbl>
#1 Jan    94925.
#2 Feb    59751.
#3 Mar   205005.
#4 Apr   137762.
#5 May   155029.
#6 Jun   152719.
#7 Jul   147238.
#8 Aug   159044.
#9 Sep   307650.
#10 Oct   200323.
#11 Nov   352461.
#12 Dec   325294.

# 8. gráfico de las ventas mensuales

# Diagrama de dispersión y gráfico de líneas que relaciona el mes
# con las ventas totales por mes
ggplot(
  ventas_mes,
  aes(
    x = Mes,
    y = Ventas,
    group = 1
  )
) + 
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Ventas agrupadas por mes",
    x = NULL,
    y = "Ventas"
  ) +
  theme_minimal()

