########################
##                    ##
## Entrada 1-1        ## 
##                    ##
########################

######## 1. Cargar librerías #########

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(lubridate)
library(skimr)
library(gt)


########## 2. Importar los datos ##########

# Cargar la dataset de ventas
sales <- read_csv(
  "blogs/blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

# Formatear la variable de fecha de la orden
sales <- sales |>
  mutate(
    order_date = mdy(`Order Date`)
  )
#glimpse(sales)

############## 3. Exploración inicial ############

#skim(sales)

############# 4. Ventas mensuales ###############

# Crear variable ventas mensuales donde se cambia las fechas a fechas
# mensuales
sales_monthly <-
  sales |>
  mutate(
    month = floor_date(order_date, "month")
  )
sales_monthly$month[1:20] # produce:
#1] "2016-11-01" "2016-11-01" "2016-06-01" "2015-10-01" "2015-10-01"
#[6] "2014-06-01" "2014-06-01" "2014-06-01" "2014-06-01" "2014-06-01"
#[11] "2014-06-01" "2014-06-01" "2017-04-01" "2016-12-01" "2015-11-01"
#[16] "2015-11-01" "2014-11-01" "2014-05-01" "2014-08-01" "2014-08-01"

# Agrupar las ventas por mes y resumir por suma de ventas mensuales
sales_monthly <- sales_monthly |>
  group_by(month) |>
  summarise(
    Sales = sum(Sales),
    .groups = "drop"
  )
sales_monthly # produce:
# A tibble: 48 × 2
#  month       Sales
#  <date>      <dbl>
#1 2014-01-01 14237.
#2 2014-02-01  4520.
#3 2014-03-01 55691.
#4 2014-04-01 28295.
#...
#9 2014-09-01 81777.
#10 2014-10-01 31453.
# ℹ 38 more rows
# ℹ Use `print(n = ...)` to see more rows

# Crear gráfico de líneas que relaciona las ventas con los meses
# de cada año
ggplot(
  sales_monthly,
  aes(month, Sales)
) +
  geom_line() +
  labs(
    title = "Ventas mensuales",
    x = NULL,
    y = "Ventas"
  )

############ 5. Distribución de la ventas ##############


# Distribución del valor de las ventas
ggplot(
  sales,
  aes(Sales)
) +
  geom_histogram(bins = 50) +
  #coord_cartesian(xlim = c(0, 700)) +
  labs(
    title = "Distribución del valor de las ventas",
    x = "Ventas",
    y = "Número de pedidos"
  )

# Distribución del valor de las ventas con zoom en el eje y
ggplot(
  sales,
  aes(Sales)
) +
  geom_histogram(bins = 800) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    title = "Distribución del valor de las ventas",
    subtitle = "Con zoom en el eje Y",
    x = "Ventas",
    y = "Número de pedidos"
  )

# Distribución del valor de las ventas con zoom en el eje x
ggplot(
  sales,
  aes(Sales)
) +
  geom_histogram(bins = 800) +
  coord_cartesian(xlim = c(0, 700)) +
  labs(
    title = "Distribución del valor de las ventas",
    subtitle = "Con zoom en el eje Y",
    x = "Ventas",
    y = "Número de pedidos"
  )

######## 6. Ventas por categoría ##########

# Agrupar por categoría y resumir por ventas
sales_category <-
  sales |>
  group_by(Category) |>
  summarise(
    Sales = sum(Sales),
    .groups = "drop"
  )
sales_category # produce:
# A tibble: 3 × 2
#  Category          Sales
#  <chr>             <dbl>
#1 Furniture       742000.
#2 Office Supplies 719047.
#3 Technology      836154.

# Gráfico de columnas
ggplot(
  sales_category,
  aes(
    #Category,
    reorder(Category, Sales), # Reordena las categorías por ventas
    Sales
  )
) +
  geom_col() +
  coord_flip() + # Cambiar los ejes
  labs(
    title = "Ventas por categoría",
    x = NULL,
    y = "Ventas"
  )

################# 7. Ventas por región ##############

# Agrupar las ventas por región y resumir por valor de las ventas
sales_region <-
  sales |>
  group_by(Region) |>
  summarise(
    Sales = sum(Sales),
    .groups = "drop"
  )
sales_region # produce:
# A tibble: 4 × 2
#  Region    Sales
#  <chr>     <dbl>
#1 Central 501240.
#2 East    678781.
#3 South   391722.
#4 West    725458.

# Grafico de columnas de las ventas por región
ggplot(
  sales_region,
  aes(
    reorder(Region, Sales), # Reordenar las regiones por ventas
    Sales
  )
) +
  geom_col() +
  coord_flip() + # Cambiar los ejes
  labs(
    title = "Ventas por región",
    x = NULL,
    y = "Ventas"
  )

############### Extras ###################

range(sales$order_date) # produce: [1] "2014-01-03" "2017-12-30"

n_distinct(sales$`Order ID`) # produce: 5009
