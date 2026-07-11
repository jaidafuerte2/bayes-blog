################################
##                            ##
## Colección 1 - Entrada 3    ##
##                            ##
################################

## 1. Cargar los paquetes

options(scipen = 999)
library(tidyverse)

## 2. Importar el conjunto de datos

sales <- read_csv(
  "blogs/blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)


## 3. Ventas y ganancias por transacción

# Sin zoom
ggplot(
  superstore,
  aes(
    x = Sales,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.45,
    color = "#2C7FB8"
  ) +
  geom_hline(
    yintercept = 0, # Crea una línea negra que cruza el eje Y en cero
    linetype = "dashed", # Tipo de línea punteada
    color = "firebrick" # Color de la linea
  ) +
  labs(
    title = "Ventas y ganancias por transacción",
    x = "Ventas",
    y = "Ganancias"
  ) + 
  theme_minimal()

# Con zoom en X
ggplot(
  superstore,
  aes(
    x = Sales,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.45,
    color = "#2C7FB8"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "firebrick"
  ) +
  coord_cartesian(xlim = c(0, 5000)) +
  labs(
    title = "Ventas y ganancias por transacción",
    x = "Ventas",
    y = "Ganancia"
  ) +
  theme_minimal()

# Con zoom en Y
ggplot(
  superstore,
  aes(
    x = Sales,
    y = Profit
  )
) +
  geom_point(
    alpha = 0.45,
    color = "#2C7FB8"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "firebrick"
  ) +
  coord_cartesian(ylim = c(-1000, 1000)) +
  labs(
    title = "Ventas y ganancias por transacción",
    x = "Ventas",
    y = "Ganancia"
  ) +
  theme_minimal()

## 4. Resumen por categoría

profit_category <-
  superstore %>%
  group_by(Category) %>%
  summarise(
    Sales = sum(Sales, na.rm = TRUE),
    Profit = sum(Profit, na.rm = TRUE),
    porc_Profit = (Profit / Sales) * 100, # Porcentaje de ganancia
    .groups = "drop"
  )
profit_category # produce:
#  Category          Sales  Profit
#  <chr>             <dbl>   <dbl>
#1 Furniture       742000.  18451.
#2 Office Supplies 719047. 122491.
#3 Technology      836154. 145455.

## 5. Ganancia por categoría

ggplot(
  profit_category,
  aes(
    x = reorder(Category, Profit), # Reordenar categorías por ganancias
    y = Profit
  )
) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Ganancia tota por categoría",
    y = "Ganancia",
    x = NULL
  ) +
  theme_minimal()


## 6. Comparación entre ventas y ganancias

profit_category # produce:
# A tibble: 3 × 4
#  Category          Sales  Profit porc_Profit
#  <chr>             <dbl>   <dbl>       <dbl>
#1 Furniture       742000.  18451.        2.49
#2 Office Supplies 719047. 122491.       17.0 
#3 Technology      836154. 145455.       17.4 

# Cambiar la tabla profit_category a formato largo
category_long <-
  profit_category %>%
  pivot_longer(
    cols = c(Sales, Profit),
    names_to = "Metric",
    values_to = "Value"
  )
category_long # produce:
# A tibble: 6 × 4
#  Category        porc_Profit Metric   Value
#  <chr>                 <dbl> <chr>    <dbl>
#1 Furniture              2.49 Sales  742000.
#2 Furniture              2.49 Profit  18451.
#3 Office Supplies       17.0  Sales  719047.
#4 Office Supplies       17.0  Profit 122491.
#5 Technology            17.4  Sales  836154.
#6 Technology            17.4  Profit 145455.#

ggplot(
  category_long,
  aes(
    x = Category,
    y = Value,
    fill = Metric
  )
) +
  geom_col(position = "dodge") + # Para que Profit y Sales no salgan 
                                 # en la misma columna sino una 
                                 # alado de la otra
  labs(
    title = "Ventas y ganancias por categoría",
    x = NULL,
    y = NULL,
    fill = NULL # Para que no salga la leyenda Metric en Profit y Sales
  ) +
  theme_minimal()


## 7. Resumen por región

profit_region <-
  superstore %>%
  group_by(Region) %>%
  summarise(
    Sales = sum(Sales, na.rm = TRUE),
    Profit = sum(Profit, na.rm = TRUE),
    porc_Profit = (Profit / Sales) * 100,
    .groups = "drop"
  )
profit_region # produce:
# A tibble: 4 × 3
#  Region    Sales  Profit
#  <chr>     <dbl>   <dbl>
#1 Central 501240.  39706.
#2 East    678781.  91523.
#3 South   391722.  46749.
#4 West    725458. 108418.


## 8. Ganancia por región

ggplot(
  profit_region,
  aes(
    x = reorder(Region, Profit),
    y = Profit
  )
) +
  geom_col(fill = "#41AE76") +
  coord_flip() +
  labs(
    title = "Ganancia total por región",
    x = NULL,
    y = "Ganancias"
  ) +
  theme_minimal()

# Con relleno de color por ventas
ggplot(
  profit_region,
  aes(
    x = Region,
    y = Profit,
    fill = Sales
  )
) +
  geom_col(position = "dodge") + 
  labs(
    title = "Ganancias por región",
    x = "Región",
    y = "Ganancias",
    fill = "Ventas" 
  ) +
  theme_minimal()
