################################
##                            ##
## Colección 1 - Entrada gt   ##
##                            ##
################################

## 1. Cargar los paquetes

options(scipen = 999) # desactivar la notación científica

library(tidyverse)
library(gt)

## 2. Importar el conjunto de datos
sales <- read_csv(
  "blog_ba/data/superstore_sales.csv",
  guess_max = 10000,
  show_col_types = FALSE
)

## 3.  Agrupar por categoría y resumir

sales_summary <-
  sales %>%
  group_by(Category) %>%
  summarise(
    Orders = n(),
    Sales = sum(Sales),
    Profit = sum(Profit),
    Avg_Sale = mean(Sales),
    .groups = "drop"
  ) 
sales_summary # produce:
# A tibble: 3 × 5
#  Category        Orders   Sales  Profit Avg_Sale
#  <chr>            <int>   <dbl>   <dbl>    <dbl>
#1 Furniture         2121 742000.  18451.  742000.
#2 Office Supplies   6026 719047. 122491.  719047.
#3 Technology        1847 836154. 145455.  836154.

## 4. Graficar la tabla sales_summary  con gt

sales_summary %>%
  gt() %>%
  # Poner título y subtítulo a la tabla con tab_header 
  tab_header(
    title = "Resumen de ventas por categoría",
    subtitle = "Dataser Superstore Sales"
  ) %>%
  # Cambiar de nombres a las columnas
  cols_label(
    Category = "Categoría",
    Orders = "Pedidos",
    Sales = "Ventas",
    Profit = "Utilidad",
    Avg_Sale = "Venta promedio"
  ) %>%
  # Darles formato a las divisas
  fmt_currency(
    columns = c(Sales, Profit),
    currency = "USD"
  ) %>%
  # Darles formato a los números
  fmt_number(
    columns = Avg_Sale,
    decimals = 2
  ) %>%
  # Alinear a la izquierda
  cols_align(
    align = "left",
    columns = Category
  ) %>%
  # Alinear a la derecha
  cols_align(
    align = "right",
    columns = c(Orders, Sales, Profit, Avg_Sale)
  ) %>%
  # Poner una nota al pie
  tab_source_note(
    source_note = md(
      "**Fuente:** Superstore Sales Dataset. Elaboración propia con R y gt."
    )
  )
  

