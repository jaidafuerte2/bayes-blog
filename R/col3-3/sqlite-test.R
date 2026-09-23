library(DBI)
library(RSQLite)

# Crear una base SQLite en memoria
con <- dbConnect(SQLite(), ":memory:")

# Crear una tabla
dbExecute(con, "
  CREATE TABLE ventas (
    producto TEXT,
    categoria TEXT,
    ventas REAL
  )
")

# Insertar algunos datos
dbExecute(con, "
  INSERT INTO ventas VALUES
    ('Laptop', 'Tecnologia', 1200),
    ('Mouse', 'Tecnologia', 150),
    ('Escritorio', 'Muebles', 500),
    ('Silla', 'Muebles', 300),
    ('Monitor', 'Tecnologia', 800)
")

# Consultar la tabla con SQL
resultado <- dbGetQuery(con, "
  SELECT
    categoria,
    SUM(ventas) AS ventas_totales
  FROM ventas
  GROUP BY categoria
  ORDER BY ventas_totales DESC
")

resultado
