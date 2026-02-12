library(tidyverse)
library(rvest)
library(dplyr)

#3.1 - Webscraping directo de la fuente de la pagina en todos los chunks
urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

  # recolección y merge de las bases de datos
base_geih <- purrr::map_dfr(urls, ~
                              read_html(.x) |>
                              html_element("table.table-striped") |>
                              html_table(fill = TRUE))
  
  # Filtro general (edad - relab)
      # Mayores de edad
      #"Obrero o empleado de empresa particular"
      #"Obrero o empleado del gobierno"
      #"Empleado doméstico"
base_geih_clean <- base_geih %>% filter(age >= 18, relab %in% c(1,2,3))

#3.2 Data Cleaning
  # Estructura general de las variables
str(base_geih_clean)
  # Resumen de las variables
summary(base_geih_clean)
  # para ciclo de vida (age, maxEduclevel, cotPension)
  # para brechas (sex, oficio, sizefirm, relab)
  # para impuestos (regsalud, cotpensión, sizefirm, oficio)

variables <- c("age", "maxEducLevel", "cotPension", 
               "sex", "oficio", "sizeFirm", "relab",
               "regSalud",
               "y_total_m")

  # Base con variables seleccionadas 
base_geih_clean_vf <- base_geih_clean[, variables]

  # Limpieza de NA (sin ingresos reportados, sin cotización a salud, sin nivel maximo de educación)

df <- base_geih_clean_vf %>% drop_na(y_total_m, regSalud, maxEducLevel)

summary(df)
