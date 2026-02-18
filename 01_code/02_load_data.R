#3.1 - Webscraping directo de la fuente de la pagina en todos los chunks
urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

# recolección y merge de las bases de datos
base_geih <- purrr::map_dfr(urls, ~
                              read_html(.x) |>
                              html_element("table.table-striped") |>
                              html_table(fill = TRUE))