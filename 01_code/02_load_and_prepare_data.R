# Webscraping directo de la fuente de la pagina en todos los chunks
urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

# Recolección y merge de las bases de datos

base_geih <- purrr::map2_dfr(urls, 1:10, function(url, id){
  read_html(url) |>
    html_element("table.table-striped") |>
    html_table(fill = TRUE) |>
    clean_names() |>
    mutate(chunk_id = id)
})

# Filtro general: Mayores de edad y ocupados
base_geih_clean <- base_geih %>% filter(age >= 18, ocu == 1)

# Estructura general de las variables
str(base_geih_clean)

# Resumen de las variables
summary(base_geih_clean)

# Definición variables:
# para ciclo de vida (age, maxEduclevel, cotPension, estrato)
# para brechas (sex, oficio, sizefirm, relab)
# para impuestos (regsalud, cotpensión, sizefirm, oficio)

variables <- c("y_total_m","age", "max_educ_level", "cot_pension", "sex", "oficio", 
               "size_firm", "relab", "total_hours_worked", "reg_salud","estrato1",
               "p6050","formal", "chunk_id")

# Base con variables seleccionadas 
base_geih_clean_vf <- base_geih_clean[, variables]

# Limpieza de NA (sin ingresos reportados, sin cotización a salud y sin nivel máximo de educación)
colSums(is.na(base_geih_clean_vf))

df <- base_geih_clean_vf %>% drop_na(y_total_m, reg_salud, max_educ_level)

# Creación nuevas variables y redefinición
df <- df %>% mutate(log_ingreso = log(y_total_m))

df <- df %>%
  mutate(female = ifelse(sex == 1, 0, 1))

df <- df %>%
  mutate(household_head = ifelse(p6050 == 1, 1, 0))

df <- df %>%
  mutate(
    max_educ_level = as.factor(max_educ_level),
    estrato1 = as.factor(estrato1),
    relab = as.factor(relab),
    size_firm = as.factor(size_firm),
    oficio = as.factor(oficio),
    reg_salud = as.factor(reg_salud),
    cot_pension = as.factor(cot_pension),
    household_head = as.factor(household_head),
    formal = as.factor(formal)
  )

summary(df)
db <- as_tibble(df)
