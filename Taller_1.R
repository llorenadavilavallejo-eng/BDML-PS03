library(tidyverse)
library(rvest)
library(dplyr)
library(skimr)

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
# para brechas (sex, oficio, sizefirm, relab, totalHoursWorked)
# para impuestos (regsalud, cotpensión, sizefirm, oficio)

variables <- c("age", "maxEducLevel", "cotPension", 
               "sex", "oficio", "sizeFirm", "relab","totalHoursWorked",
               "regSalud",
               "y_total_m")

# Base con variables seleccionadas 
base_geih_clean_vf <- base_geih_clean[, variables]

# Limpieza de NA (sin ingresos reportados, sin cotización a salud, sin nivel maximo de educación) no se imputan por no afectar la var

df <- base_geih_clean_vf %>% drop_na(y_total_m, regSalud, maxEducLevel)

#3.3 Resumen y visualización

summary(df)
db <- as_tibble(df)
skim(db) %>% head()

# Visualización de los datos contra ingreso
ggplot(data = db , mapping = aes(x = age , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = maxEducLevel , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = cotPension , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = sex , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = oficio , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = sizeFirm , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = relab , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = regSalud , y = y_total_m)) + geom_point(col = "red" , size = 0.5)
ggplot(data = db , mapping = aes(x = totalHoursWorked , y = y_total_m)) + geom_point(col = "red" , size = 0.5)

# Visualización agrupada de datos por sexo
ggplot(data = db , mapping = aes(x = age , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = maxEducLevel , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = cotPension , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = sex , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = oficio , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = sizeFirm , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = relab , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = regSalud , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = totalHoursWorked , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()

# Distribución de edad en variables
ggplot(data=db) + geom_histogram(mapping = aes(x=y_total_m , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=maxEducLevel , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=cotPension , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=sex , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=oficio , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=sizeFirm , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=relab , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=totalHoursWorked , group=as.factor(age),fill=as.factor(age)))


# cajas por variable
ggplot(data=db , mapping = aes(as.factor(sex), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(age), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(maxEducLevel), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(cotPension), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(oficio), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(sizeFirm), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(relab), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(regSalud), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")
ggplot(data=db , mapping = aes(as.factor(totalHoursWorked), y_total_m)) + geom_boxplot() + geom_point(aes(colour=as.factor(sex))) + 
  scale_color_manual(values = c("0"="red" , "1"="blue"),label = c("0"="Hombre" , "1"="Mujer"),name = "Sexo")

# Valores faltantes

db_miss <- skim(db) %>% select( skim_variable, n_missing)
Nobs <- nrow(db) 
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
db_miss <- db_miss %>% arrange(-n_missing) ## or arrange(desc(n_missing))
ggplot(db_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5))  