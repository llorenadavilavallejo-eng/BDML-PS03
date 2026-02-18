library(tidyverse)
library(rvest)
library(dplyr)
library(skimr)
library(stargazer)
library(boot)

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
base_geih_clean <- base_geih %>% filter(age >= 18) #, relab %in% c(1,2,3)

#3.2 Data Cleaning
# Estructura general de las variables
str(base_geih_clean)
# Resumen de las variables
summary(base_geih_clean)
# para ciclo de vida (age, maxEduclevel, cotPension)
# para brechas (sex, oficio, sizefirm, relab)
# para impuestos (regsalud, cotpensión, sizefirm, oficio)

variables <- c("age", "maxEducLevel", "cotPension", 
               "sex", "oficio", "sizeFirm", "relab", "totalHoursWorked",
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

# Visualización agrupada de datos por sexo
ggplot(data = db , mapping = aes(x = age , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = maxEducLevel , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = cotPension , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = sex , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = oficio , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = sizeFirm , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = relab , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()
ggplot(data = db , mapping = aes(x = regSalud , y = y_total_m, group = as.factor(sex), color = as.factor(sex))) + geom_point()

# Distribución de edad en variables
ggplot(data=db) + geom_histogram(mapping = aes(x=y_total_m , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=maxEducLevel , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=cotPension , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=sex , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=oficio , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=sizeFirm , group=as.factor(age),fill=as.factor(age)))
ggplot(data=db) + geom_histogram(mapping = aes(x=relab , group=as.factor(age),fill=as.factor(age)))


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

# 4. Sección 1

# Modelo Ingreso - Edad (NO condicionado)
mod_1 <- lm(log(y_total_m) ~ age + I(age^2), data = db)
stargazer(mod_1,type="text", omit.stat=c("ser","f","adj.rsq"))

str(mod_1)
mod_1$coefficients
summary(mod_1)

# Edad pico (cuando la derivada del ingreso con respecto a la edad es igual a 0)

# Bootstrap

set.seed(1996)
B <- 1000

eta_mod_1<-rep(NA,B) #vector vacio para guardar el resultado
length(eta_mod_1)

for(i in 1:B){
  db_sample<- sample_frac(db,size=1,replace=TRUE)
  f<-lm(log(y_total_m) ~ age + I(age^2), data = db_sample)
  eta_mod_1[i]<- -(f$coefficients[2])/(2*f$coefficients[3]) #saves it in the above vector
}

eta_mod_1
plot(hist(eta_mod_1))
mean(eta_mod_1)
sqrt(var(eta_mod_1))
quantile(eta_mod_1,c(0.025,0.975))

# Modelo ingreso - edad - horas - relación

mod_2 <- lm(log(y_total_m) ~ age + I(age^2) + totalHoursWorked + factor(relab) , data = db)
stargazer(mod_2,type="text", omit.stat=c("ser","f","adj.rsq"))

str(mod_2)
mod_2$coefficients
summary(mod_2)

# Bootstrap

set.seed(1996)
B <- 1000

eta_mod_2<-rep(NA,B) #vector vacio para guardar el resultado
length(eta_mod_2)

for(i in 1:B){
  db_sample<- sample_frac(db,size=1,replace=TRUE)
  f<-lm(log(y_total_m) ~ age + I(age^2) + totalHoursWorked + factor(relab), data = db_sample)
  eta_mod_2[i]<- -(f$coefficients[2])/(2*f$coefficients[3]) #saves it in the above vector
}

eta_mod_2
plot(hist(eta_mod_2))
mean(eta_mod_2)
sqrt(var(eta_mod_2))
quantile(eta_mod_2,c(0.025,0.975))

# Tabla comparativa

stargazer(mod_1, mod_2,
          type = "text",
          add.lines = list(
            c("Edad Pico", round(mean(eta_mod_1),2), round(mean(eta_mod_2),2)),
            c("IC Bootstrap (2.5%)", round(quantile(eta_mod_1,c(0.025,0.975))[1],2), round(quantile(eta_mod_1,c(0.025,0.975))[2],2)),
            c("IC Bootstrap (97.5%)", round(quantile(eta_mod_2,c(0.025,0.975))[1],2), round(quantile(eta_mod_2,c(0.025,0.975))[2],2)),
            c("Controles de Empleo", "No", "Sí")
          ),
          notes = "Fuente: GEIH 2018 Bogotá.",
          notes.align = "l")

# Visualización perfiles predichos

edades <- 18:80
moda_relab <- as.numeric(names(which.max(table(db$relab))))

df_preds <- data.frame(
  age = edades,totalHoursWorked = mean(db$totalHoursWorked, na.rm = TRUE),relab = moda_relab)

df_preds <- df_preds %>%
  mutate(
    pred_mod_1 = predict(mod_1, newdata = df_preds),
    pred_mod_2 = predict(mod_2, newdata = df_preds)
  )

ggplot(df_preds, aes(x = age)) +
  # Mod_1 (Incondicional)
  geom_line(aes(y = pred_mod_1, color = "Incondicional (Solo Edad)"), size = 1) +
  # Mod_2 (Condicional)
  geom_line(aes(y = pred_mod_2, color = "Condicional (Horas + Relab)"), size = 1, linetype = "dashed") +
  
  labs(title = "Perfil Edad-Ingreso: Modelos Incondicional vs. Condicionado",
       x = "Edad", 
       y = "Log(Ingreso Mensual)",
       color = "Modelo") +
  theme_minimal() +
  scale_color_manual(values = c("Incondicional (Solo Edad)" = "blue", 
                                "Condicional (Horas + Relab)" = "red"))