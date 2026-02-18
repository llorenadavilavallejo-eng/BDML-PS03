#Limpiar espacio de trabajo
cat("\014")
rm(list = ls())

#Agregamos la librerias necesarias 

library(tidyverse)
library(rvest)
library(dplyr)
library(skimr)
library(stargazer)
library(boot)
library(knitr)
library(kableExtra)

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

#Punto 5
#Section 2: Gender–Labor Income Gap##
# A continuacion se realizara el  análisis  sobre   el perfil edad ingresos-difiere  entre  hombres  y  
#mujeres  y por otro lado frente a  la  brecha  salarial  de  género observada  puede  atribuirse  a  diferencias  en  las  
#características  observables  frente  vs  factores  no  explicados.

#El punto se divide en 3
#5.1 brecha  incondicional de genero vs condicional FWL
#5.2 Las interacciones de los perfiles edad-ingreso por sexo
#5.3 Peak ages con los intervalos de confianza

#Acontinuación vamos a preparar las varibales que se necesitara para realizar  los diferentes puntos 

final_db <- db %>%
  mutate(
    female = if_else(sex == 1, 0, 1),
    log_w = log(y_total_m)
  ) %>%
  filter(is.finite(log_w), y_total_m > 0)

#A continuacion se realizara se incluira los con controles , incluimos varios controles que se 
#adecuan a las caracteristicas, esto es con el fin de poder tener mejores resultados en nuestras regresiones

var_controles <- c("age",
                   "I(age^2)",
                   "totalHoursWorked", 
                   "as.factor(relab)",
                   "as.factor(maxEducLevel)",
                   "as.factor(oficio)",
                   "as.factor(sizeFirm)",
                   "as.factor(regSalud)",
                   "as.factor(cotPension)"
)   



##Aqui vamos a crear una funcion auxiliar con el fin de construir la formula ###

fml <- function(y, rhs_vec){
  as.formula(paste0(y," ~ ", paste(rhs_vec, collapse = "+")))
}

#Punto 5.1. a continuacion se calcula la brecha condicional####

####Modelo Condicional######
f_in <- lm(log_w  ~ female, data = final_db)


beta_in <- coef(f_in)[["female"]]
se_in <- summary(f_in)$coef["female", "Std. Error"]
r2_in <- summary(f_in)$r.squared
n_in <- nobs(f_in)


##FLW 5.1.B####Brecha condicional usando FRISCH-WAUGH-LOVELL########

controles <- var_controles

reg_fx <- lm(fml("female", controles), data = final_db)
res_fx <- resid(reg_fx)


############################################
# logaritmo de ingreso sobre x##
reg_fy <- lm(fml("log_w", controles), data = final_db)
res_fy <- resid(reg_fy)


#regresion residual (sin intercepto) segun el teorema de FWL

reg_fwl <- lm(res_fy ~ 0 + res_fx)
beta_fwl <- coef(reg_fwl)[["res_fx"]]
se_fwl <- summary(reg_fwl)$coef["res_fx","Std. Error"]

##En esta parte validaremos  con OLS

f_reg <- lm(fml("log_w", c("female", controles)), data = final_db)
stopifnot(abs(coef(f_reg)[["female"]] - beta_fwl) < 1e-8)


r2_cond <- summary(f_reg)$r.squared
n_cond <- nobs(f_reg)




#Punto 5.1.C### Error estandar BOOTSTRAP PARA BETAfemale### condicional, si utiliza bootstrap
#para aproximar la distribcion de nuestro ejercicio

#Para esto fijamos una semillapara repoducibilidad

set.seed(19)
B <- 1000

b_strap <- replicate(B, {
  idm <- sample.int(nrow(final_db), replace = TRUE)
  d_b <- final_db[idm, ]
  
  
  ##FWL en las muestra bootstrap
  
  fx_b <- resid(lm(fml("female", var_controles), data = d_b))
  fy_b <- resid(lm(fml("log_w", var_controles), data = d_b))
  
  coef(lm(fy_b ~ 0 + fx_b))["fx_b"]
  
})

#El SE Strap es la desviacion estandar de los coeficientes

se_strap <- sd(b_strap)


#Tabla de resumen Incondicional vs Condicional 

#Donde se podra observar la brecha frente a las caracteristicas que tomamos

tab <- tibble(
  Especificación = c( "Incondicional: log(w) ~ Female",
                      "Condicional FWL: Female | X"),
  `Beta_F` = round(c(beta_in, beta_fwl), 4),
  `SE_Analit` = round(c(se_in, se_fwl), 4),
  `SE_Bot`= round(c(NA, se_strap), 4),
  `R_2` = round(c(r2_in, r2_cond), 4),
  N = c(n_in, n_cond)
)

#realizaremos el ajuste de las tablas, con el fin de organizar la presentacion 
#Brecha de genero Incondiciona Vs Condicional

stargazer(f_in, f_reg,
          type = "html",
          out = "tabla_brecha_genero_slides.html",
          title = "Brecha de Genero en Ingresos Laborales",
          column.labels = c("Incondicional", "Condicional"),
          covariate.labels = c("Mujer", "Edad", "Edad²", "Horas"),
          omit = c("as.factor"),
          add.lines = list(
            c("SE Bootstrap Mujer", "", sprintf("%.4f", se_strap)),
            c("Controles completos", "No", "Si")
          ),
          keep.stat = c("n", "rsq"),
          digits = 3,
          notes = "SE analíticos en paréntesis. *** p<0.001, ** p<0.01, * p<0.05"
)

cat("\n✓ Tabla HTML generada: tabla_brecha_genero_slides.html\n")
cat("  → Abre en navegador → Copia → Pega en PowerPoint\n\n")

###5.2##Las interacciones de los perfiles edad-ingreso por sexo########


#Acontinaucion corremos el modelo

m_perfiles <- lm(
  fml("log_w",
      c("female",
        "age", "I(age^2)",
        "female:age", "female:I(age^2)",
        setdiff(var_controles, c("age", "I(age^2)"))
      )
  ),
  data = final_db
)


#Vamos a utilizar una cuncion auxilar # con el fin 

moda_f <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
  
}

####Acontinuacion vamos a crear la Base de predicción: controles fijos, pero la edad varia####

age_seq <- seq(min(final_db$age), max(final_db$age), by = 1)

Base_fija <- final_db %>%
  summarise(
    totalHoursWorked = mean(totalHoursWorked, na.rm = TRUE)
  ) %>%
  
  mutate(
    relab = as.integer(moda_f(final_db$relab)),
    maxEducLevel = as.integer(moda_f(final_db$maxEducLevel)),
    oficio = as.integer(moda_f(final_db$oficio)),
    sizeFirm = as.integer(moda_f(final_db$sizeFirm)),
    regSalud = as.integer(moda_f(final_db$regSalud)),
    cotPension = as.integer(moda_f(final_db$cotPension))
    
  )

###Hombres########

Base_m <- Base_fija %>%
  slice(rep(1, length(age_seq))) %>%
  mutate(age = age_seq, female = 0)

###Mujeres###

Base_f <- Base_fija %>%
  slice(rep(1, length(age_seq))) %>%
  mutate(age = age_seq, female = 1)

pred_m <- predict(m_perfiles, newdata = Base_m, se.fit =TRUE)
pred_f <- predict(m_perfiles, newdata = Base_f, se.fit =TRUE)

df_pred <- bind_rows(
  tibble(age = age_seq, female = 0, fit = pred_m$fit, se = pred_m$se.fit),
  tibble(age = age_seq, female = 1, fit = pred_f$fit, se = pred_f$se.fit)
)%>%
  mutate(sex = if_else(female == 1, "mujer", "hombre" ),
         lwr = fit - 1.96 *se,
         upr = fit + 1.96 *se
  )


################### GRAFICO###################################
##############Perfil salarial estimado de acuerdo a Edad y Sexo#####################################

df_pred <- df_pred %>%
  mutate(
    lwr = fit - 1.96*se,
    upr = fit + 1.96*se
  )
ggplot(df_pred, aes(x = age, y = fit, color = sex, fill = sex)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("hombre" = "dodgerblue4",
                                "mujer"  = "red")) +
  scale_fill_manual(values = c("hombre" = "dodgerblue4",
                               "mujer"  = "red")) +
  labs(
    title = "Perfil salarial estimado de acuerdo a Edad y Sexo",
    x = "Edad",
    y = "predicción log(ingreso laboral mensual)",
    color = "Sexo",
    fill  = "Sexo"
  ) +
  theme_minimal()




##5.3 Peak ages con los intervalos de confianza

#Acontinuacion learizaremos los peaks ages es decir los hombres y mujeres a que edad alcanzan su maximo
#ingreso

coef_prof <- coef(m_perfiles)

#hombre beta age y age^2
b1_m <- coef_prof[["age"]]
b2_m <- coef_prof[["I(age^2)"]]
peak_m <- -b1_m/ (2*b2_m)

#mujer

b1_f <- b1_m + coef_prof[["female:age"]]
b2_f <- b2_m + coef_prof [["female:I(age^2)"]]
peak_f <- -b1_f/ (2*b2_f)


########Bootstrap peak age y los intervalos de confianza###

#Para esto fijamos una semillapara repoducibilidad

set.seed(19)
B <- 1000


boot_peaks <- replicate(B, {
  
  idx <- sample.int(nrow(final_db), replace = TRUE)
  d_b <- final_db [idx, ]
  
  
  m_b <- lm(
    fml("log_w",
        c("female",
          "age", "I(age^2)",
          "female:age", "female:I(age^2)",
          setdiff(var_controles, c("age", "I(age^2)"))
        )
    ),
    data = d_b
  )
  
  
  cb <- coef(m_b) 
  b1_m_b <- cb[["age"]]
  b2_m_b <- cb[["I(age^2)"]]
  b1_f_b <- b1_m_b + cb [["female:age"]]
  b2_f_b <- b2_m_b + cb [["female:I(age^2)"]]
  
  c(
    peak_m = -b1_m_b/(2*b2_m_b),
    peak_f = -b1_f_b/(2*b2_f_b)
  )
  
})

##Extraemos ICS
ci_m <-quantile(boot_peaks["peak_m",], c(0.025, 0.975), na.rm = TRUE)
ci_f <-quantile(boot_peaks["peak_f",], c(0.025, 0.975), na.rm = TRUE)

##Tabla### peak age por sexo

tab_peaks <- tibble(
  Grupo  = c("Hombre", "Mujer"),
  `Peak Age` = round(c(peak_m, peak_f), 1),
  `IC 95% Inf.` = round(c(ci_m[1], ci_f[1]), 1),
  `IC 95% Sup.` = round(c(ci_m[2], ci_f[2]), 1)
)

tabla_peaks_html <- tab_peaks %>%
  kable(format = "html",
        caption = "Peak Ages por Sexo (Intervalos de Confianza Bootstrap 95%)",
        col.names = c("Grupo", "Peak Age", "IC 95% Inferior", "IC 95% Superior"),
        align = c("l", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                font_size = 16,
                position = "center") %>%
  row_spec(0, bold = TRUE, background = "#4472C4", color = "white") %>%
  row_spec(1:2, background = c("white", "#F2F2F2"))

save_kable(tabla_peaks_html, "tabla_peaks.html")

cat("Tabla peaks guardada: tabla_peaks.html")
cat("Abre en navegador y captura pantalla")
