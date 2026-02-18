
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
