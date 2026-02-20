# Grupo de variables control de acuerdo con las características del empleado

controles <- c("age","I(age^2)","total_hours_worked", "max_educ_level",
               "size_firm", "reg_salud","cot_pension")   

# Función auxiliar

fml <- function(y, rhs_vec){
  as.formula(paste0(y," ~ ", paste(rhs_vec, collapse = "+")))
}

# Cálculo de la brecha condicional
# Modelo no condicionado

mod_3 <- lm(log_ingreso ~ female, data = db)

summary(mod_3)
beta_in <- coef(mod_3)[["female"]]
se_in <- summary(mod_3)$coef["female", "Std. Error"]
r2_in <- summary(mod_3)$r.squared
n_in <- nobs(mod_3)

# Modelo condicionado - usando FRISCH-WAUGH-LOVELL
# Female explicado por los controles

reg_fx <- lm(fml("female", controles), data=db)
res_fx <- resid(reg_fx)

# Logaritmo de ingreso explicado por los controles

reg_fy <- lm(fml("log_ingreso", controles), data = db)
res_fy <- resid(reg_fy)

# Regresión residual según el teorema de FWL

reg_fwl <- lm(res_fy ~ res_fx)
beta_fwl <- coef(reg_fwl)[["res_fx"]]
se_fwl <- summary(reg_fwl)$coef["res_fx","Std. Error"]

# Validación con OLS

f_reg <- lm(fml("log_ingreso", c("female", controles)), data = db)

r2_cond <- summary(f_reg)$r.squared
n_cond <- nobs(f_reg)

# Ejercicio errores estándar por Bootstrap

set.seed(1996)
B <- 1000

b_strap <- replicate(B, {
  idm <- sample.int(nrow(db), replace = TRUE)
  d_b <- db[idm, ]
  
  
  ##FWL en las muestras bootstrap
  
  fx_b <- resid(lm(fml("female", controles), data = d_b))
  fy_b <- resid(lm(fml("log_ingreso", controles), data = d_b))
  
  coef(lm(fy_b ~ fx_b))["fx_b"]
  
})

# SE Strap es la desviación estándar de los coeficientes

se_strap <- sd(b_strap)

# Tabla de resumen Incondicional vs Condicional 

tab <- tibble(
  Especificación = c( "Incondicional: log(Ingresos) ~ Female",
                      "Condicional FWL: Female | X"),
  `Beta_F` = round(c(beta_in, beta_fwl), 4),
  `SE_Analit` = round(c(se_in, se_fwl), 4),
  `SE_Bot`= round(c(NA, se_strap), 4),
  `R_2` = round(c(r2_in, r2_cond), 4),
  N = c(n_in, n_cond)
)

# Realizaremos el ajuste de las tablas para organizar la presentación 
# Brecha de género: Incondicional Vs Condicional

stargazer(mod_3, f_reg,
          type = "text",
          out = "02_output/tables/brecha_genero_sección_2.txt",
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

cat("\n✓ Tabla text generada: brecha_genero_sección_2\n")

# Perfiles edad-ingreso por sexo

moda_f <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE)) [1]
}

age_seq <-seq(min(db$age), max(db$age), by = 1)

Base_fija <- db %>%
  summarise(
    total_hours_worked = mean(total_hours_worked, na.rm = TRUE)
  ) %>%
  mutate(
    max_educ_level = factor(moda_f(db$max_educ_level)),
    oficio = factor(moda_f(db$oficio)),
    size_firm = factor(moda_f(db$size_firm)),
    reg_salud = factor(moda_f(db$reg_salud)),
    cot_pension = factor(moda_f(db$cot_pension))
  )

#Base para Hombres (female = 0)

Base_m <- Base_fija%>%
  slice(rep(1, length(age_seq))) %>%
  mutate(age = age_seq, female = 0)

#Base para Mujeres (female = 1)

Base_f <- Base_fija%>%
  slice(rep(1, length(age_seq))) %>%
  mutate(age = age_seq, female = 1) 


stopifnot(nrow(Base_m) == length(age_seq))
stopifnot(nrow(Base_f) == length(age_seq))

#Prediccion f_reg

pred_m <-predict(f_reg, newdata = Base_m, se.fit = TRUE)
pred_f <-predict(f_reg, newdata = Base_f, se.fit = TRUE)


#Unimos las predicciones

df_pred <- bind_rows(
  tibble(
    age = age_seq, 
    female = 0,
    fit = pred_m$fit,
    se = pred_m$se.fit),
  tibble(
    age = age_seq,
    female = 1,
    fit = pred_f$fit,
    se = pred_f$se.fit
  )
)%>%
  mutate(
    sex = if_else(female == 1, "mujer", "hombre"),
    lwr = fit - 1.96 * se,
    upr = fit + 1.96 * se
  )

#Realizamos el grafico frente a los perfiles

p_perfiles <- ggplot(df_pred, aes(x = age, y = fit, color = sex, fill = sex)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, color = NA) +
  geom_line(linewidth = 1 ) +
  scale_color_manual(values = c ("hombre" = "dodgerblue4",
                                 "mujer" = "red")) +
  scale_fill_manual(values = c ("hombre" = "dodgerblue4",
                                "mujer" = "red")) +
  labs(
    title =  "Perfil salarial estimado de acuerdo a Edad y Sexo",
    x = "Edad",
    y = "Prediccion log(ingreso laboral mensual)",
    color = "Sexo",
    fill = "Sexo"
  ) +
  theme_minimal()

print(p_perfiles)

ggsave("02_output/figures/grafico_perfiles_edad_sexo.png", p_perfiles,
       width = 10, height = 6, dpi = 300)

cat("\n✓ Gráfico generado: grafico_perfiles_edad_sexo\n")

# Peak ages con los intervalos de confianza

#Acontinuacion learizaremos los peaks ages es decir los hombres y mujeres a que edad alcanzan su maximo
#ingreso

coef_modelo <- coef(f_reg)

#hombre beta age y age^2
b1 <- coef_modelo[["age"]]
b2 <- coef_modelo[["I(age^2)"]]
peak_age <- -b1/ (2*b2)


########Bootstrap peak age y los intervalos de confianza###

#Para esto fijamos una semillapara repoducibilidad

set.seed(19)
B <- 1000


boot_peak <- replicate(B, {
  idx <- sample.int(nrow(db), replace = TRUE)
  d_b <- db[idx, ]
  
  m_b <- lm(fml("log_ingreso", c("female", controles)), data = d_b)
  
  cb <- coef(m_b) 
  b1_b <- cb[["age"]]
  b2_b <- cb[["I(age^2)"]]
  
  -b1_b / (2 * b2_b)
})


##Extraemos ICS
ci_peak <-quantile(boot_peak, c(0.025, 0.975), na.rm = TRUE)

cat(sprintf("IC 95%%: [%.1f - %.1f] años\n", ci_peak[1], ci_peak[2]))


##Tabla### peak age por sexo

tab_peaks <- tibble(
  `Grupo`  = "Ambos (Hombre y Mujer)",
  `Peak Age` = round(peak_age, 1),
  `IC 95% Inf.` = round(ci_peak[1], 1),
  `IC 95% Sup.` = round(ci_peak[2], 1)
)

print(kable(tab_peaks, format = "simple"))

#Exportamos la tabla

tabla_peaks_html <- tab_peaks %>%
  kable(format = "html",
        capation = " Peak age ",
        col.names = c("Grupo", "Peak Age (años)", "IC 95% inferior", "IC 95% superior"),
        align = c("l", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c ("striped", "hover", "condensed"),
                full_width = FALSE,
                font_size = 16,
                position = "center") %>%
  row_spec(0, bold = TRUE, background = "#4472C4", color = "white") %>%
  row_spec(1, background = "White")

save_kable(tabla_peaks_html, "02_output/tables/tablas_peaks.html")
cat("\n✓ Tabla html generada: tabla_peaks.html\n")
