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

# Interacciones de los perfiles edad-ingreso por sexo

m_perfiles <- lm(
  fml("log_ingreso",
      c("female",
        "age", "I(age^2)",
        "max_educ_level"*"age", "female"*"max_educ_level",
        setdiff(controles, c("age", "I(age^2)"))
      )
  ),
  data = db
)

# Función auxilar # con el fin 

moda_f <- function(x) {
  ux <- na.omit(x)
  names(sort(table(ux), decreasing = TRUE))[1]
  
}

# Base de predicción: controles fijos, pero la edad varia

age_seq <- seq(min(db$age), max(db$age), by = 1)

Base_fija <- db %>%
  summarise(
    total_hours_worked = mean(total_hours_worked, na.rm = TRUE)
  ) %>%
  mutate(
    relab = factor(moda_f(db$relab), levels = levels(db$relab)),
    max_educ_level = factor(moda_f(db$max_educ_level), levels = levels(db$max_educ_level)),
    oficio = factor(moda_f(db$oficio), levels = levels(db$oficio)),
    size_firm = factor(moda_f(db$size_firm), levels = levels(db$size_firm)),
    reg_salud = factor(moda_f(db$reg_salud), levels = levels(db$reg_salud)),
    cot_pension = factor(moda_f(db$cot_pension), levels = levels(db$cot_pension))
    )

# Hombres

Base_m <- Base_fija %>%
  slice(rep(1, length(age_seq))) %>%
  mutate(age = age_seq, female = 0)

# Mujeres

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

# Gráfico Perfil salarial estimado de acuerdo a Edad y Sexo

df_pred <- df_pred %>%
  mutate(
    lwr = fit - 1.96*se,
    upr = fit + 1.96*se
  )

grafico_edad_sexo <- ggplot(df_pred, aes(x = age, y = fit, color = sex, fill = sex)) +
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

ggsave(
  filename = "02_output/figures/Perfil_edad_ingreso_sexo.png",
  plot = grafico_edad_sexo,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n✓ Gráfico generado: Perfil_edad_ingreso_sexo\n")


# Peak ages con intervalos de confianza

coef_prof <- coef(m_perfiles)

# Hombre beta age y age^2
b1_m <- coef_prof[["age"]]
b2_m <- coef_prof[["I(age^2)"]]
peak_m <- -b1_m/ (2*b2_m)

# Mujer

b1_f <- b1_m + coef_prof[["female:age"]]
b2_f <- b2_m + coef_prof [["female:I(age^2)"]]
peak_f <- -b1_f/ (2*b2_f)

# Bootstrap peak age e intervalos de confianza

set.seed(1996)
B <- 1000

boot_peaks <- replicate(B, {
  
  idx <- sample.int(nrow(db), replace = TRUE)
  d_b <- db [idx, ]
  
  m_b <- lm(
    fml("log_ingreso",
        c("female",
          "age", "I(age^2)",
          "female:age", "female:I(age^2)",
          setdiff(controles, c("age", "I(age^2)"))
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

# Tabla Peak age por sexo

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

save_kable(tabla_peaks_html, file = "02_output/tables/tabla_peaks.html")

cat("\n✓ Tabla html generada: tabla_peaks.html\n")
