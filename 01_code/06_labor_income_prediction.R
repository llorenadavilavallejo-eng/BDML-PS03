# Partición de los datos según el chunk_id

train <- db %>% filter(chunk_id <= 7)
test  <- db %>% filter(chunk_id >= 8)

# Definición modelos antes utilizados y nuevos

mod_1 <- lm(log_ingreso ~ age + I(age^2), data = train)
mod_2 <- lm(log_ingreso ~ age + I(age^2) + total_hours_worked + relab, data = train)
mod_3 <- lm(log_ingreso ~ female, data = train)
mod_4 <- lm(fml("log_ingreso", c("female", controles)), data = train)

mod_5 <- lm(log_ingreso ~ age + I(age^2) + female + max_educ_level, data=train) # modelo mincer con brecha salarial
mod_6 <- lm(log_ingreso ~ age + I(age^2) + female + max_educ_level + total_hours_worked + relab + formal, data=train) # modelo 6 + horas, formal, tipo de relab y formalidad
mod_7 <- lm(log_ingreso ~ age + I(age^2) + I(age^3) + female + max_educ_level + total_hours_worked + relab + formal, data=train) # modelo 7 + age^3
mod_8 <- lm(log_ingreso ~ age + I(age^2) + max_educ_level*age + female*max_educ_level + total_hours_worked + relab + formal, data=train) # modelo 7 + interacciones
mod_9 <- lm(fml("log_ingreso", c("female","formal","estrato1", controles)), data = train) # modelo con diferentes características sociales y laborales

modelos <- list(mod_1,mod_2,mod_3,mod_4,mod_5,mod_6,mod_7,mod_8,mod_9)

# Cálculo RMSE

rmse <- function(model, data){
  pred <- predict(model, newdata = data)
  y <- data$log_ingreso
  sqrt(mean((y - pred)^2, na.rm=TRUE))
}

rmse_table <- sapply(modelos, rmse, data=test)

data.frame(Modelo = paste0("Mod_",1:9), RMSE = rmse_table)

# Cuadro comparativo

stargazer(modelos,
          type = "html",
          out = "02_output/tables/comparacion_modelos_sección_3.html",
          add.lines = list(
            c("RMSE Out-of-sample", round(rmse_table[1],3), round(rmse_table[2],3), 
              round(rmse_table[3],3), round(rmse_table[4],3), round(rmse_table[5],3), 
              round(rmse_table[6],3), round(rmse_table[7],3), round(rmse_table[8],3), 
              round(rmse_table[9],3), round(rmse_table[10],3))),
          title = "Comparación de los modelos de prediccion",
          dep.var.labels = "log(ingresos mensuales)",
          keep = c("age","I(age^2)","max_educ_level","female"),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3,
          omit.stat = c("f")) 

cat("\n✓ Tabla text generada: comparacion_modelos_sección_3\n")

# Cuadro comparativo mejorado

vars_mostrar <- c(
  "age",
  "I(age^2)",
  "I(age^3)",
  "total_hours_worked",
  "relab",
  "female",
  "max_educ_level",
  "size_firm",
  "reg_salud",
  "cot_pension",
  "formal",
  "age:max_educ_level",
  "max_educ_level:female",
  "estrato1"
)

tiene_var <- function(modelo, var){
  trms <- attr(terms(modelo), "term.labels")
  as.integer(var %in% trms)
}

tabla <- sapply(modelos, function(m){
  sapply(vars_mostrar, function(v) tiene_var(m, v))
})

tabla <- as.data.frame(tabla)
colnames(tabla) <- paste0("M",1:length(modelos))
rownames(tabla) <- vars_mostrar

tabla[] <- ifelse(tabla==1,"✔","")

tabla["R² ajustado",] <- round(sapply(modelos, function(x) summary(x)$adj.r.squared),3)
tabla["Observaciones",] <- sapply(modelos, nobs)
tabla["RMSE",] <- round(rmse_table,3)

nombres_es <- c(
  "Edad",
  "Edad²",
  "Edad³",
  "Horas trabajadas",
  "Relación laboral",
  "Mujer",
  "Max. nivel educativo",
  "Tamaño de la empresa",
  "Afiliación a salud",
  "Cotización a pensión",
  "Empleo formal",
  "Edad × Educación",
  "Mujer × Educación",
  "Estrato"
)

rownames(tabla)[1:length(nombres_es)] <- nombres_es

tabla_html <- kable(tabla,
                    format="html",
                    align="c",
                    caption="Especificaciones de los modelos y desempeño predictivo") %>%
  kable_styling(full_width=FALSE,
                bootstrap_options=c("striped","condensed"),
                font_size=15)

save_kable(tabla_html,"02_output/tables/tabla_modelos_resumen.html")

cat("\n✓ Tabla resumen generada: tabla_modelos_resumen.html\n")

# Errores de predicción del mejor modelo

modelo_final <- mod_9

y_test <- test$log_ingreso
y_hat  <- predict(modelo_final, newdata=test)
errors <- y_test - y_hat

grafico_errores_prediccion <- ggplot(data.frame(errors), aes(x=errors)) +
  geom_histogram(bins=40, fill="steelblue", color="white") +
  labs(title="Distribución de errores de predicción - Mejor modelo",
       x="Error (real - predicho)",
       y="Frecuencia") +
  theme_minimal()

ggsave(
  filename = "02_output/figures/Errores_prediccion.png",
  plot = grafico_errores_prediccion,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n✓ Gráfico generado: Errores_prediccion\n")

df_pred <- data.frame(
  y_test = y_test,
  y_hat  = y_hat
)

grafica_ingreso_observado_estimado <- ggplot(df_pred, aes(x = y_hat, y = y_test)) +
  geom_point(alpha = 0.3, color="steelblue") +
  geom_abline(slope=1, intercept=0, color="red", linewidth=1) +
  labs(title="Ingreso mensual observado y estimado",
       x="Log (Ingreso) estimado",
       y="Log (Ingreso) observado") +
  theme_minimal()

ggsave(
  filename = "02_output/figures/Ingreso_observado_estimado.png",
  plot = grafica_ingreso_observado_estimado,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n✓ Gráfico generado: Ingreso_observado_estimado\n")

# LOOCV

X<- model.matrix(modelo_final)
y <- model.response(model.frame(modelo_final))

beta_hat <- modelo_final$coefficients

G_inv<- solve(t(X)%*%X)
vec<- 1/(1-hatvalues(modelo_final))
N <- nrow(X)
LOO <- numeric(N)

for (i in 1:N) {
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * modelo_final$residuals[i]
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  LOO[i]<-  new_error
}

looCV_error <- mean(LOO)
loocv_mf <- sqrt(looCV_error)
rmse_mh  <- rmse_table[9]

tabla_comparacion <- data.frame(
  Modelo = "Mejor Modelo",
  `LOOCV` = round(loocv_mf, 3),
  `RMSE` = round(rmse_mh, 3)
)

tabla_html <- kable(tabla_comparacion, format = "html") %>%
  kable_styling(full_width = FALSE)

save_kable(tabla_html, "02_output/tables/loocv_vs_rmse.html")

# Observaciones dificiles de predecir e influyentes

X <- model.matrix(modelo_final, data=train)
X <- X[,-1]
X_std <- scale(X)
y <- train$log_ingreso
y <- matrix(y, ncol=1)

# Descomposición FWL
P <- X_std %*% solve(t(X_std)%*%X_std) %*% t(X_std)
M <- diag(nrow(X_std)) - P

y_tilde <- M %*% y
X_tilde <- M %*% X_std

XtX_inv <- solve(t(X_tilde)%*%X_tilde)
beta_hat <- XtX_inv %*% t(X_tilde) %*% y_tilde
beta_hat <- matrix(beta_hat, ncol=1)

# Loop de recalcular el modelo final luego de eliminar cada i-ésima observación
N <- nrow(X_tilde)
beta_influence <- numeric(N)

for(i in 1:N){
  xi <- matrix(X_tilde[i,], ncol=1)
  ei <- as.numeric(y_tilde[i,1] - t(xi)%*%beta_hat)
  hi <- as.numeric(t(xi)%*%XtX_inv%*%xi)
  beta_minus_i <- beta_hat - (XtX_inv %*% xi) * (ei/(1-hi))
  beta_influence[i] <- sum((beta_minus_i-beta_hat)^2)
}

# Influencia de cada observación
train$coef_influence <- beta_influence

top_inf <- train %>% arrange(desc(coef_influence)) %>% head(10)

train$leverage <- hatvalues(modelo_final)
train$residuals <- residuals(modelo_final)

cutoff <- 3*mean(train$leverage)

# Gráfica residuo vs leverage de cada observación en train

grafico_residuos_leverage <- ggplot(train,aes(residuals,leverage))+
  geom_point(alpha=.4)+
  geom_hline(yintercept=cutoff,color="red")+
  theme_minimal()

ggsave(
  filename = "02_output/figures/Residuos_leverage.png",
  plot = grafico_residuos_leverage,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n✓ Gráfico generado: residuos_leverage\n")

cat("\n✓ FIN DEL CÓDIGO \n")

