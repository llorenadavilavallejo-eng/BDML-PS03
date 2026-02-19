# Partición de los datos según el chunk_id

train <- db %>% filter(chunk_id <= 7)
test  <- db %>% filter(chunk_id >= 8)

# Limpieza previa por nuevos factores en oficio base test

factor <- c("oficio")

for(v in factor){
  train[[v]] <- factor(train[[v]])
  test[[v]]  <- factor(test[[v]], levels = levels(train[[v]]))
}

# Definición modelos antes utilizados y nuevos

mod_1 <- lm(log_ingreso ~ age + I(age^2), data = train)
mod_2 <- lm(log_ingreso ~ age + I(age^2) + total_hours_worked + relab, data = train)
mod_3 <- lm(log_ingreso ~ female, data = train)
mod_4 <- lm(fml("log_ingreso", c("female", controles)), data = train)
mod_5 <- lm(fml("log_ingreso", c("female","female:age", "female:I(age^2)", controles)), data = train)

mod_6 <- lm(log_ingreso ~ age + I(age^2) + female + max_educ_level, data=train) # modelo mincer con brecha salarial y formalidad
mod_7 <- lm(log_ingreso ~ age + I(age^2) + female + max_educ_level + total_hours_worked + relab + formal, data=train) # modelo 6 + horas, formal y tipo de relab
mod_8 <- lm(log_ingreso ~ age + I(age^2) + I(age^3) + female + max_educ_level + total_hours_worked + relab + formal, data=train) # modelo 7 + age^3
mod_9 <- lm(log_ingreso ~ age + I(age^2) + max_educ_level*age + female*max_educ_level + total_hours_worked + relab + formal, data=train) # modelo 7 + interacciones
mod_10 <- lm(fml("log_ingreso", c("female","formal","estrato1","household_head", controles)), data = train) # modelo con diferentes características sociales y laborales

modelos <- list(mod_1,mod_2,mod_3,mod_4,mod_5,mod_6,mod_7,mod_8,mod_9,mod_10)

# Cálculo RMSE

rmse <- function(model, data){
  pred <- predict(model, newdata = data)
  y <- data$log_ingreso
  sqrt(mean((y - pred)^2, na.rm=TRUE))
}

rmse_table <- sapply(modelos, rmse, data=test)

data.frame(Modelo = paste0("Mod_",1:10), RMSE = rmse_table)

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

# Errores de predicción del mejor modelo

modelo_final <- mod_4

y_test <- test$log_ingreso
y_hat  <- predict(modelo_final, newdata=test)
errors <- y_test - y_hat

summary(errors)
sd(errors, na.rm = TRUE)

grafico_errores_prediccion <- ggplot(data.frame(errors), aes(x=errors)) +
  geom_histogram(bins=40, fill="steelblue", color="white") +
  labs(title="Distribución de errores de predicción - Mejor modelo",
       x="Error (real - predicho)",
       y="Frecuencia") +
  theme_minimal()

ggsave(
  filename = "02_output/figures/Errores_prediccion .png",
  plot = grafico_errores_prediccion,
  width = 7,
  height = 5,
  dpi = 300
)

cat("\n✓ Gráfico generado: Errores_prediccion\n")

# LOOCV

res <- residuals(modelo_final)
h <- hatvalues(modelo_final)

loo_errors <- res/(1-h)
loocv_rmse <- sqrt(mean(loo_errors^2))
loocv_rmse

X<- model.matrix(modelo_final)
y <- model.response(model.frame(modelo_final))

beta_hat <- modelo_final$coefficients

## Calculate the inverse of  (X'X), call it G_inv
G_inv<- solve(t(X)%*%X)

## and 1/1-hi
vec<- 1/(1-hatvalues(modelo_final))

N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation
for (i in 1:N) {
  # get the new beta
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * modelo_final$residuals[i]
  ## get the new error
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  LOO[i]<-  new_error
}

looCV_error <- mean(LOO)
sqrt(looCV_error)

# predicción en test
y_hat  <- predict(modelo_final, newdata = test)
y_test <- test$log_ingreso

# RMSE validación
rmse_validation <- sqrt(mean((y_test - y_hat)^2, na.rm = TRUE))

rmse_validation

test$pred <- predict(modelo_final, newdata = test)

test$error <- test$log_ingreso - test$pred
test$abs_error <- abs(test$error)

library(dplyr)

hard_obs <- test %>%
  arrange(desc(abs_error))

head(hard_obs, 10)

train_used <- model.frame(modelo_final)

nrow(train)
nrow(train_used)

# Observaciones influyentes
X <- model.matrix(modelo_final)
u <- residuals(modelo_final)
h <- hatvalues(modelo_final)
XtX_inv <- solve(t(X) %*% X)

beta_influence <- sapply(1:nrow(X), function(i){
  
  xi <- matrix(X[i,], ncol = 1)
  
  delta_beta <- XtX_inv %*% xi * (u[i] / (1 - h[i]))
  
  sum(delta_beta^2)
  
})

train_used$beta_influence <- beta_influence

influential <- train_used %>%
  arrange(desc(beta_influence))

head(influential, 15)

summary(influential$beta_influence)

hist(influential$beta_influence, breaks = 50,
     main = "Distribución de influencia en coeficientes",
     xlab = "||β(-i) − β||²")
