
train <- geih_model %>% filter(chunk_id <= 7)
test  <- geih_model %>% filter(chunk_id >= 8)

m1 <- lm(log_ingreso ~ age + age2, data = train)
m2 <- lm(log_ingreso ~ age + age2 + total_hours_worked + relab, data = train)
m3 <- lm(log_ingreso ~ female, data = train)

controls <- c("age","age2","total_hours_worked","max_educ_level",
              "size_firm","estrato1","household_head","reg_salud","cot_pension")

form_controls <- as.formula(
  paste("log_ingreso ~ female +", paste(controls, collapse = "+"))
)

m4 <- lm(form_controls, data = train)
summary(m4)

rmse <- function(model, data){
  pred <- predict(model, newdata = data)
  y <- data$log_ingreso
  sqrt(mean((y - pred)^2, na.rm=TRUE))
}

rmse_m1 <- rmse(m1, test)
rmse_m2 <- rmse(m2, test)
rmse_m3 <- rmse(m3, test)
rmse_m4 <- rmse(m4, test)

data.frame(
  Modelo = c("Age", "Age+Labor", "Gender", "Full controls"),
  RMSE = c(rmse_m1, rmse_m2, rmse_m3, rmse_m4)
)

m5 <- lm(log_ingreso ~ age*max_educ_level + age2 + total_hours_worked + relab,
         data=train)

m6 <- lm(log_ingreso ~ female + max_educ_level*reg_salud + age + age2 +
           total_hours_worked + relab,
         data=train)

m7 <- lm(log_ingreso ~ female + size_firm + age + age2 + total_hours_worked +
           relab + max_educ_level,
         data=train)

m8 <- lm(log_ingreso ~ female*household_head + age + age2 + total_hours_worked +
           relab + max_educ_level,
         data=train)

m9 <- lm(log_ingreso ~ age + age2 + total_hours_worked +
           relab + max_educ_level + size_firm,
         data=train)

models <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)

rmse_table <- sapply(models, rmse, data=test)

data.frame(
  Modelo = paste0("M",1:9),
  RMSE = rmse_table
)

best_model <- m4

y_test <- log(test$log_ingreso)
y_hat  <- predict(best_model, newdata=test)

errors <- y_test - y_hat

summary(errors)
sd(errors)

ggplot(data.frame(errors), aes(x=errors)) +
  geom_histogram(bins=40, fill="steelblue", color="white") +
  labs(title="Distribución de errores de predicción",
       x="Error (real - predicho)",
       y="Frecuencia") +
  theme_minimal()

test$abs_error <- abs(errors)

hard_obs <- test %>% arrange(desc(abs_error))

head(hard_obs, 10)

# residuos OLS
res <- residuals(best_model)

# leverage (diagonal de la matriz hat)
h <- hatvalues(best_model)

# errores LOOCV
loo_errors <- res/(1-h)

# LOOCV RMSE
loocv_rmse <- sqrt(mean(loo_errors^2))

loocv_rmse

# predicción en test
y_hat  <- predict(best_model, newdata = test)
y_test <- test$log_ingreso

# RMSE validación
rmse_validation <- sqrt(mean((y_test - y_hat)^2, na.rm = TRUE))

rmse_validation

test$pred <- predict(best_model, newdata = test)

test$error <- test$log_ingreso - test$pred
test$abs_error <- abs(test$error)

library(dplyr)

hard_obs <- test %>%
  arrange(desc(abs_error))

head(hard_obs, 10)

train_used <- model.frame(best_model)

nrow(train)
nrow(train_used)

X <- model.matrix(best_model)
u <- residuals(best_model)
h <- hatvalues(best_model)
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
