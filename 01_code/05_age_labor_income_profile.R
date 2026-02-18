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
