# ZADATAK 8 - logisticka regresija =============================================
# Dataset: Arcadis.xlsx
library(readxl)
set.seed(123)


df <- as.data.frame(read_excel(file.choose(), sheet = 1))

# 1) EmploymentCategory: low (min do 0.3), medium (0.3 do 0.6), high (> 0.6)
df$EmploymentCategory <- cut(df$Employment,
                             breaks = c(-Inf, 0.3, 0.6, Inf),
                             labels = c("low", "medium", "high"),
                             right = TRUE)

# 2) EducationCategory: low (min do 0.4), medium (0.4 do 0.6), high (> 0.6)
df$EducationCategory <- cut(df$education,
                            breaks = c(-Inf, 0.4, 0.6, Inf),
                            labels = c("low", "medium", "high"),
                            right = TRUE)

# 3) HighEmployment: 1 ako je EmploymentCategory == "high", inače 0
df$HighEmployment <- ifelse(df$EmploymentCategory == "high", 1, 0)

# Provera broja instanci
table(df$EmploymentCategory)
table(df$EducationCategory)
table(df$HighEmployment)


glm.fit <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation + Airpollution
               + Energy + Greenspace + EducationCategory,
               data = df,
               family = binomial)

summary(glm.fit)
cf <- summary(glm.fit)$coefficients
cf[, 4] < 0.05                                          # koji su koeficijenti znacajni
rownames(cf)[cf[, 4] < 0.05 & rownames(cf) != "(Intercept)"]   # znacajni prediktori
coef(glm.fit) # koeficijenti
exp(coef(glm.fit)) # odds ratio (OR)


glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

# Matrica konfuzije. To je poseban tip tabele kontigencije
cm <- table(Stvarno = factor(df$HighEmployment, levels = c(0, 1)),
            Predvidjeno = factor(glm.pred, levels = c(0,1)))
print(cm)
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[2,2] # true positive
  TN <- cmatrix[1,1] # true negative
  FN <- cmatrix[2,1] # false negative
  FP <- cmatrix[1,2] # false positive
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

compute.eval.metrics(cm)


# za seed od 123 do 132 napravi model, predvidi, i nadji evaluacione metrike
rez <- data.frame()
for (s in 123:132) {
  set.seed(s)
  
  n <- nrow(df)
  train_idx <- sample(1:n, size = round(0.7 * n))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]
  
  glm.fit <- glm(HighEmployment ~ crime + Drinkingwaterandsanitation + Airpollution
                 + Energy + Greenspace + EducationCategory,
                 data = train,
                 family = binomial)
  
  glm.probs <- predict(glm.fit, newdata = test, type = "response")
  glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
  
  cm <- table(factor(test$HighEmployment, levels = c(0, 1)),
              factor(glm.pred, levels = c(0, 1)))
  
  metrics <- compute.eval.metrics(cm)
  rez <- rbind(rez, data.frame(Seed = s, t(metrics)))
}
# prikaz tabele
print(round(rez, 3))

# koji seed daje najbolji F1
rez[which.max(rez$F1), ]
