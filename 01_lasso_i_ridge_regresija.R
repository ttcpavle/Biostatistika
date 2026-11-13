# ZADATAK 1 - lasso i ridge regresija ==========================================
# Dataset: Arcadis.xlsx
library(glmnet)
library(readxl)
set.seed(123)

df <- as.data.frame(read_excel(file.choose(), sheet = 1))
# obican lr model (ne trazi se u ovom zadatku ali nema veze)
regmodel <- lm(Tourism ~ education + crime + health + affordability + Energy
               + Greenspace,
               data = df)
summary(regmodel)


prediktori <- c("education", "crime", "health", "affordability", "Energy", "Greenspace")

X <- as.matrix(df[,prediktori])
y <- df$Tourism

set.seed(123)
cv_ridge <- cv.glmnet(X,y, family = "gaussian", type.measure = "mse", alpha = 0)


round(cv_ridge$lambda.min, 4) # value of lambda that gives minimum cvm
best_lambda_ridge <- cv_ridge$lambda.min
# coef funkcija ekstraktuje koeficijente iz modela koje vracaju funkcije za modele
coef(cv_ridge, s = "lambda.min") 


koef <- coef(cv_ridge, s = "lambda.min")
rownames(koef)
zadrzane <- rownames(koef)[koef[, 1] != 0] # filtrirati koeficijente koji nisu 0
zadrzane <- zadrzane[zadrzane != "(Intercept)"] # izbacivanje slobodnog clana
length(zadrzane)
zadrzane

cv_ridge$lambda.1se # largest value of lambda such that error is within 1 standard error of the minimum.
coef(cv_ridge, s="lambda.1se")


# predikcije direktno iz cv.glmnet objekta za lambda.min
pred_Tourism <- as.numeric(predict(cv_ridge, newx = X, s = "lambda.min"))
rez <- data.frame(
  City = df$City,
  Tourism = df$Tourism,
  pred_Tourism = pred_Tourism,
  abs_err = abs(df$Tourism - pred_Tourism)
)
rez
rez$abs_err



# top 5 gradova sa najvecom apsolutnom greskom
rez[order(-rez$abs_err), ]
top5 <- head(rez[order(-rez$abs_err), ], 5)
top5

eu_as_high_crime <- df[df$crime > median(df$crime) & df$Continent %in% c(1, 3), ]
eu_as_high_crime

X_sub <- as.matrix(eu_as_high_crime[, prediktori])
y_sub <- eu_as_high_crime$Tourism

set.seed(123)
cv_ridge_2 <- cv.glmnet(X_sub, y_sub, family = "gaussian", type.measure = "mse", alpha = 0)
cv_ridge_2$lambda.min
pred_Tourism_2 <- as.numeric(predict(cv_ridge_2, newx = X_sub, s = "lambda.min"))

rez_2 <- data.frame(
  Continent    = eu_as_high_crime$Continent,
  City = eu_as_high_crime$City,
  Tourism = eu_as_high_crime$Tourism,
  pred_Tourism = pred_Tourism_2,
  abs_err = abs(eu_as_high_crime$Tourism - pred_Tourism_2)
)
rez_2

kontinenti <- c(1, 3)                          # 1 = Europa, 3 = Azija
tabela <- NULL

for (k in kontinenti) {
  deo <- rez_2[rez_2$Continent == k, ]
  top3 <- head(deo[order(-deo$abs_err), ], 3)
  tabela <- rbind(tabela, top3)
}
tabela