# ZADATAK 6 - survival analysis ================================================
# Dataset: plucaSurvival.xlsx
library(survival)
library(survminer)
library(readxl)
set.seed(123)

df <- as.data.frame(read_excel(file.choose(), sheet = 1))

df$event <- ifelse(df$status == 2, 1, 0)
df$sex <- factor(df$sex, levels = c(1, 2), labels = c("Male", "Female"))

# Broj opservacija, dogadjaja i cenzurisanih
N_total <- nrow(df)
N_events <- sum(df$event == 1)
N_cens <- sum(df$event == 0)
N_total; N_events; N_cens

# Medijalno vreme preživljavanja za ceo uzorak
fit_all <- survfit(Surv(time, event) ~ 1, data = df)
fit_all
summary(fit_all)$table["median"]

# Kaplan-Meier po polu (za grafik i poredjenje)
fit <- survfit(Surv(time, event) ~ sex, data = df)
ggsurvplot(fit, data = df, pval = TRUE, risk.table = TRUE)

summary(fit, times=365)$surv

# da li postoji statisticki znacajna razlika izmedju polova?
surv_diff <- survdiff(Surv(time, event) ~ sex, data = df)
surv_diff

# Statisticki znacaj promenljive pol. Univarijatni cox PH model
cox_sex <- coxph(Surv(time, event) ~ sex, data = df)
summary(cox_sex)

# multivarijantni cox PH model
cox_multi <- coxph(Surv(time, event) ~ sex + age + ph.ecog, data = df)
cox_multi_sum <- summary(cox_multi)
cox_multi_sum


df$AgeGroup <- cut(df$age,
                   breaks = quantile(df$age, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   labels = c("Low", "Medium", "High"),
                   include.lowest = TRUE)
table(df$AgeGroup)

fit_age <- survfit(Surv(time, event) ~ AgeGroup, data = df)
survdiff(Surv(time, event) ~ AgeGroup, data = df)

# 1) Tačna p-vrednost log-rank testa
lr <- survdiff(Surv(time, event) ~ AgeGroup, data = df)
chisq <- lr$chisq
p_val <- 1 - pchisq(lr$chisq, df = length(lr$n) - 1)
chisq; p_val

# 2) Tabela 1: n, events, median_survival, S_365 po grupama
res <- data.frame()
for (g in levels(df$AgeGroup)) {
  d <- df[df$AgeGroup == g, ]
  f <- survfit(Surv(time, event) ~ 1, data = d)
  s365 <- summary(f, times = 365, extend = TRUE)$surv
  res <- rbind(res, data.frame(
    AgeGroup        = g,
    n               = nrow(d),
    events          = sum(d$event),
    median_survival = unname(summary(f)$table["median"]),
    S_365           = round(s365, 3)
  ))
}
# redosled kao u zadatku: Medium, Low, High
res <- res[match(c("Medium", "Low", "High"), res$AgeGroup), ]
res

# 3) "Najžilavija" grupa = najveća medijana
best <- res$AgeGroup[which.max(res$median_survival)]
best
median_best <- max(res$median_survival)

# 4) Tabela 2: poređenje najžilavije grupe sa preostale dve
tab2 <- data.frame()
for (g in setdiff(res$AgeGroup, best)) {
  median_other <- res$median_survival[res$AgeGroup == g]
  delta_days   <- median_best - median_other
  extra_pm     <- delta_days / (median_other / 30)
  tab2 <- rbind(tab2, data.frame(
    Best                = best,
    Other               = g,
    Median_days_best    = median_best,
    Median_days_other   = median_other,
    Delta_days          = delta_days,
    Extra_days_per_month = round(extra_pm, 2)
  ))
}
tab2


# 1) Podskup: samo žene sa ph.ecog <= 1 (izbacujemo i NA vrednosti za ph.ecog)
sub <- df[df$sex == "Female" & !is.na(df$ph.ecog) & df$ph.ecog <= 1, ]
nrow(sub)

# 2) Kandidati za cutoff = kvantili starosti unutar ovog podskupa
probs   <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80)
cutoffs <- unique(quantile(sub$age, probs = probs, na.rm = TRUE))
cutoffs

n_min      <- 5
events_min <- 5
lvl        <- c("age < cutoff", "age >= cutoff")

# 3) Petlja po cutoff vrednostima
res <- data.frame()
for (cut in cutoffs) {
  sub$AgeBin <- factor(ifelse(sub$age < cut, lvl[1], lvl[2]), levels = lvl)
  
  n_g  <- sapply(lvl, function(l) sum(sub$AgeBin == l))
  ev_g <- sapply(lvl, function(l) sum(sub$event[sub$AgeBin == l]))
  
  # uključujemo samo cutoff gde obe grupe imaju >= 5 pacijenata i >= 5 događaja
  if (all(n_g >= n_min) && all(ev_g >= events_min)) {
    lr <- survdiff(Surv(time, event) ~ AgeBin, data = sub)
    res <- rbind(res, data.frame(
      cutoff  = as.numeric(cut),
      n_low   = n_g[1], n_high = n_g[2],
      ev_low  = ev_g[1], ev_high = ev_g[2],
      chisq   = lr$chisq,
      p_value = 1 - pchisq(lr$chisq, df = 1)
    ))
  }
}
res

# 4) "Najbolji" cutoff = najveća chisq vrednost
best <- res[which.max(res$chisq), ]
best
best_cut <- best$cutoff
best$chisq
best$p_value

# 5) Finalna tabela za izabrani cutoff
sub$AgeBin <- factor(ifelse(sub$age < best_cut, lvl[1], lvl[2]), levels = lvl)

final <- data.frame()
for (l in lvl) {
  d <- sub[sub$AgeBin == l, ]
  f <- survfit(Surv(time, event) ~ 1, data = d)
  final <- rbind(final, data.frame(
    AgeBin          = l,
    n               = nrow(d),
    events          = sum(d$event),
    median_survival = unname(summary(f)$table["median"]),
    S_365           = round(summary(f, times = 365, extend = TRUE)$surv, 3)
  ))
}
final