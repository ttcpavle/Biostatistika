# paketi potrebni za zadatke

paketi <- c(
  "readxl",     # citanje .xlsx fajlova (zadaci 1-6, 8)
  "glmnet",     # Ridge/LASSO (zadatak 1)
  "coin",       # wilcox_test (zadatak 2)
  "dunn.test",  # post-hoc posle Kruskal-Wallis testa (zadatak 3)
  "cluster",    # pam() za K-medoids (zadatak 4)
  "REdaS",      # Bartlett test i KMO (zadatak 5)
  "psych",      # faktorska analiza, fa() (zadatak 5)
  "survival",   # analiza preživljavanja (zadatak 6)
  "survminer",  # ggsurvplot (zadatak 6)
  "foreign",    # read.spss za Food.sav (zadatak 7)
  "biclust"     # biklasterovanje (zadatak 7)
)

nedostaju <- paketi[!paketi %in% rownames(installed.packages())]
if (length(nedostaju) > 0) install.packages(nedostaju)



# Datasetovi se ne nalaze u ovom repozitorijumu. U pitanju su 2 dataseta:
# - Acredis.xlsx
# - plucaSurvival.xlsx