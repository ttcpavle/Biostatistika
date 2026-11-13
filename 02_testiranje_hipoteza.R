# ZADATAK 2 - testiranje hipoteza ==============================================
# Dataset: Arcadis.xlsx
library(coin)
library(readxl)
set.seed(123)
df <- as.data.frame(read_excel(file.choose(), sheet = 1))

# jedino mesto gde se pravi faktor
df$Continent <- factor(df$Continent, levels = c(1, 2, 3),
                       labels = c("Europe", "North America", "Asia"))

# zadatak pod a)
ea <- droplevels(subset(df, Continent %in% c("Europe", "Asia")))

sh_eu <- shapiro.test(ea$Easeofdoingbusiness[ea$Continent == "Europe"])
sh_as <- shapiro.test(ea$Easeofdoingbusiness[ea$Continent == "Asia"])
print(sh_eu)
print(sh_as)

# izbor testa u zavisnosti od Shapiro rezultata
if (sh_eu$p.value > 0.05 & sh_as$p.value > 0.05) {
  print(t.test(Easeofdoingbusiness ~ Continent, data = ea))
} else {
  print(wilcox_test(Easeofdoingbusiness ~ Continent, data = ea))
}



# zadatak pod b)
med <- median(df$Greenspace)   # medijana na CELOM skupu

gs <- droplevels(subset(df, Continent %in% c("Europe", "Asia") &
                          Greenspace > med))
tabela <- data.frame()
for(v in c("crime", "health", "Energy")){
  x <- gs[[v]]
  g <- gs$Continent
  p_eu <- shapiro.test(x[g=="Europe"])$p.value
  p_as <- shapiro.test(x[g=="Asia"])$p.value
  if(p_eu>0.05 & p_as>0.05){
    # t-test
    test   <- "t-test"
    t      <- t.test(x ~ g)
    stat   <- unname(t$statistic)
    p_val  <- t$p.value
  }else{
    test <- "Wilcoxon"
    w <- wilcox_test(x ~ g)
    stat   <- as.numeric(statistic(w))
    p_val  <- as.numeric(pvalue(w))
  }
  tabela <- rbind(tabela, data.frame(
    Varijable = v,
    Shapiro_p_Euro = p_eu,
    Shapiro_p_Asia = p_as,
    Korisceni_test = test,
    p_value = p_val,
    Test_statistic = stat
  ))
}
tabela



# zadatak pod c
ena <- df[df$Continent %in% c("Europe", "North America") & df$affordability<quantile(df$affordability,0.25),]
nrow(ena)
pragovi <- c(0.8, 0.9, 0.95)

sw_p <- function(x) if (length(x) >= 3) shapiro.test(x)$p.value else NA
tab2c <- data.frame()
for (pr in pragovi) {
  s    <- subset(ena, crime < pr)
  x_eu <- s$Tourism[s$Continent == "Europe"]
  x_na <- s$Tourism[s$Continent == "North America"]
  
  p_eu <- sw_p(x_eu)
  p_na <- sw_p(x_na)
  
  if (length(x_eu) < 2 | length(x_na) < 2) {
    test  <- "nema dovoljno podataka"
    p_val <- NA
  } else if (!is.na(p_eu) & !is.na(p_na) & p_eu > 0.05 & p_na > 0.05) {
    test  <- "t-test"
    p_val <- t.test(x_eu, x_na)$p.value
  } else {
    test  <- "Wilcoxon"
    s2    <- droplevels(s)
    p_val <- pvalue(wilcox_test(Tourism ~ Continent, data = s2))
  }
  
  tab2c <- rbind(tab2c, data.frame(
    Crime_prag         = pr,
    n_Europe           = length(x_eu),
    n_NorthAmerica     = length(x_na),
    Shapiro_p_Europe   = round(p_eu, 4),
    Shapiro_p_NorthAm  = round(p_na, 4),
    Korisceni_test     = test,
    p_value            = round(as.numeric(p_val), 4)
  ))
}
tab2c