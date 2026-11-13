# ZADATAK 3 - testiranje hipoteza ==============================================
# Dataset: Arcadis.xlsx
library(dunn.test)
library(readxl)
set.seed(123)
df <- as.data.frame(read_excel(file.choose(), sheet = 1))


df$Continent <- factor(df$Continent, levels = c(1, 2, 3),
                       labels = c("Europe", "North America", "Asia"))

# prvi deo: Connectivity po kontinentima
sh <- c()
for (k in levels(df$Continent)) {
  s <- shapiro.test(df$Connectivity[df$Continent == k])
  sh[k] <- s$p.value
  cat(k, ": W =", round(s$statistic, 4), ", p =", round(s$p.value, 4), "\n")
}

# izbor testa u zavisnosti od Shapiro rezultata
if (all(sh > 0.05)) {
  a1 <- aov(Connectivity ~ Continent, data = df)
  print(summary(a1))
  print(TukeyHSD(a1))
} else {
  print(kruskal.test(Connectivity ~ Continent, data = df))
  dunn.test(df$Connectivity, df$Continent, method = "bonferroni", kw = TRUE)
  #pairwise.wilcox.test(df$Connectivity, df$Continent, p.adjust.method = "bonferroni")
}

# drugi deo: Tourism po kontinentima, samo gradovi sa Greenspace > medijana
med <-median(df$Greenspace)
podskup <- df[df$Greenspace > med,]
sh2 <- c()
for (k in levels(podskup$Continent)) {
  p <- shapiro.test(podskup$Tourism[podskup$Continent == k])$p.value
  sh2[k] <- p
  cat(k, ": p =", round(p, 4), "\n")
}

if (all(sh2 > 0.05)) {
  # sve grupe normalne -> ANOVA + Tukey
  a <- aov(Tourism ~ Continent, data = podskup)
  print(summary(a))
  print(TukeyHSD(a))
} else {
  print(kruskal.test(Tourism ~ Continent, data = podskup))
  dunn.test(podskup$Tourism, podskup$Continent, method = "bonferroni", kw = TRUE)
}