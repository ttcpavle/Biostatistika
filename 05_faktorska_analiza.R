# ZADATAK 5 - faktorska analiza ================================================
# Dataset: Arcadis.xlsx
library(REdaS)    # bart_spher(), KMO()
library(psych)    # fa()
library(readxl)
set.seed(123)


df <- as.data.frame(read_excel(file.choose(), sheet = 1)) # odmah konvertuj u dataframe
df
df.use <- subset(df, select=-c(City, Continent))
acredis_mat <- as.matrix(df.use)

anyNA(acredis_mat)
nrow(acredis_mat)

bartlett_out <- bart_spher(acredis_mat) # ili cortest.bartlett(cor(acredis_mat), n = nrow(acredis_mat))
print(bartlett_out)
kmo_out <- REdaS::KMO(acredis_mat)      # REdaS:: jer psych takodje ima KMO
print(kmo_out)

bartlett_out$p.value
kmo_out$MSA

# Koliko principalnih komponenti pokriva 70% varijanse?
acredis.pca <- prcomp(acredis_mat, center=TRUE, scale. = TRUE)
summary(acredis.pca)
eigenvalues <- acredis.pca$sdev^2
prop_var <- eigenvalues / sum(eigenvalues)
cum_prop <- cumsum(prop_var)
pca_var_table <- data.frame(
  PC       = 1:length(eigenvalues),
  Eigenval = round(eigenvalues, 3),
  PropVar  = round(prop_var, 3),
  CumProp  = round(cum_prop, 3)
)
print(pca_var_table)
n_fact <- which(cum_prop >= 0.7)[1]  # broj faktora koji objasnjava bar 70%
n_fact



# faktorska opterecenja. Default metoda je minres
fa_res <- fa(acredis_mat, nfactors = n_fact, rotate = "varimax", scores="regression")
print(fa_res)

print(fa_res$loadings, cutoff = 0) # Proveravamo celu matricu a ne samo MR1
L <- unclass(fa_res$loadings) # loadings

# najvece apsolutno opterecenje na faktoru MR1
j <- which.max(abs(L[, "MR1"]))
rownames(L)[j]
L[j, "MR1"]

# promenljiva sa najmanjim komunalitetom i komunalitet od health
fa_res$communality
fa_res$communality[which.min(fa_res$communality)]
fa_res$communality["health"]

# faktor sa najvecom prosecnom komunalnoscu
prosek_komun <- colMeans(L^2)    # prosečni komunalitet po faktoru
prosek_komun
f_best <- names(which.max(prosek_komun))
f_best
max(prosek_komun)

# dve promenljive sa najvecim opterecenjem na tom faktoru
sort(abs(L[, f_best]), decreasing = TRUE)[1:2]

# nadji gradove koji imaju najvece vrednosti za svaki faktor
scores <- as.data.frame(fa_res$scores)   # skorovi: gradovi x faktori
scores$City <- df$City                    # dodaj imena gradova

res <- data.frame()
for (f in colnames(fa_res$scores)) {     # MR1, MR2, ...
  i <- which.max(scores[[f]])            # red sa najvećim skorom na tom faktoru
  res <- rbind(res, data.frame(
    Faktor = f,
    Grad   = scores$City[i],
    Skor   = round(scores[[f]][i], 3)
  ))
}
res

# od ovde zadatak trazi tacno nfactors = 4
fa_res <- fa(acredis_mat, nfactors = 4, rotate = "varimax", scores="regression")

# izdvajanje samo gradova evrope u gornjem kvartilu po turizmu
# na tom skupu izracunati cor() izmedju Tourism i svakog faktora
evropa <- df$Continent == 1
q75 <- quantile(df$Tourism[evropa], 0.75, na.rm=TRUE)
q75

# logicki vektor kao filter
idx <-evropa & df$Tourism >=q75
idx[is.na(idx)] <- FALSE
evropa_turizam_q75 <- df[idx,]

# scores za ove gradove
scores <- as.data.frame(fa_res$scores)
scores_sub <- scores[idx,]
scores_sub

cors <- c()
for(f in colnames(scores_sub)){
  cors[f] <- cor(evropa_turizam_q75$Tourism, scores_sub[[f]], use = "complete.obs")
}
round(cors,4)
names(which.max(abs(cors)))            # faktor sa najvecom |cor|
cors[names(which.max(abs(cors)))]      # i ta vrednost

# podskup health, affordability, Energy, Easeofdoingbusiness, Connectivity
haeec <- subset(df, select=c(health, affordability, Energy, Easeofdoingbusiness, Connectivity, Continent))
colSums(is.na(haeec))
haeec_evropa <- haeec[haeec$Continent == 1, names(haeec) != "Continent"]
haeec_severna_amerika <- haeec[haeec$Continent == 2, names(haeec) != "Continent"]
haeec_azija <- haeec[haeec$Continent == 3, names(haeec) != "Continent"]
# ne mora konverzija u matrice, to je potrebno samo za neke funkcije
nrow(haeec_evropa)
nrow(haeec_severna_amerika)
nrow(haeec_azija)

podskupovi <- list(Evropa = haeec_evropa,
                   SevAmerika = haeec_severna_amerika,
                   Azija = haeec_azija)

rez <- list() # rez cuva dataframe rezultata izracunat za evropu, severnu ameriku i aziju
for (ime in names(podskupovi)) {
  d <- podskupovi[[ime]][, c("health","affordability","Energy","Easeofdoingbusiness","Connectivity")]
  broj_faktora <- sum(eigen(cor(d))$values > 1)
  f <- fa(d, nfactors=broj_faktora, rotate="varimax")
  rez[[ime]] <- data.frame(
    Kontinent = ime,
    n         = nrow(d),
    Bartlett_p = bart_spher(d)$p.value,
    KMO       = REdaS::KMO(d)$MSA,
    n_factors = broj_faktora, # TRUE je u sumi jedanko 1, FALSE=0
    top_communality_variable = names(which.max(f$communality)),
    Comm_value = max(f$communality)
  )
}
tabela <- do.call(rbind, rez)
tabela

# sada analiziramo prosecne skorove podskupa gradova nad pronadjenim faktorima
fa_res <- fa(acredis_mat, nfactors = 4, rotate = "varimax", scores="regression")
print(fa_res)

q75_connectivity <- quantile(df.use$Connectivity, 0.75, na.rm=TRUE)
q25_affordability <- quantile(df.use$affordability, 0.25, na.rm=TRUE)

maska <- df.use$Connectivity>=q75_connectivity & df.use$affordability<=q25_affordability
sum(maska)

scores <- as.data.frame(fa_res$scores)
scores_sub <- scores[maska,]
scores_sub


proseci <- numeric()
for(fk in colnames(scores_sub)){
  proseci[fk] <- mean(scores_sub[[fk]])
}
proseci
izabran <- names(which.max(abs(proseci)))
prosek_izabran <- proseci[izabran]
izabran

temp <- df[maska, c("City", "Continent", "Tourism")]
temp$scores <- scores_sub[,izabran]
temp

if (prosek_izabran > 0) {
  temp <- temp[order(temp$scores, decreasing = TRUE), ]
} else {
  temp <- temp[order(temp$scores, decreasing = FALSE), ]
}
top5 <- head(temp, 5)
top5
