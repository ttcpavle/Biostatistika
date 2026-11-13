# ZADATAK 7 - biklasterovanje ==================================================
# Dataset: Food.sav
library(foreign)   # read.spss
library(biclust)
set.seed(123)

df  <- read.spss(file.choose(), to.data.frame = TRUE)
df.use <- subset(df, select = -c(Country))         # numeric features only
food_matrix <- as.matrix(df.use)
# max klastera: 25, delta=1.5, alpha=1, method=BCCC()
set.seed(123)
resBCCC <- biclust(food_matrix, method = BCCC(), delta = 1.5, alpha = 1, number = 25)
summary(resBCCC)

resBCCC@RowxNumber


# helper: za odredjeni bicluseter objekat i dataframe, vratiti vrednosti za promenljivu var
# iz biklastera k
bc_vals <- function(B, X, k, var, require_in = TRUE) {
  if (!inherits(B, "Biclust")) stop("B must be a 'Biclust' object.")
  if (is.null(colnames(X)))   stop("X must have column names.")
  if (!is.character(var) || length(var) != 1L) stop("Provide one column name (character).")
  if (k < 1L || k > B@Number) stop(sprintf("k must be in 1..%d", B@Number))
  j <- match(var, colnames(X)); if (is.na(j)) stop(sprintf("Column '%s' not found in X.", var))
  r <- which(B@RowxNumber[, k, drop = TRUE]); if (!length(r)) stop(sprintf("Bicluster %d has no rows.", k))
  if (require_in && !isTRUE(B@NumberxCol[k, j])) stop(sprintf("Column '%s' is not part of bicluster %d.", var, k))
  X[r, j, drop = TRUE]
}


mean(bc_vals(resBCCC, df.use, 2, "V17"))
max(bc_vals(resBCCC, df.use, 4, "V13"))
median(bc_vals(resBCCC, df.use, 5, "V7"))
min(bc_vals(resBCCC, df.use, 3, "V2"))

# Provera
biclust::bicluster(df.use, resBCCC, number = 1)

# grafik pripadnosti promenljive biklasteru. Proveravamo za V5
biclustmember(resBCCC, df.use)

ord <- bicorder(resBCCC, cols = TRUE, rev = TRUE)
#cols = TRUE -> orders the columns by appearance in the bicluster, else order the rows
biclustmember(resBCCC,
              df.use,
              which = ord,
              mid=TRUE, # if TRUE shows all 3 mean values
              main = "Bicluster membership graph ORDERED")
# u koliko biklastera se pojavljuje svaka promenljiva
resBCCC@NumberxCol
pojavljivanja <-colSums(resBCCC@NumberxCol)
names(pojavljivanja) <- colnames(df.use)
pojavljivanja
pojavljivanja["V5"]
pojavljivanja["V11"]
pojavljivanja[pojavljivanja == max(pojavljivanja)]   # promenljiva(e) sa najvise pojavljivanja


# koji parovi promenljivih se najcesce pojavljuju u istim biklasterima
heatmapBC(x = df.use,
          bicResult = resBCCC,
          number = c(4),
          order    = FALSE, #if FALSE there is no bicluster overlapping
          outside  = TRUE)


M <- resBCCC@NumberxCol * 1            # TRUE/FALSE u 1/0
colnames(M) <- colnames(df.use)
M
parovi <- t(M) %*% M               # matrica zajedničkih pojavljivanja
diag(parovi) <- 0                  # izbaci par promenljive same sa sobom
parovi
max(parovi)
which(parovi==max(parovi), arr.ind=TRUE)

# afinitet i within_sd
df.use.matrix <- as.matrix(df.use)

rez <- data.frame()
for (i in 1:resBCCC@Number) {
  redovi <- which(resBCCC@RowxNumber[, i])
  kolone <- which(resBCCC@NumberxCol[i, ])
  blok   <- df.use.matrix[redovi, kolone, drop = FALSE]
  
  rez <- rbind(rez, data.frame(
    biklaster       = i,
    n_rows    = length(redovi),
    n_cols    = length(kolone),
    afinitet  = mean(blok) / mean(df.use.matrix[, kolone, drop = FALSE]),
    within_sd = sd(as.vector(blok))
  ))
}
rez

# Najveći afinitet
rez[which.max(rez$afinitet), c("biklaster", "afinitet")]

# Filter n_rows >= 6 i n_cols >= 5, pa najmanji within_sd
f <- subset(rez, n_rows >= 6 & n_cols >= 5)
f[which.min(f$within_sd), ]