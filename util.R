# Function for converting binary strings into
# corresponding numeric binary vectors
bitter <- function(x) {
  y <- numeric(nchar(x))
  for (i in 1:nchar(x)) {
    if (
      (substr(x, i, i) != "1") &
      (substr(x, i, i) != "0")
    ) {
      y[i] <- 0
    } else {
      y[i] <- as.numeric(substr(x, i, i))
    }
  }
  return(y)
}

# Function for finding maximum run length of
# binary sequence
argmax <- function(x) {
  a <- c(x[1], x)
  b <- c(x, x[length(x)])
  k <- abs(a - b)
  k[1] <- 1
  k <- k[c(rep(TRUE, length(k) - 1), FALSE)]
  j <- 1
  v <- 0
  for (i in 1:length(k)) {
    if (k[i] == 0) {
      v <- c(v, 0)
    }
    if (k[i] == 1) {
      while ((k[i] - k[i + j] != 0) & (i + j <= length(k))) {
        j <- j + 1
      }
      v <- c(v, j)
      j <- 1
    }
  }
  return(max(v))
}

# Function for finding number of runs of
# binary sequence
argrun <- function(x) {
  a <- c(x[1], x)
  b <- c(x, x[length(x)])
  return(sum(abs(a - b)) + 1)
}

# Primal probability vector generator
probvec <- function(n, p) {
  a <- numeric(n + 1)
  for (i in 0:n) {
    a[i + 1] <- (p^i) * ((1 - p)^(n - i))
  }
  return(a)
}

# Functions for max run length combinatorics
# vector generator
comat <- function(n, h, m, z) {
  z[n + 1, h + 1] <- 0
  if ((n >= 1) & (h >= 0)) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] + z[n, h + 1]
  }
  if ((n >= 1) & (h >= 1)) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] + z[n, h]
  }
  if ((n >= 2 + m) & (h >= 1)) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] - z[n - m - 1, h]
  }
  if ((n >= 2 + 2 * m) & (h >= 1 + m)) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] + z[n - 2 * m - 1, h - m]
  }
  if ((n >= 2 + m) & (h >= 1 + m)) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] - z[n - m - 1, h - m]
  }
  if (((h == 0) & (n == 0)) | ((h == m + 1) & (n == 2 * m + 2))) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] + 1
  }
  if (((h == 0) & (n == m + 1)) | ((h == m + 1) & (n == m + 1))) {
    z[n + 1, h + 1] <- z[n + 1, h + 1] - 1
  }
  return(z)
}

filler <- function(n, m) {
  z <- matrix(numeric((n + 1)^2), n + 1, n + 1)
  for (i in 0:n) {
    for (j in 0:i) {
      z <- comat(i, j, m, z)
    }
  }
  return(z)
}

# CDF and p-value for maximum run length
maxrunpdf <- function(n, p) {
  a <- numeric(n)
  for (i in 1:n) {
    a[i] <- sum(filler(n, i)[n + 1, ] * probvec(n, p))
  }
  return(a - c(0, a[-n]))
}

maxrunpval <- function(n, p, k) {
  a <- maxrunpdf(n, p)
  b <- 0
  for (i in 1:n) {
    if (a[i] <= a[k]) {
      b <- b + a[i]
    }
  }
  return(b)
}

# Functions for run number combinatorics
# vector generator
comat2 <- function(n, h, r) {
  if ((n == h) & (r == 1)) {
    return(1)
  }
  if ((h == n) | (h == 0)) {
    return(0)
  }
  if (r %% 2 == 1) {
    return(choose(h - 1, (r - 1) / 2) * choose(n - h - 1, (r - 3) / 2))
  }
  if (r %% 2 == 0) {
    return(choose(h - 1, (r - 2) / 2) * choose(n - h - 1, (r - 2) / 2))
  }
}

filler2 <- function(n, r) {
  a <- numeric(n + 1)
  for (j in 0:n) {
    a[j + 1] <- comat2(n, j, r)
  }
  return(a + rev(a))
}

# pdf and p-value for number of runs
numrunpdf <- function(n, p) {
  a <- numeric(n)
  for (i in 1:n) {
    a[i] <- sum(filler2(n, i) * probvec(n, p))
  }
  return(a)
}

numrunpval <- function(n, p, k) {
  a <- numrunpdf(n, p)
  b <- 0
  for (i in 1:n) {
    if (a[i] <= a[k]) {
      b <- b + a[i]
    }
  }
  return(b)
}

# P-vector generator
pvecgen <- function(x, p) {
  n <- length(x)
  o <- hadamard(n) %*% x
  z <- abs(
    (o - c(n * p, rep(0, n - 1)))
    /
      (sqrt(n * p * (1 - p)))
  )
  k <- numeric(n)
  for (i in 1:length(z)) {
    k[i] <- 2 * (1 - pnorm(z[i]))
  }
  return(k)
}

# Measure of uniformity
uniftest <- function(k) {
  v <- seq(1 / (length(k)), 1, 1 / (length(k)))
  s <- sort(k)
  return(2 * abs(1 - (s %*% v) / (s %*% s)))
}

# Uniformity test plot
unifplot <- function(k) {
  v <- seq(1 / (length(k)), 1, 1 / (length(k)))
  s <- sort(k)
  plot(sort(k), v,
       main = "Kolmogorov-Smirnov plot for WH p-vector", xlab = "Ordered p-vector",
       ylab = "Quantiles", asp = 1
  )
  abline(a = 0, b = 1)
  abline(a = 0, b = (s %*% v) / (s %*% s), col = "red")
}

# P-value generator for
# Bernoulli test
berpval <- function(x, p) {
  n <- length(x)
  m <- sum(x)
  b <- 0
  for (i in 0:n) {
    if (dbinom(i, n, p) <= dbinom(m, n, p)) {
      b <- b + dbinom(i, n, p)
    }
  }
  return(b)
}

# Last equalisation test
# and p-value generator
lasteqpdf <- function(n, p) {
  a <- numeric((n / 2) + 1)
  for (j in 0:(n / 2)) {
    for (i in j:(n / 2)) {
      a[j + 1] <- a[j + 1] + (choose(2 * j, j) * choose(2 * (i - j), i - j)) * (1 / (1 - 2 * (i - j))) * (p * (1 - p))^i
    }
  }
  return(a)
}

lasteqstat <- function(x) {
  n <- length(x)
  a <- cumsum(2 * x - 1)
  for (i in 1:n) {
    if (a[i] == 0) {
      b <- i
    }
  }
  if (length(a[a == 0]) == 0) {
    b <- 0
  }
  return(b)
}

lasteqpval <- function(n, s, p) {
  b <- 0
  a <- lasteqpdf(n, p)
  for (i in 0:(n / 2)) {
    if (a[i + 1] <= a[(s / 2) + 1]) {
      b <- b + a[i + 1]
    }
  }
  return(b)
}

# Multinomial(2) test code
# and p-value generator
roveck <- function(x, k) {
  t <- length(x)
  a <- numeric(t / k)
  b <- numeric(2^k)
  for (n in 1:(t / k)) {
    s <- x[(k * (n - 1) + 1):(k * n)]
    a[n] <- Reduce(function(x, y) x * 2 + y, s)
  }
  for (i in 1:(2^k)) {
    b[i] <- length(a[a == (i - 1)])
  }
  return(b)
}

multipval <- function(n, p, k) {
  v <- probvec(3, p)
  x <- dmultinom(k, n / 2, v)
  b <- 0
  for (i in 0:(n / 2)) {
    for (j in 0:((n / 2) - i)) {
      for (l in 0:((n / 2) - i - j)) {
        if (dmultinom(c(i, j, l, (n / 2) - (i + j + l)), n / 2, v) <= x) {
          b <- b + dmultinom(c(i, j, l, (n / 2) - (i + j + l)), n / 2, v)
        }
      }
    }
  }
  return(b)
}