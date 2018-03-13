library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
median(df$a)
median(df$b)
median(df$c)
median(df$d)

output <- vector("double", ncol(df))
for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}
output

y <- vector("double", 0)
seq_along(y)
1:length(y)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df

means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100,1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))


# Exercises p. 321
x = 1:8
for (nm in names(x)) {
  print(nm)
}

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels(c("auto", "manual")))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)
df %>% map_dbl(mean)

rm(mtcars)
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))
  
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>%
  map(summary) %>% 
  map_dbl(~.$r.squared)

models %>%
  map(summary) %>% 
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)


# Exercises p. 328
mtcars %>% 
  map_dbl(mean)

nycflights13::flights %>% 
  map(class)

nycflights13::flights %>% 
  map_chr(typeof)

iris %>% 
  map(unique) %>% 
  map_int(length)

c(-10, 0, 10, 100) %>% 
  map(~rnorm(10, mean = .))

iris %>% 
  map_lgl(is.factor)

map(list(1:5), runif)
map(1:5, runif)

map(-2:2, rnorm, n = 5)
map_dbl(-2:2, rnorm, n = 5)


safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str











