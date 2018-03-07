df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
# df$a <- (df$a - min(df$a, na.rm = T)) /
#   (max(df$a, na.rm = TRUE) - min(df$a, na.rm = T))

x <- df$a
(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))

rng <- range(x, na.rm = T)
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = T)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))

df %>% 
  dplyr::mutate(a = rescale01(a),
                b = rescale01(b),
                c = rescale01(c),
                d = rescale01(d))


# Exercises p. 273
both_na <- function(u, v) {
  sum(is.na(u) & is.na(v))
}


calculate <- function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unknown op!")
         )
}
calculate(3,2,1)
calculate(3,2,2)
calculate(3,2,3)
calculate(3,2,4)
calculate(3,2,5)


# Exercises p. 279
greet <- function(x) {
  require(lubridate)
  require(dplyr)
  if (between(hour(x), 6, 11)) {
    return("Good morning!")
  } else if (between(hour(x), 12, 17)) {
    return("Good afternoon!")
  } else if (between(hour(x), 18, 22)) {
    return("Good evening!")
  } else {
    return("Good night!")
  }
}


commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output", "!")

x <- c(1, 2)
sum(x, na.mr = TRUE)


# Exercises p. 285
rule("Title", pad = "-+")
rule2 <- function(..., pad = "-") {
  title <- paste0(...)
  width <- ceiling((getOption("width") - nchar(title) - 5)/nchar(pad))
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}


show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

x <- show_missings(mtcars)
class(x)
dim(x)

test <- mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings()
test

`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
rm(`+`)


