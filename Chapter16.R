library(tidyverse)

typeof(letters)

x <- list("a", "b", 1:10)
length(x)

typeof(1)
typeof(1L)
1.5L

x <- sqrt(2)^2
x
x - 2


# Exercises p. 296
x <- c(0, NA, NaN, Inf, -Inf)
is.finite(x)
!is.infinite(x)

dplyr::near

typeof(parse_logical("1"))
typeof(parse_integer("1"))
typeof(parse_number("1"))

typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

1:10 + 1:2
1:10 + 1:3

c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

x = c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1,1,5,5,5,2)]
x[c(-1,-3,-5)]

x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

x <- list(1, 2, 3)
x
str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a
str(a[1:2])
str(a[4])

str(y[[1]])
str(y[[4]])

a$a
a[["a"]]



