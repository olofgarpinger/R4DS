library(magrittr)

assign("x", 10)
x

"x" %>% assign(100)
x

env <- environment()
"x" %>% assign(100, envir = env)
x

rnorm(100) %>% 
  matrix(ncol = 2) %>% 
  plot() %>% 
  str()

rnorm(100) %>% 
  matrix(ncol = 2) %T>% 
  plot() %>% 
  str()

mtcars %$%
  cor(disp,mpg)

mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)

mtcars %<>% transform(cyl = cyl * 2) 




