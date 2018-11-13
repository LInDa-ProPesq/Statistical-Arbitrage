iris %>%
  extract(, 1:4) %>%
  head

good.times <-
  Sys.Date() %>%
  as.POSIXct %>%
  seq(by = "15 mins", length.out = 100) %>%
  data.frame(timestamp = .)

good.times$quarter <-
  good.times %>%
  use_series(timestamp) %>%
  format("%M") %>%
  as.numeric %>%
  divide_by_int(15) %>%
  add(1)

iris$Sepal.Length %<>% sqrt
x <- rnorm(100)
x %<>% abs %>% sort
is_weekend <- function(day)
{
  # day could be e.g. character a valid representation
  day %<>% as.Date
  result <- day %>% format("%u") %>% as.numeric %>% is_greater_than(5)
  if (result)
    message(day %>% paste("is a weekend!"))
  else
    message(day %>% paste("is not a weekend!"))
  invisible(result)
}

# Basic use:
iris %>% head
# Use with lhs as first argument
iris %>% head(10)
# Using the dot place-holder
"Ceci n'est pas une pipe" %>% gsub("une", "un", .)
# When dot is nested, lhs is still placed first:
sample(1:10) %>% paste0(LETTERS[.])
# This can be avoided:
rnorm(100) %>% {c(min(.), mean(.), max(.))} %>% floor
# Lambda expressions:
iris %>%
{
  size <- sample(1:10, size = 1)
  rbind(head(., size), tail(., size))
}
# renaming in lambdas:
iris %>%
{
  my_data <- .
  size <- sample(1:10, size = 1)
  rbind(head(my_data, size), tail(my_data, size))
}
# Building unary functions with %>%
trig_fest <- . %>% tan %>% cos %>% sin
%T>% 11 1:10 %>% trig_fest
trig_fest(1:10)

rnorm(200) %>%
  matrix(ncol = 2) %T>%
  plot %>% # plot usually does not return anything.
  colSums
