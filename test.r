x <- rnorm(100, mean = 10, sd = 1)
y <- runif(100, min = -1, max = 1)
data <- data.frame(x, y)
readr::write_csv(mtcars, "mtcars.csv", append = TRUE)

for(name in names(iris)){
    print(paste("this is a column", name, sep = " "))
}

mean(iris$Sepal.Length, na.rm = FALSE)