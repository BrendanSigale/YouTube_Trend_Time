library(readxl)
library(tidyverse)

df <- read_excel( # Command to read Excel file
  "Working Book.xlsx", # File name
  sheet = 3) # Sheet by its number

l <- list()

i <- 1
j <- 1
while(j < 44) {
  tmpdf <- subset(df, category_id ==j)
  if(nrow(tmpdf) > 0){
    reg <- step(lm(days_spent_trending ~ ., data=tmpdf), direction = "backward")
    l[[i]] <- reg
    i <- i+1
    j <- j+1
  }
  else {
    j <- j+1
  }
}

lreg <- lapply(l, '[', c('coefficients'))
lres <- lapply(l, '[', c('residuals'))

dfreg <- data.frame()

n <- 1
m <- 1

while(n <= length(lreg[n])) {
  while (m < length(lreg[[c(n, 1, m)]])) {
      dfreg[n,m] <- lreg[[c(n, 1, m)]]
      m <- m + 1
  }
  m <- 1
  n <- n+1
}
new.lreg <- unlist(lreg, recursive = TRUE)

dfres <- data.frame()

m <- 1
n <- 1

while(n <= length(lres[n])) {
  while (m < length(lres[[c(n,1)]])) {
    dfres[m,n] <- lres[[c(n, 1, m)]]
    m <- m + 1
  }
  m <- 1
  n <- n+1
}

new.lres <- unlist(lres, recursive=FALSE)


dfres <- data.frame(matrix(unlist(new.lres), nrow=length(new.lres), byrow=T))

dfres <- t(dfres)
