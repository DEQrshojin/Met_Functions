
someVector = (sample.int(201, size = 100, replace = TRUE) - 101) / 100

centralTendecy = ifelse(mean(someVector) < 0, median(someVector), mean(someVector))

