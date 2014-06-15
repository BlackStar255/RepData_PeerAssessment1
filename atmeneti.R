activity <- read.csv("activity.csv")
x <- lapply(split(activity$steps,activity$date), mean)
steps <- data.frame(Date = NA, Total.Steps = NA)
for (i in 1:(length(x)-1)) {
        steps[i,1] <- names(x)[i]
        steps[i,2] <- x[[i]]
}

