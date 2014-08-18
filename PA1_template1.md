##Reproducible Research
====================================

#Peer Assessment 1

This assignment report consist of five parts

Part 1: Loading and preprocessing the data

```r
a <- read.csv("C:/Users/salahelding/Documents/R/repdata-data-activity/activity.csv")
```

Part2: Mean total number of steps taken per day


```r
y <- numeric()
x <- numeric()
z <- numeric()
a$date <- as.POSIXlt(a$date)

d <- max(a$date) - min(a$date) + 1
a$date <- as.character(a$date)
r <- split(a, a$date)
i <- 1
while (i <= d) {
    
    g <- r[[i]]
    
    y <- rbind(y, sum(g$steps, na.rm = TRUE))
    
    i <- i + 1
}
```
The following is histogram showing the total number of steps taken each day

```r
hist(y)
```

<img src="figure/unnamed-chunk-3.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />
The mean number of steps taken per day


```r
mean(y)
```

```
[1] 9354
```
The median of number of steps taken per day

```r
median(y)
```

```
[1] 10395
```
