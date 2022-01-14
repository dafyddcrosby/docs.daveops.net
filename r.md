# "R"

## Syntax cheatsheet

```r
# create a vector
things <- c('foo', 'bar', 'baz')
```

## plotting

### bar chart

```r
png(file="output.png")
barplot(vector,names.arg=vector_of_names, xlab="groups", ylab="frequency")
dev.off()
```

## import a CSV

```r
dat <- read.csv(file="foobar.csv", header=TRUE, sep=",")
```

## Links

https://www.r-project.org/

