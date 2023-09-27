# Read the air data for the year 1999
pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#",
                  header = FALSE, sep = "|", na.strings = "")
# fix the header names
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- make.names(cnames[[1]])

# assign the sample value of pm 2.5
x0 <- pm0$Sample.Value

# Read the air data for the year 2012
pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#",
                  header = FALSE, sep = "|", na.strings = "")

# fix the header names
names(pm1) <- make.names(cnames[[1]])

x1 <- pm1$Sample.Value

# A quick Comparison
summary(x0); summary(x1)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.00    7.20   11.50   13.74   17.90  157.10   13217 
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -10.00    4.00    7.63    9.14   12.00  908.97   73133

# Some boxplots
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))  # some NAs due to log of negative values

## Why are there negative values in x1??
negative <- x1 < 0
str(negative)
sum(negative, na.rm = TRUE)   # 26474 negative values
mean(negative, na.rm = TRUE)  # 2.1% of the whole data 

# Convert the dates column data to Dates class
dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
hist(dates, "month")
hist(dates[negative], "month")

# Exploring change at one monitor
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

# Discovering the subsets for New-York monitor site
head(site0); head(site1)

site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
both <- intersect(site0, site1)

pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

# Split by each monitor
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

# Fix the dates and pm values for the subsets
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
plot(dates1, x1sub)

dates0 <- pm0sub$Date
x0sub <- pm0sub$Sample.Value
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
plot(dates0, x0sub)

# Building a Panel Plot
rng <- range(x0sub, x1sub, na.rm = TRUE)

par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = TRUE))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = TRUE))

# Exploring change at the state level
mean0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

summary(mean0)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.862   9.519  12.315  12.406  15.640  19.956

mean1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))

summary(mean1)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   4.006   7.355   8.729   8.759  10.613  11.992 

data0 <- data.frame(state = names(mean0), mean = mean0)
data1 <- data.frame(state = names(mean1), mean = mean1)
data <- merge(data0, data1, by = "state")
head(data)

# Plotting the results
par(mfrow = c(1,1), mar = c(4,4,2,1))
with(data, plot(rep(1999, 52), data[,2], xlim = c(1998, 2013)))
with(data, points(rep(2012, 52), data[,3]))
segments(rep(1999, 52), data[,2], rep(2012, 52), data[,3])


