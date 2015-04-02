library(lattice)
library(ggplot2)

data(Chem97, package = "mlmRev")
summary(Chem97)
str(Chem97)

pl <- histogram(~gcsescore | factor(score), data = Chem97)
print(pl)
pg <- ggplot(Chem97, aes(gcsescore)) + geom_histogram(binwidth = 0.2) + facet_wrap(~score)
print(pg)

pl <- densityplot(~gcsescore | factor(score), data = Chem97, plot.points = FALSE, ref = TRUE)
print(pl)
pg <- ggplot(Chem97, aes(gcsescore)) + stat_density(geom = "path", position = "identity") + facet_wrap(~score)
print(pg)

pl <- densityplot(~gcsescore, data = Chem97, groups = score, plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))
print(pl)
pg <- ggplot(Chem97, aes(gcsescore)) + stat_density(geom = "path", position = "identity", aes(colour = factor(score)))
print(pg)


data(Oats, package = "MEMSS")

tp1.oats <- xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
print(tp1.oats)
gp.oats <- ggplot(Oats, aes(nitro, yield)) + geom_point() + geom_line() + facet_wrap(~Block + Variety, ncol = 3)
print(gp)

print(tp1.oats[, 1])
pg <- pg.oats %+% subset(Oats, Block == "I")
print(pg)

pl <- update(tp1.oats, aspect = "xy")
print(pl)
pg <- pg.oats + opts(panel.margin = unit(0, "lines"))


## Stopped because the versoin author is using is old
d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) +
  xlim(0, 2) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)

d + facet_wrap(~ color)

d + facet_wrap(~ color, ncol = 1)
d + facet_wrap(~ color + cut)

## To change plot order of facet wrap,
## change the order of varible levels with factor()
diamonds$color <- factor(diamonds$color, levels = c("G", "J", "D", "E", "I", "F", "H"))
# Repeat first example with new order
d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) +
xlim(0, 2) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ color)

# You can choose to keep the scales constant across all panels
# or vary the x scale, the y scale or both:
p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 60)
p + facet_wrap(~ color)
p + facet_wrap(~ color, scales = "free_y")

p <- qplot(displ, hwy, data = mpg)
p + facet_wrap(~ cyl)
p + facet_wrap(~ cyl, scales = "free")

# Add data that does not contain all levels of the faceting variables
cyl6 <- subset(mpg, cyl == 6)
p + geom_point(data = cyl6, colour = "red", size = 1) +
  facet_wrap(~ cyl)

p + geom_point(data = transform(cyl6, cyl = 7), colour = "red") +
  facet_wrap(~ cyl)

p + geom_point(data = transform(cyl6, cyl = NULL), colour = "red") +
  facet_wrap(~ cyl)

## PF 3
rm(flights)
library(nycflights13)
flights <- as.data.table(flights)
str(flights)


## description of the dataset
##  AA      American Airlines Inc.
##  DL        Delta Air Lines Inc.
##  UA       United Air Lines Inc.

## airports abbreviatins
##  EWR Newark Liberty Intl 40.69250 -74.16867  18 -5   A
##  JFK John F Kennedy Intl 40.63975 -73.77893  13 -5   A
##  LGA          La Guardia 40.77725 -73.87261  22 -5   A

## variable of interests
## correlation between dep_delay and arr_delay

ggplot(flights[carrier %in% c('AA', 'DL', 'UA')], aes(arr_delay)) + geom_histogram()

with(na.omit(flights), cor(dep_delay, arr_delay))
with(na.omit(flights), cor(dep_time, arr_delay))

cor(na.omit(flights[, list(month, day, dep_time, dep_delay, arr_delay, air_time, distance)]))

## dep_time is somewhat correlated to arr_delay

## put dep_time into several buckets
ggplot(flights[carrier %in% c('AA', 'DL', 'UA')], aes(dep_time)) + geom_histogram(binwidth = 60) + facet_wrap(~carrier, ncol = 2)

ggplot(flights[carrier %in% c('AA', 'DL', 'UA')], aes(dep_time)) + geom_histogram(binwidth = 60) + facet_wrap(~origin, ncol = 1)

range(flights$dep_time, na.rm = TRUE)
## table(cut(flights$dep_time, breaks = c(0, 700, 1200, 1700, 2401)))
## records with unknown dep_time has no arr_delay time

flights[, time.segment := cut(dep_time, breaks = c(0, 600, 1200, 1700, 2100, 2401), labels = c('late-night', 'morning', 'afternoon', 'evening', 'night'))]
flights[time.segment == 'late-night', time.segment := 'night']

flights[, .N, by = time.segment]

flights[is.na(time.segment), time.segment := 'Unknown']

flights[, .N, by = time.segment]

flights[carrier %in% c('AA','DL','UA'), .N, by = time.segment]

flights.target <- flights[carrier %in% c('AA', 'DL', 'UA')]

flights.target[, .N, by = time.segment]

flights.target

flights.target[, ':='(delay.mean = mean(arr_delay, na.rm=TRUE),
                      delay.median = median(arr_delay, na.rm=TRUE))
               , by = list(carrier, origin, month, time.segment)]


flights.target[, .N, by = time.segment]

flights.target[, mean(arr_delay, na.rm=TRUE), by = time.segment]

unique(flights[carrier %in% c('AA', 'DL', 'UA'), origin])
unique(flights[carrier %in% c('AA', 'DL', 'UA'), dest])

flights.target[, carrier := ordered(carrier, levels = c('AA','DL','UA'), labels = c("American Airline", "Delta Airline", "United Airline"))]

#flights.target <- na.omit(flights.target)

ggplot(data = flights.target, aes(x = ordered(month), y = delay.mean)) + geom_bar(stat = "identity") + facet_wrap( ~carrier + origin) + xlab("Month") + ylab("Avg arrival delay")

ggplot(data = flights.target, aes(x = ordered(month), y = delay.median, fill = carrier)) + geom_bar(stat = "identity", position = "dodge") + facet_wrap( ~origin + time.segment, ncol = 5) + xlab("Month") + ylab("Avg arrival delay") + ggtitle("Avg arrival dealy in each month parralleled by Carrier and departure airport in NYC")


