# start by loading the packages for plotting and data transformation
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# let's look at the diamonds data set that ships with dplyr
diamonds <- data.frame(diamonds)
diamonds

# when we look at the current headings we see that the columns 8, 9 and 10 could have better descriptors
head(diamonds)
# let's rename those headings
names(diamonds)[8] <- "length"
names(diamonds)[9] <- "width"
names(diamonds)[10] <- "depth"

# now we have two columns named depth, woops.
head(diamonds)
# let's rename the first depth column to depth perc
names(diamonds)[5] <- "depthperc"

# Now we want a calculated column.  In Excel you generally operate on cells adjacent to one another and add a columns for the new result.  In R we're going to do this using the mutate function.  To find out more about this function you can run ?mutate

# In this example we'll create a new variable called cubic by passing the diamonds data set to the mutate function and passing the expression length * width * depth into the second argument.
diamonds <- mutate(diamonds, cubic = length * width * depth)
# Let's add another column for total volumn of the diamond where we add all the dimensions together
diamonds <- mutate(diamonds, total = length + width + depth)

# Now let's say we want to summarize the numeric data of our variables.  In Excel you'd go to the bottom of the columns and type in something =SUM(A1:A53609).  In R we'll use various column functions.  To return the average of a column we'll use colMeans(diamonds[,c(1, 5:11)])
colMeans(diamonds[,c(1, 5:12)])
# The expresion above calculates the average for column 1 as well as 5:12.
# Let's round carat to the closest .25
diamonds$carat2 <- round(diamonds$carat / .25) * .25

# Now let's summarize
Summary <- aggregate(cbind(depthperc, table, price, length, width, depth, cubic, total) ~ cut + color + clarity + carat2, data = diamonds, mean)
# The expression above is telling R to take the mean of all the numeric fields(in the cbind function) and group them by the non-numeric fields then assign those records to a new data fram we call Summary.

# Now we'll do something very similar to Excel's Pivot Table functionality.  Imagine comparing the price across different clarity values for each color.  You would do this by putting color on the row self, clarity on the column self and price in the values.  In R we'll use the reshape2 package
install.packages("reshape2")
library(reshape2)
# Then we use dcast to get our data into the same pivot table format
pivot_table <- dcast(diamonds[, c('color', 'clarity', 'price')], color~clarity, mean)
# Let's try another example where we look at average prive across cuts by clarity.
pivot_table2 <- dcast(diamonds[, c('clarity', 'cut', 'price')], clarity~cut, mean)

# Now for the function we've all be waiting for, VLOOKUPS.  In Excel it's extremely common to return the value from a lookup or helper table into your working table.  For example, say I need the freight rate for a give route.  Well, we may need to go lookup the freight rate for that route from our freight rate table.

# So how are we going to do VLOOKUPs in R?  In R we'll use the merge function.

# Our example is going to be calculating a diamonds price above or below the average price for that diamonds cut + color + clarity + carat.  Our working table will be diamonds and our lookup table will be the Summary table we created earlier.

# Let's start by renaming price color in our Summary table to avgprice.
names(Summary)[7] <- "avgprice"
# Next we'll merge the data sets and bring over average price
diamonds <- merge(diamonds, Summary[,c(1:4, 7)], by.x = c("cut", "color", "clarity", "carat2"), by.y = c("cut", "color", "clarity", "carat2"))
# The above expression merged the two data frame together using just the need columns bringing us avgprice(column 7 from Summary) to the diamonds data frame.

# Now let's calculate the perc over/under each diamond is compared to it's average price for its respective group.
library(dplyr)
diamonds <- mutate(diamonds, overunderperc = price / avgprice)

# Now let's think about creating some conditional statements.  In Excel you can perfom all sorts of conditional statemnets using IF, SUMIF, CHOOSE and many more.  In R we'll perfom our conditionals directly on the vector and assign a result to a new column.

# Let's do example where we assign diamonds a size class of Small, Medium or Large based on the carat weight of that diamond.
diamonds$size[diamonds$carat < 0.5] <- "Small"
diamonds$size[diamonds$carat >=0.5 & diamonds$carat < 1] <- "Medium"
diamonds$size[diamonds$carat >= 1] <- "Large"

# Now we move on to Graphing

# In Excel you hunt around to for the right menu item to click or drag the propper area of a pivot chart.  In R you'll be typing out what you want to visualize.  You'll get the hang of it sooner than you think, promise.

# Bar Chart
barplot(table(diamonds$size), main = "Diamond Size Distribution", xlab = "Size Category", ylab = "Number of Diamonds", col = "grey")

# Line Chart
library(ggplot2)
ggplot(diamonds, aes(clarity)) +
  geom_freqpoly(aes(group = color, colour = color)) +
  labs(x = "Clarity", y = "Number of Diamonds", title = "Clarity by Color")

# Scatter Plot
ggplot(diamonds, aes(carat, price, color = clarity)) +
  geom_point() +
  labs(x = "Carat Weight", y = "Price", title = "Price by Carat Weight")

qplot(carat, price, data = daimonds, color = clarity, xlab = "Carat Weight", ylab = "Price", main = "Price by Carat Weight")

# to create othe charts with ggplot visit http://docs.ggplot2.org/current/
