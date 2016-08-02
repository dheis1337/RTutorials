library(Quandl)
library(TTR)
library(ggplot2)
library(dplyr)
library(data.table)


data1 <- Quandl("ECB/EURUSD", order = "asc")

data2 <- BBands(data1$Value, n = 30, sd = 2.5)
data2 <- data.frame(cbind(data1, data2))

# Convert data frame to data.table
EURUSD <- as.data.table(data2)


# Row subsetting



# Subset data.table by a certain row index
EURUSD[1:19, ]

# Subset values in rows by logical condition. Both of these return the same value. The 
# Comma separator is optional in this subsetting in data.table
EURUSD[Value < dn,]
EURUSD[Value < dn]

# Subset values in rows by multiple logical conditions.
EURUSD[Date >= "2008-01-01" & Date < "2009-01-01",]

# Subset values and order them. This application isn't too helpful, but just to demonstrate.
EURUSD[order(Value, decreasing = TRUE)]

# Ordering by the first arugment, then breaking ties by the second argument. This also orders it
# by the first argument, THEN by the second argument.
EURUSD[order(Value, dn, decreasing = TRUE)]

# Order all rows in a specific column that meet either (of multiple) conditions
diamonds[cut %in% c("Ideal", "Premium")]




# Subsetting by columns



# Subset data.table by column and return a vector. This is great for computation
EURUSD[ , Value]
mean(EURUSD[ , Value])

# Subset data.table by column and return a data.table. Either one or multiple columns can be 
# indexed at a time. This is great for creating new data.tables
EURUSD[, .(Value)]
EURUSD[, .(Date, Value)]

# Calculations in j (the columns)
diamonds[, mean(depth)]

# Calculations on multiple columns
diamonds[, .(mean(x),
             mean(y),
             mean(z))]

# Calculating some columns and gather others
diamonds[, .(cut, 
             mean(depth),
             mean(price))]



# Group by subsetting - joins BY group



# Creates diamonds dataset as a data.table
diamonds <- as.data.table(diamonds)

# Calculates the mean PRICE of diamonds grouped by CLARITY. This creates a new data.table. 
# The second expression just names the calculation
diamonds[, mean(price), by = clarity]
diamonds[, .( Mean.Price = mean(price)), by = clarity]

# Create another new column (i.e. calculate another thing and add it to the new data.table)
diamonds[, .(Mean.Price = mean(price),
             Min.Price = min(price),
             Max.Price = max(price)),
                by = clarity]
# Calculate a measure and group by multiple columns in your data.table
diamonds[, .(Mean.Price = mean(price)), 
             by = c("clarity", "cut")]

# Creating new names for group by column
diamonds[, .(Mean.price = mean(price)),
             by = .(Cut.Groups = cut)]

# Grouping on only a certain number of rows by specifying i
diamonds[1:3, .(Mean.price = mean(price)),
         by = .(Cut.Groups = cut)]

# Use .N to get total number of observations for each group
diamonds[, .N, by = clarity]
diamonds[, .N, by = cut]



# Altering data.table with the := in j. This will automatically change the data.table!!! 
# Be careful when using := method




# Creating a new column with := 
diamonds[, round.x := round(x)]

# Updating an existing column using := 
diamonds[, x := round(x)]

# Creating two new columns in a data.table using :=
# The following syntax can be used to update existing columns by naming an already
# existing column in the data.table as in the example above
diamonds[, c("round.x", "round.y") := round(x), round(y)]
data(diamonds)
diamonds <- as.data.table(diamonds)

# You can accomplish the same thing as above using the following format
diamonds[, ':=' (x = round(x))]

# Removing a column with := 
diamonds[, x := NULL]

# Remvoing multiple columns with above syntax
diamonds[, c("x", "y") := NULL]

# Deleting multiple columns in data.table by assigning a variable with desired columns to delete
# first and then passing it to the data.table syntax with :=
col.names <- c("clarity", "color", "cut")
diamonds[, (col.names) := NULL]


# Setting and using keys 


# Set a key for EURUSD 
setkey(diamonds, cut)

# Set multiple column keys. These joins are called equi-joins
setkey(diamonds, cut, clarity)

# Return rows where the first criteria is met, and then the second one
diamonds[.("Ideal", "SI2")]

# Return rows with the first critera, then subset those rows by either of the two criteria in c()
diamonds[.("Ideal",c("SI2","VS1"))]
  
# Reference by key with just ""s
diamonds["Ideal"]
diamonds[c("Ideal", "Premium")]

# Use the 'mult' argument in j when referncing by keys to determine which row to return
diamonds["Ideal", mult = "first"]
diamonds["Ideal", mult = "last"]

# Use the 'nomatch' argument to determine what should be returned if a subset reference isn't
# found. 
diamonds["Huge"]
diamonds[c("Huge", "Ideal"), nomatch = 0]

# Use the 'by = EACHI' argument to group a function's result in j by the known groups in i
diamonds["Ideal", sum(price)] # Returns the sum of the whole price column 
diamonds[c("Ideal", "Premium"), sum(price), by = .EACHI] # Returns the sum of the prices for each cut (i.e each i subset)




# Advanced data.table operations


# Return the 2nd to last row or 284 from last row. This uses .N in i. .N stores the last row in the
# data.table
diamonds[.N-1]
diamonds[.N-283]

# Return the number rows in the data.table. Uses .N in j
diamonds[, .N]

# Use .SD in j to return all columns/data except for column specified in by
diamonds[, print(.SD), by = cut]

# Select first and last rows of each group in by (clarity)
diamonds[, .SD[c(1, .N)], by = clarity]



# Chaining statements together. Chaining allows you to group subsets. Think of it as calling the 
# first subset, then calling a new subset on the data.table that was returned by the proceeding subset
diamonds["Ideal"][, .(Mean = mean(price)), by = color][Mean > 4000]



# set() family of functions



# Use setnames() to update column names. set(data.table, "old", "new")
setnames(diamonds, "cut", "Cut")
setnames(diamonds, c("Cut", "price"), c("cut", "Price"))

# Use setcolorder to change the order of the columns
setcolorder(diamonds, c("cut", "color", "clarity", "carat", "Price", "table", "depth", "x", "y", "z"))
  
