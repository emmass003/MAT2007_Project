
# Question 1: In which countries is depression most common in 2017?
#
# Introduction to programming (MAT2007) - data analysis project
# (the second question of the project is answered in question2_men_women.R)
#
# What this script does, step by step:
#  1. Tests its own functions on small examples
#  2. Reads the dataset (one CSV file downloaded from Kaggle)
#  3. Takes the table with depression per country out of the file
#  4. Cleans the table (keeps only real countries with a value)
#  5. Calculates the average of all countries, with its uncertainty
#      (sub-sampling method)
#  6. Ranks the countries by their share of people with depression
#  7. Plot: bar chart of the five highest and the five lowest countries
#  8.Saves the ranking of all countries in a CSV file
#
# Every step is done by a function. The functions are written first, and the
# main program at the bottom calls them one after the other.
#
# How to run (from the folder that contains this script and the CSV file):
#   Rscript question1_countries.R
#
# Only base R is used, so no extra packages have to be installed.




# SETTINGS
# Everything you might want to change is collected here.


# Name of the data file. It has to be in the same folder as this script.
data_file <- "Mental health Depression disorder Data.csv"

# The year used for the result. 2017 is the most recent year in the file.
year_to_study <- 2017

# How many countries to show at the top and at the bottom of the ranking
n_to_show <- 5

# In how many sub-samples (groups) the countries are split to calculate the
# uncertainty of an average (see mean_with_uncertainty below)
n_subsamples <- 5

# Name of the column we use, exactly as it is written in the file
depression_column <- "Depression (%)"

# Colours used in the plot
highest_colour <- "firebrick"    # countries with the most depression
lowest_colour  <- "grey60"       # countries with the least depression

# Names of the files this script creates
plot_file    <- "question1_plot_countries.png"
ranking_file <- "question1_ranking.csv"



# FUNCTIONS
# Each function does one small job, so it can be tested on its own.
# The functions use the settings above.


# Reads the CSV file and returns it as a data frame.
# Everything is read as text first, because the file contains several
# different tables stacked under each other (see get_table below), so one
# column can hold both words and numbers.
load_data <- function(file_name) {

  # Condition: stop with a clear message if the file is not in the folder
  if (!file.exists(file_name)) {
    stop("I cannot find the file '", file_name, "'. ",
         "Put it in the same folder as this script and run the script ",
         "from that folder.")
  }

  # Exception handling: if reading goes wrong, explain what happened
  # instead of showing a confusing error.
  #   colClasses = "character" : read every column as text
  #   check.names = FALSE      : keep the column names exactly as in the file
  raw_data <- tryCatch(
    read.csv(file_name, colClasses = "character", check.names = FALSE),
    error = function(e) {
      stop("The file was found but could not be read as a CSV file: ",
           conditionMessage(e))
    }
  )

  # Condition: the file must have the columns that every table shares
  needed_columns  <- c("Entity", "Code", "Year")
  missing_columns <- needed_columns[!(needed_columns %in% names(raw_data))]
  if (length(missing_columns) > 0) {
    stop("These columns are missing from the file: ",
         paste(missing_columns, collapse = ", "))
  }

  return(raw_data)
}


# Takes one table out of the file and returns it with its own column names.
#
# The file holds several tables stacked under each other. Each new table
# starts with a row that repeats the header, so its "Entity" cell contains
# the word "Entity". These header rows tell us where every table starts.
#   table_number = 1 : the first table (uses the header of the file itself)
#   table_number = 2 : the table below the first repeated header, and so on
get_table <- function(raw_data, table_number) {

  # Row numbers of the repeated headers inside the file
  header_rows <- which(raw_data$Entity == "Entity")
  n_tables    <- length(header_rows) + 1

  # Condition: the table we ask for has to exist
  if (table_number < 1 || table_number > n_tables) {
    stop("The file has ", n_tables, " tables, so table ", table_number,
         " does not exist.")
  }

  # First row of every table: row 1, and the row after each repeated header
  first_rows <- c(1, header_rows + 1)
  # Last row of every table: the row before each repeated header, and the
  # last row of the file
  last_rows  <- c(header_rows - 1, nrow(raw_data))

  # Cut out the rows of the table we want
  one_table <- raw_data[first_rows[table_number]:last_rows[table_number], ]

  # Give the table its own column names. For table 1 these are the names of
  # the file. For the other tables they are written in the repeated header.
  if (table_number > 1) {
    names(one_table) <- as.character(raw_data[header_rows[table_number - 1], ])
  }

  # Throw away columns without a name (they are empty in this table)
  one_table <- one_table[, names(one_table) != "", drop = FALSE]

  return(one_table)
}


# Cleans the table with depression for both sexes together (table 1). It
# returns a small, tidy data frame with one row per country and year, and
# prints how many rows are removed in every step.
clean_depression_table <- function(disorder_table) {

  cat("Rows in the depression table:", nrow(disorder_table), "\n")

  # Step 1: keep only the columns we need and turn the text into numbers.
  # The table has a column for several disorders, we only use depression.
  tidy <- data.frame(
    country    = disorder_table$Entity,
    code       = disorder_table$Code,
    year       = as.integer(disorder_table$Year),
    depression = as.numeric(disorder_table[[depression_column]]),
    stringsAsFactors = FALSE
  )

  # Step 2: remove regions and groups of countries.
  # Real countries have a country code (for example "NLD"). Rows without a
  # code are regions such as "Western Europe" or groups such as "High SDI".
  # Keeping them would count the same people twice.
  is_region <- tidy$code == ""
  cat("  removed", sum(is_region), "rows of regions and country groups\n")
  tidy <- tidy[!is_region, ]

  # Step 3: remove the row for the whole world (code "OWID_WRL")
  is_world <- tidy$code == "OWID_WRL"
  cat("  removed", sum(is_world), "rows for the world as a whole\n")
  tidy <- tidy[!is_world, ]

  # Step 4: remove rows with a missing or impossible value.
  # A percentage of a population has to be between 0 and 100.
  is_bad <- is.na(tidy$year) | is.na(tidy$depression) |
            tidy$depression < 0 | tidy$depression > 100
  cat("  removed", sum(is_bad), "rows with missing or impossible values\n")
  tidy <- tidy[!is_bad, ]

  cat("Rows left after cleaning:", nrow(tidy), "\n")
  cat("Countries:", length(unique(tidy$country)),
      "| Years:", min(tidy$year), "to", max(tidy$year), "\n\n")

  return(tidy)
}


# Keeps only the rows of one year, so there is one row per country.
# It stops with a clear message if there is no data for that year.
keep_one_year <- function(data, year) {

  one_year <- data[data$year == year, ]

  # Condition: there has to be data for this year
  if (nrow(one_year) == 0) {
    stop("There is no data for the year ", year, ".")
  }

  return(one_year)
}


# Calculates the average of a list of values together with its statistical
# uncertainty, using the sub-sampling technique from the course:
#   1. split the sample in N sub-samples of (almost) the same size
#   2. calculate the average of every sub-sample
#   3. the average of those sub-sample averages is the result
#   4. the standard deviation of those sub-sample averages is the uncertainty
#
# The idea: if every sub-sample gives almost the same average, the average is
# known precisely. If the sub-samples disagree, it is not.
#
# The values are dealt out like playing cards: value 1 goes to sub-sample 1,
# value 2 to sub-sample 2, ..., and after sub-sample N it starts again at 1.
# No random numbers are used, so the result is the same every time.
mean_with_uncertainty <- function(values, n_groups) {

  # Condition: we need at least 2 sub-samples, and at least 2 values in each
  if (n_groups < 2) {
    stop("At least 2 sub-samples are needed.")
  }
  if (length(values) < 2 * n_groups) {
    stop("There are not enough values for ", n_groups, " sub-samples.")
  }

  # The sub-sample number of every value: 1, 2, ..., N, 1, 2, ..., N, ...
  group <- rep(1:n_groups, length.out = length(values))

  # Loop over the sub-samples and calculate the average of each one
  subsample_means <- numeric(n_groups)       # empty list of N numbers
  for (g in 1:n_groups) {
    subsample_means[g] <- mean(values[group == g])
  }

  # Result: the average of the sub-sample averages, and their standard
  # deviation as the statistical uncertainty
  return(c(mean = mean(subsample_means), uncertainty = sd(subsample_means)))
}


# Ranks the rows of a data frame by one column, from the highest value to the
# lowest, and adds a column "rank" (1 = highest).
#   data : the data frame, one row per country
#   column_name : the name of the column to sort by, for example "depression"
rank_countries <- function(data, column_name) {

  # Condition: the column has to exist
  if (!(column_name %in% names(data))) {
    stop("The column '", column_name, "' does not exist.")
  }

  # order() gives the row numbers in sorted order, highest value first
  sorted      <- data[order(data[[column_name]], decreasing = TRUE), ]
  sorted$rank <- 1:nrow(sorted)

  return(sorted)
}


# Prints the result of the ranking: the average of all countries, the
# countries at the top and at the bottom, and how far apart they are.
#   ranking : the ranked countries (from rank_countries)
#   n_show  : how many countries to print at the top and at the bottom
#   average : the average of all countries and its uncertainty
#                 (from mean_with_uncertainty)
print_ranking <- function(ranking, n_show, average) {

  n_countries <- nrow(ranking)

  # Condition: there must be enough countries to show a top and a bottom
  if (n_countries < 2 * n_show) {
    stop("There are not enough countries to show the ", n_show,
         " highest and the ", n_show, " lowest.")
  }

  # The standard deviation shows how much the countries differ from each other
  spread <- sd(ranking$depression)

  # sprintf() fills the numbers into the text: %.2f means 2 decimals
  cat("Result for", year_to_study, "(", n_countries, "countries )\n")
  cat(sprintf("  uncertainty from %d sub-samples of the %d countries\n",
              n_subsamples, n_countries))
  cat(sprintf("  all countries: mean share with depression = %.2f +- %.2f %%\n",
              average["mean"], average["uncertainty"]))
  cat(sprintf("  all countries: median = %.2f %%, standard deviation = %.2f %%\n",
              median(ranking$depression), spread))

  # The first rows of the ranking are the highest countries
  cat("  highest share of people with depression:\n")
  for (i in 1:n_show) {                            # loop over the top rows
    cat(sprintf("    %3d. %-12s %.2f %%\n", ranking$rank[i],
                ranking$country[i], ranking$depression[i]))
  }

  # The last rows of the ranking are the lowest countries
  cat("  lowest share of people with depression:\n")
  for (i in (n_countries - n_show + 1):n_countries) {   # loop over the bottom rows
    cat(sprintf("    %3d. %-12s %.2f %%\n", ranking$rank[i],
                ranking$country[i], ranking$depression[i]))
  }

  # Compare the highest country with the lowest country
  top    <- ranking[1, ]
  bottom <- ranking[n_countries, ]
  cat(sprintf("  %s has %.1f times the share of %s\n",
              top$country, top$depression / bottom$depression, bottom$country))

  # How far are they from the average, counted in standard deviations?
  cat(sprintf("  %s lies %.1f standard deviations above the mean, %s lies %.1f below it\n\n",
              top$country, (top$depression - average["mean"]) / spread,
              bottom$country, (average["mean"] - bottom$depression) / spread))
}


# Draws the bar chart of the highest and the lowest countries and saves it as
# a picture. One horizontal bar per country: the highest on top, the lowest
# below them. The dashed line is the average of all countries.
#  ranking : the ranked countries (from rank_countries)
#  n_show : how many countries to show at the top and at the bottom
#  average : the average of all countries and its uncertainty
#  file_name : the name of the picture file
plot_countries <- function(ranking, n_show, average, file_name) {

  n_countries <- nrow(ranking)

  # The first rows are the highest countries, the last rows the lowest
  highest_countries <- ranking[1:n_show, ]
  lowest_countries  <- ranking[(n_countries - n_show + 1):n_countries, ]

  # rbind() puts the two small tables under each other
  shown <- rbind(highest_countries, lowest_countries)

  # barplot() draws from the bottom up, so the order is reversed with rev()
  # to get the highest country at the top
  heights <- rev(shown$depression)
  labels  <- rev(shown$country)
  colours <- rev(c(rep(highest_colour, n_show), rep(lowest_colour, n_show)))

  # png() opens a picture file, everything drawn after it goes into that file.
  # width and height are the size in pixels, res is the sharpness.
  png(file_name, width = 1600, height = 1100, res = 200)

  # par(mar = ...) sets the white margins: bottom, left, top, right.
  # The left margin is wide, to make room for the country names.
  par(mar = c(4.5, 7, 4.5, 1))

  # barplot() gives back the position of the middle of every bar, which we
  # need to write the values next to the bars.
  # - horiz = TRUE : horizontal bars
  # - las = 1 : write the names horizontally
  # - xlim : range of the x axis, with room for the values
  bar_middles <- barplot(heights, names.arg = labels,
                         horiz = TRUE, las = 1,
                         col = colours, border = NA,
                         xlim = c(0, max(heights) * 1.25),
                         xlab = "Share of population with depression (%)")

  # Title in two lines: a big one and a smaller one below it that says what
  # exactly is shown. line = ... is the distance above the plot.
  title(main = "Countries with the most and the least depression", line = 2.6)
  mtext(sprintf("The %d highest and the %d lowest of %d countries in %d",
                n_show, n_show, n_countries, year_to_study),
        side = 3, line = 1.2, cex = 0.9)

  # Dashed vertical line at the average of all countries
  # (lty = 2 makes it dashed, lwd = 2 makes it thick)
  abline(v = average["mean"], lty = 2, lwd = 2)

  # Write the value next to every bar (pos = 4: to the right)
  text(heights, bar_middles, labels = sprintf("%.2f %%", heights),
       pos = 4, cex = 0.8)

  # The legend explains the two colours and the dashed line (bty = "n": no box)
  legend("bottomright", bty = "n", cex = 0.8,
         legend = c(paste(n_show, "highest countries"),
                    paste(n_show, "lowest countries"),
                    sprintf("mean of all %d countries (%.2f +- %.2f %%)",
                            n_countries, average["mean"], average["uncertainty"])),
         fill = c(highest_colour, lowest_colour, NA),
         border = NA, lty = c(NA, NA, 2), lwd = c(NA, NA, 2))

  invisible(dev.off())   # close and save the picture
}


# Small tests of our own functions with examples where we know the answer.
# If one of them fails, stopifnot() stops the script with an error.
run_tests <- function() {

  # Test 1: sub-sampling the values 1 to 6 with 2 sub-samples.
  # Sub-sample 1 gets 1, 3, 5 (average 3) and sub-sample 2 gets 2, 4, 6
  # (average 4). The average of 3 and 4 is 3.5 and their standard deviation
  # is 0.7071068.
  result <- mean_with_uncertainty(c(1, 2, 3, 4, 5, 6), 2)
  stopifnot(abs(result["mean"] - 3.5) < 1e-10)
  stopifnot(abs(result["uncertainty"] - 0.7071068) < 1e-6)

  # A tiny fake file with two tables stacked under each other, built in the
  # same way as the real file. The row with "Entity" starts the second table.
  fake <- data.frame(
    Entity = c("Aland", "Some region", "World", "Entity", "Aland"),
    Code   = c("ALA",   "",            "OWID_WRL", "Code", "ALA"),
    Year   = c("2017",  "2017",        "2017",  "Year",  "1800"),
    d      = c("3.5",   "3.0",         "3.2",   "Other table", "7"),
    stringsAsFactors = FALSE
  )
  names(fake)[4] <- depression_column

  # Test 2: table 1 of the fake file has 3 rows, table 2 has 1 row
  stopifnot(nrow(get_table(fake, 1)) == 3)
  stopifnot(nrow(get_table(fake, 2)) == 1)

  # Test 3: cleaning table 1 keeps only the real country and turns the text
  # into a number. capture.output() hides the messages that
  # clean_depression_table prints.
  hidden <- capture.output(cleaned <- clean_depression_table(get_table(fake, 1)))
  stopifnot(nrow(cleaned) == 1)
  stopifnot(cleaned$country == "Aland")
  stopifnot(cleaned$depression == 3.5)

  # Test 4: keep_one_year keeps only the rows of the year we ask for
  two_years <- data.frame(country = c("Aland", "Aland"), year = c(2016, 2017))
  stopifnot(nrow(keep_one_year(two_years, 2017)) == 1)

  # Test 5: ranking puts the highest value first and gives it rank 1
  fake_countries <- data.frame(country = c("Aland", "Bland", "Cland"),
                               depression = c(3.0, 5.0, 2.0))
  ranked <- rank_countries(fake_countries, "depression")
  stopifnot(all(ranked$country == c("Bland", "Aland", "Cland")))
  stopifnot(all(ranked$rank == c(1, 2, 3)))

  cat("All tests passed.\n\n")
}


# MAIN PROGRAM
# The functions above are called here, one step after the other.


# 1. Test the functions before using them 
# If a function has a mistake, the script stops here instead of giving wrong
# results later.
run_tests()

# 2. Read the data 
# raw_data is the whole file as one big table of text
raw_data <- load_data(data_file)
cat("Rows in the file:", nrow(raw_data), "\n")

# 3. Take the table we need out of the file
# Table 1 holds the share of the whole population (men and women together)
# with each disorder, per country and year.
disorder_table <- get_table(raw_data, 1)
cat("  used:", nrow(disorder_table), "rows of table 1\n")
cat("  not used:", nrow(raw_data) - nrow(disorder_table),
    "rows that belong to other tables\n")

# Condition: stop if table 1 does not have the column we need
if (!(depression_column %in% names(disorder_table))) {
  stop("Table 1 of the file does not have the column '", depression_column, "'.")
}

# 4. Clean the table
# depression_all_years has one row per country and year
# (the file has the years 1990 to 2017)
depression_all_years <- clean_depression_table(disorder_table)

# Keep only the year we want to study: one row per country
depression_one_year <- keep_one_year(depression_all_years, year_to_study)

# 5. Average of all countries, with its uncertainty
# The average share with depression over all countries is the number that
# every single country is compared with. Its statistical uncertainty comes
# from the sub-sampling method.
# This is done before the countries are sorted: the sub-samples have to be a
# fair mix of countries, and here they are still in the order of the file
# (alphabetical). mean_with_uncertainty() gives back two numbers, named
# "mean" and "uncertainty".
average_all <- mean_with_uncertainty(depression_one_year$depression, n_subsamples)

# 6. Rank the countries
# Sort the countries from the highest to the lowest share with depression,
# and print the top and the bottom of the ranking
country_ranking <- rank_countries(
  depression_one_year[, c("country", "depression")], "depression")

print_ranking(country_ranking, n_to_show, average_all)

# 7.  Plot: the highest and the lowest countries
plot_countries(country_ranking, n_to_show, average_all, plot_file)

# 8.  Save the results
# write.csv() saves a table as a CSV file. row.names = FALSE leaves out the
# row numbers. The file holds all countries, from the highest to the lowest.
write.csv(country_ranking[, c("rank", "country", "depression")],
          ranking_file, row.names = FALSE)

cat("Done. Created:", plot_file, "and", ranking_file, "\n")
