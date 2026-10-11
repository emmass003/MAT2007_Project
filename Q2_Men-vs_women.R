# Question 2: Are men or women more likely to have depression in 2017?

# Introduction to programming (MAT2007) - data analysis project

# What this script does, step by step:
#   1. Tests its own functions on small examples
#   2. Reads the dataset (one CSV file downloaded from Kaggle)
#   3. Takes the two tables we need out of the file
#   4. Checks that the men/women table really is about depression
#   5. Cleans the men/women table (keeps only real countries with a value)
#   6. Describes the data for one year (mean, median, standard deviation)
#   7. Plot 1: histograms of depression in men and in women
#   8. Plot 2: scatter plot, men against women, one dot per country
#   9. Calculates the result: the difference between women and men, with its
#      uncertainty (sub-sampling method)
#   10. Plot 3: bar chart of the average for men and for women, with error bars

# Every step is done by a function. The functions are written first, and the
# main program at the bottom calls them one after the other.

# How to run (from the folder that contains this script and the CSV file):
#   Rscript question2_men_women.R

# Only base R is used, so no extra packages have to be installed.

#                SETTINGS
# Everything you might want to change is collected here.

# Name of the data file. It has to be in the same folder as this script.
data_file <- "Mental health Depression disorder Data.csv"

# The year used for the result. 2017 is the most recent year in the file.
year_to_study <- 2017

# In how many sub-samples (groups) the countries are split to calculate the
# uncertainty of an average (see mean_with_uncertainty below)
n_subsamples <- 5

# Names of the columns we use, exactly as they are written in the file
male_column       <- "Prevalence in males (%)"
female_column     <- "Prevalence in females (%)"
depression_column <- "Depression (%)"

# Colours used in all plots, so men and women look the same everywhere
male_colour   <- "steelblue"
female_colour <- "darkorange"

# Names of the pictures this script creates
histogram_file <- "question2_plot1_histograms.png"
scatter_file   <- "question2_plot2_scatter.png"
bar_chart_file <- "question2_plot3_bar_chart.png"

#                 FUNCTIONS
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


# Checks that the men/women table (table 2) is about depression.

# The column names of table 2 do not say which disorder they describe.
# Table 1 has a column "Depression (%)" for both sexes together. If table 2
# is about depression, that value must lie between the value for men and the
# value for women, for the same country and year.

# The function returns the share of rows (between 0 and 1) where this is true,
# and stops the script if it is true for fewer than 99 in 100 rows.
check_table_is_depression <- function(disorder_table, sex_table) {

  # Text key "country year" (for example "Netherlands 2017") to match the rows
  # of the two tables
  key_disorder <- paste(disorder_table$Entity, disorder_table$Year)
  key_sex      <- paste(sex_table$Entity, sex_table$Year)

  # match() gives, for every row of table 1, the row with the same key in
  # table 2
  position <- match(key_disorder, key_sex)

  # Three lists of numbers in the same order: depression for both sexes
  # together (table 1), for men and for women (table 2)
  both_sexes <- as.numeric(disorder_table[[depression_column]])
  men        <- as.numeric(sex_table[[male_column]])[position]
  women      <- as.numeric(sex_table[[female_column]])[position]

  # TRUE when the value for both sexes lies between the two separate values.
  # pmin() and pmax() give the smaller and the larger of the two, row by row.
  is_between <- both_sexes >= pmin(men, women) & both_sexes <= pmax(men, women)

  # mean() of TRUE/FALSE values gives the share that is TRUE
  share_ok <- mean(is_between)

  cat(sprintf("Check: 'Depression (%%)' lies between the value for men and for women in %.1f %% of %d rows\n\n",
              100 * share_ok, length(is_between)))

  # Condition: stop if the check fails for more than 1 in 100 rows
  if (share_ok < 0.99) {
    stop("The men/women table does not match the depression column, ",
         "so it may describe another disorder.")
  }

  return(share_ok)
}


# Cleans the table with depression in men and women. It returns a small, tidy
# data frame with one row per country and year, and prints how many rows are
# removed in every step.
clean_sex_table <- function(sex_table) {

  cat("Rows in the men/women table:", nrow(sex_table), "\n")

  # Step 1: keep only the columns we need and turn the text into numbers.
  # suppressWarnings() is used for the year because a few rows have years
  # such as "10000 BCE" that cannot become a number. They turn into NA
  # (= missing) and are removed in step 2.
  tidy <- data.frame(
    country = sex_table$Entity,
    code    = sex_table$Code,
    year    = suppressWarnings(as.integer(sex_table$Year)),
    male    = as.numeric(sex_table[[male_column]]),
    female  = as.numeric(sex_table[[female_column]]),
    stringsAsFactors = FALSE
  )

  # Step 2: remove rows without a value for men or women.
  # The table also has a population column that covers many more years, so
  # most rows have a population but no depression value.
  is_empty <- is.na(tidy$year) | is.na(tidy$male) | is.na(tidy$female)
  cat("  removed", sum(is_empty), "rows without a value for men and women\n")
  tidy <- tidy[!is_empty, ]

  # Step 3: remove regions and groups of countries.
  # Real countries have a country code (for example "NLD"). Rows without a
  # code are regions such as "Western Europe" or groups such as "High SDI".
  # Keeping them would count the same people twice.
  is_region <- tidy$code == ""
  cat("  removed", sum(is_region), "rows of regions and country groups\n")
  tidy <- tidy[!is_region, ]

  # Step 4: remove the row for the whole world (code "OWID_WRL")
  is_world <- tidy$code == "OWID_WRL"
  cat("  removed", sum(is_world), "rows for the world as a whole\n")
  tidy <- tidy[!is_world, ]

  # Step 5: remove impossible values.
  # A percentage of a population has to be between 0 and 100.
  is_bad <- tidy$male < 0 | tidy$male > 100 |
            tidy$female < 0 | tidy$female > 100
  cat("  removed", sum(is_bad), "rows with impossible values\n")
  tidy <- tidy[!is_bad, ]

  cat("Rows left after cleaning:", nrow(tidy), "\n")
  cat("Countries:", length(unique(tidy$country)),
      "| Years:", min(tidy$year), "to", max(tidy$year), "\n\n")

  return(tidy)
}


# Adds two columns that compare women with men in every row:
#   difference : women minus men, in percentage points
#   ratio      : women divided by men (1.5 means 1.5 times as common in women)
add_comparison <- function(data) {
  data$difference <- data$female - data$male
  data$ratio      <- data$female / data$male
  return(data)
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


# Prints the mean, median, standard deviation, minimum and maximum for men and
# for women.
describe_data <- function(one_year) {

  cat("Summary for", year_to_study, "(", nrow(one_year), "countries )\n")

  for (group in c("male", "female")) {        # loop over the two columns
    values <- one_year[[group]]               # all values of this column
    # sprintf() fills the numbers into the text: %.2f means 2 decimals
    cat(sprintf("  %-7s mean = %.2f %%, median = %.2f %%, sd = %.2f %%, min = %.2f %%, max = %.2f %%\n",
                group, mean(values), median(values), sd(values),
                min(values), max(values)))
  }
  cat("\n")
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


# Calculates the result of this question: the average for men and for women,
# the average difference and the average ratio, each with its uncertainty.
#
# Men and women are measured in the same countries, so they are compared
# country by country: the difference and the ratio are calculated for every
# country first (see add_comparison) and then averaged.
#
# The function returns a small table with one row per quantity and the
# columns "mean" and "uncertainty".
compare_women_men <- function(one_year, n_groups) {

  # The four quantities we want an average of. Each name is a column of
  # one_year.
  quantities <- c("male", "female", "difference", "ratio")

  # Empty table for the results. NA means "not filled in yet".
  results <- data.frame(mean = rep(NA, 4), uncertainty = rep(NA, 4),
                        row.names = quantities)

  # Loop over the four quantities and fill in the table row by row
  for (quantity in quantities) {
    result <- mean_with_uncertainty(one_year[[quantity]], n_groups)
    results[quantity, "mean"]        <- result["mean"]
    results[quantity, "uncertainty"] <- result["uncertainty"]
  }

  return(results)
}


# Prints the result of this question in readable sentences.
#   results: the table made by compare_women_men
#   one_year: the cleaned data, one row per country
#   sex_table: the uncleaned table 2, which still has the row for the world
print_result <- function(results, one_year, sex_table) {

  cat("Result for", year_to_study, "\n")
  cat(sprintf("  uncertainties from %d sub-samples of the %d countries\n",
              n_subsamples, nrow(one_year)))
  cat(sprintf("  men:   mean share with depression = %.2f +- %.2f %%\n",
              results["male", "mean"], results["male", "uncertainty"]))
  cat(sprintf("  women: mean share with depression = %.2f +- %.2f %%\n",
              results["female", "mean"], results["female", "uncertainty"]))
  cat(sprintf("  difference (women - men) = %.2f +- %.2f percentage points (%.0f times its uncertainty)\n",
              results["difference", "mean"], results["difference", "uncertainty"],
              results["difference", "mean"] / results["difference", "uncertainty"]))
  cat(sprintf("  ratio (women / men)      = %.2f +- %.2f\n",
              results["ratio", "mean"], results["ratio", "uncertainty"]))

  # In how many countries is depression more common in women than in men?
  n_women_higher <- sum(one_year$female > one_year$male)
  cat(sprintf("  depression is more common in women in %d of %d countries\n",
              n_women_higher, nrow(one_year)))

  # The countries with the biggest and the smallest ratio.
  # which.max() gives the row number of the highest value, which.min() the
  # lowest.
  biggest  <- one_year[which.max(one_year$ratio), ]
  smallest <- one_year[which.min(one_year$ratio), ]
  cat(sprintf("  biggest ratio:  %s (men %.2f %%, women %.2f %%, ratio %.2f)\n",
              biggest$country, biggest$male, biggest$female, biggest$ratio))
  cat(sprintf("  smallest ratio: %s (men %.2f %%, women %.2f %%, ratio %.2f)\n",
              smallest$country, smallest$male, smallest$female, smallest$ratio))

  # The file also has one row for the world as a whole. It was removed during
  # cleaning, so here we read it straight from the men/women table.
  is_world_row <- sex_table$Code == "OWID_WRL" & sex_table$Year == year_to_study
  world_male   <- as.numeric(sex_table[[male_column]][is_world_row])
  world_female <- as.numeric(sex_table[[female_column]][is_world_row])
  cat(sprintf("  world as a whole (row 'World' in the file): men %.2f %%, women %.2f %%\n\n",
              world_male, world_female))
}


# Plot 1: two histograms next to each other, men on the left and women on the
# right. A histogram shows how the values are spread over the countries.
# Both histograms use the same x axis, the same y axis and the same bars, so
# they can be compared directly.
plot_histograms <- function(one_year, file_name) {

  # The same bars (breaks) for both histograms: from the lowest to the highest
  # value, rounded to whole numbers, in steps of 0.25 %
  lowest  <- floor(min(one_year$male, one_year$female))
  highest <- ceiling(max(one_year$male, one_year$female))
  bars    <- seq(lowest, highest, by = 0.25)

  # The same y axis for both histograms. hist() with plot = FALSE only counts
  # the countries per bar without drawing anything, so we can look up the
  # tallest bar of the two histograms and use it as the top of the y axis.
  counts_male   <- hist(one_year$male,   breaks = bars, plot = FALSE)$counts
  counts_female <- hist(one_year$female, breaks = bars, plot = FALSE)$counts
  y_range       <- c(0, max(counts_male, counts_female))

  # png() opens a picture file, everything drawn after it goes into that file.
  # width and height are the size in pixels, res is the sharpness.
  png(file_name, width = 1800, height = 900, res = 200)

  # par() sets how the picture is laid out:
  #   mfrow = c(1, 2): 1 row and 2 columns, so two plots next to each other
  #   mar = c(...): the white margins (bottom, left, top, right)
  #   oma = c(...): extra room around both plots, at the top for a title
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2, 0))

  # Left plot: men.
  #   breaks     : where the bars start and end
  #   col/border : colour of the bars and of their edges
  #   ylim       : range of the y axis
  #   main       : title above the plot
  #   xlab/ylab  : text along the x axis and the y axis
  hist(one_year$male, breaks = bars, col = male_colour, border = "white",
       ylim = y_range,
       main = "Men",
       xlab = "Share of men with depression (%)",
       ylab = "Number of countries")
  # abline(v = ...) draws a vertical line, lty = 2 makes it dashed, lwd = 2
  # makes it thick. Here it marks the mean.
  abline(v = mean(one_year$male), lty = 2, lwd = 2)

  # Right plot: women, drawn in exactly the same way
  hist(one_year$female, breaks = bars, col = female_colour, border = "white",
       ylim = y_range,
       main = "Women",
       xlab = "Share of women with depression (%)",
       ylab = "Number of countries")
  abline(v = mean(one_year$female), lty = 2, lwd = 2)

  # One title above both plots. mtext() writes text in the margin:
  # side = 3 is the top, outer = TRUE uses the extra room made with oma.
  mtext(sprintf("How common is depression in each of the %d countries? (%d)",
                nrow(one_year), year_to_study),
        side = 3, outer = TRUE, font = 2, cex = 1.1, line = 0.5)

  invisible(dev.off())   # close and save the picture
}


# Plot 2: scatter plot with men on the x axis and women on the y axis.
# Every dot is one country. The dashed line shows where men and women would
# have exactly the same share. A dot above the line means depression is more
# common in women in that country.
plot_scatter <- function(one_year, file_name) {

  # The same range on both axes, so the dashed line runs from corner to corner
  lowest     <- floor(min(one_year$male, one_year$female))
  highest    <- ceiling(max(one_year$male, one_year$female))
  axis_range <- c(lowest, highest)

  png(file_name, width = 1400, height = 1300, res = 200)
  par(mar = c(4.5, 4.5, 4.5, 1))    # margins: bottom, left, top, right

  #   pch = 19  : filled round dots
  #   xlim/ylim : range of the x axis and the y axis
  plot(one_year$male, one_year$female,
       pch = 19, col = female_colour,
       xlim = axis_range, ylim = axis_range,
       xlab = "Share of men with depression (%)",
       ylab = "Share of women with depression (%)")

  # abline(a = 0, b = 1) draws the line y = 0 + 1 * x, so the line where the
  # share of women is equal to the share of men
  abline(a = 0, b = 1, lty = 2, lwd = 2)

  # Title in two lines: a big one and a smaller one below it that says what
  # exactly is shown. line = ... is the distance above the plot.
  title(main = "Depression in women against depression in men", line = 2.6)
  mtext(sprintf("Each dot is one of %d countries in %d", nrow(one_year),
                year_to_study),
        side = 3, line = 1.2, cex = 0.9)

  # Write the names of three countries: the highest value for women, and the
  # biggest and smallest ratio between women and men
  to_label <- unique(c(which.max(one_year$female),
                       which.max(one_year$ratio),
                       which.min(one_year$ratio)))
  text(one_year$male[to_label], one_year$female[to_label],
       labels = one_year$country[to_label],
       pos = 2, cex = 0.7)                     # pos = 2: left of the dot

  # The legend explains the dots and the dashed line (bty = "n": no box)
  legend("bottomright", bty = "n", cex = 0.85,
         legend = c("one country", "same share in men and women"),
         pch = c(19, NA), lty = c(NA, 2), lwd = c(NA, 2),
         col = c(female_colour, "black"))

  invisible(dev.off())   # close and save the picture
}


# Plot 3: bar chart with one bar for men and one for women. The height of a
# bar is the average share with depression over all countries, and the small
# line on top of it is the uncertainty of that average (the error bar).
#   results: the table made by compare_women_men
#   n_countries: the number of countries, for the title
plot_bar_chart <- function(results, n_countries, file_name) {

  # The two numbers to draw and their uncertainties, men first
  bar_heights     <- c(results["male", "mean"], results["female", "mean"])
  bar_uncertainty <- c(results["male", "uncertainty"],
                       results["female", "uncertainty"])

  png(file_name, width = 1300, height = 1100, res = 200)
  par(mar = c(3, 4.5, 4.5, 1))      # margins: bottom, left, top, right

  # barplot() draws the bars and gives back the x position of the middle of
  # every bar, which we need to put the error bars in the right place.
  #   names.arg : the text under every bar
  #   ylim : range of the y axis, with some room above the bars for text
  bar_middles <- barplot(bar_heights,
                         names.arg = c("Men", "Women"),
                         col = c(male_colour, female_colour), border = NA,
                         ylim = c(0, max(bar_heights) * 1.25),
                         ylab = "Share with depression (%)")

  # Title in two lines, as in plot 2
  title(main = "Depression in men and women", line = 2.6)
  mtext(sprintf("Average of %d countries in %d, with uncertainty",
                n_countries, year_to_study),
        side = 3, line = 1.2, cex = 0.9)

  # arrows() with flat heads (angle = 90) draws the error bars:
  # from mean - uncertainty up to mean + uncertainty
  arrows(bar_middles, bar_heights - bar_uncertainty,
         bar_middles, bar_heights + bar_uncertainty,
         angle = 90, code = 3, length = 0.08, lwd = 2)

  # Write the value with its uncertainty above every bar (pos = 3: above)
  text(bar_middles, bar_heights + bar_uncertainty,
       labels = sprintf("%.2f +- %.2f %%", bar_heights, bar_uncertainty),
       pos = 3)

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
  # same way as the real file
  fake <- data.frame(
    Entity = c("Aland", "Entity", "Aland", "Bland", "Bland", "Some region", "World"),
    Code   = c("ALA",   "Code",   "ALA",   "BLA",   "BLA",   "",            "OWID_WRL"),
    Year   = c("2017",  "Year",   "2017",  "2017",  "1800",  "2017",        "2017"),
    first  = c("3.5",   male_column,   "2.0", "3.0", "",     "2.5",         "2.5"),
    second = c("",      female_column, "4.0", "4.5", "",     "4.2",         "4.2"),
    stringsAsFactors = FALSE
  )

  # Test 2: table 1 of the fake file has 1 row, table 2 has 5 rows
  stopifnot(nrow(get_table(fake, 1)) == 1)
  stopifnot(nrow(get_table(fake, 2)) == 5)

  # Test 3: table 2 gets the column names from the repeated header
  stopifnot(all(c(male_column, female_column) %in% names(get_table(fake, 2))))

  # Test 4: cleaning keeps only the two real countries with a value.
  # capture.output() hides the messages that clean_sex_table prints.
  hidden <- capture.output(cleaned <- clean_sex_table(get_table(fake, 2)))
  stopifnot(nrow(cleaned) == 2)
  stopifnot(all(cleaned$country == c("Aland", "Bland")))
  stopifnot(all(cleaned$female == c(4.0, 4.5)))

  # Test 5: the comparison columns. Aland has men 2.0 and women 4.0, so the
  # difference is 2.0 and the ratio is 2.0.
  compared <- add_comparison(cleaned)
  stopifnot(compared$difference[1] == 2.0)
  stopifnot(compared$ratio[1] == 2.0)

  # Test 6: keep_one_year keeps only the rows of the year we ask for
  two_years <- data.frame(country = c("Aland", "Aland"), year = c(2016, 2017))
  stopifnot(nrow(keep_one_year(two_years, 2017)) == 1)

  cat("All tests passed.\n\n")
}


# MAIN PROGRAM
# The functions above are called here, one step after the other.


# 1. Test the functions before using them
# If a function has a mistake, the script stops here instead of giving wrong
# results later.
run_tests()

# 2.. Read the data 
# raw_data is the whole file as one big table of text
raw_data <- load_data(data_file)
cat("Rows in the file:", nrow(raw_data), "\n")

# 3. Take the two tables we need out of the file
# Table 1 holds the share of people with each disorder (both sexes together).
# It is only used for the check in step 3.
# Table 2 holds the share for men and for women separately.
disorder_table <- get_table(raw_data, 1)
sex_table      <- get_table(raw_data, 2)
cat("  used:", nrow(disorder_table), "rows of table 1 (depression, both sexes) and",
    nrow(sex_table), "rows of table 2 (men and women)\n")
cat("  not used:", nrow(raw_data) - nrow(disorder_table) - nrow(sex_table),
    "rows that belong to other tables\n")

# Condition: stop if table 2 does not have the two columns we need
if (!all(c(male_column, female_column) %in% names(sex_table))) {
  stop("Table 2 of the file does not have the columns '", male_column,
       "' and '", female_column, "'.")
}

# 4. Check that the men/women table is about depression
share_ok <- check_table_is_depression(disorder_table, sex_table)

# 5. Clean the men/women table
# all_years has one row per country and year, with the columns male and
# female (the file has the years 1990 to 2017), plus the difference and the
# ratio between women and men
all_years <- clean_sex_table(sex_table)
all_years <- add_comparison(all_years)

# Keep only the year we want to study: one row per country
one_year <- keep_one_year(all_years, year_to_study)

# 6. Describe the data
describe_data(one_year)

# 7. Plot 1: histograms
plot_histograms(one_year, histogram_file)

# 8. Plot 2: scatter plot, men against women
plot_scatter(one_year, scatter_file)

# 9. Result: women compared with men
# results is a small table with the mean and the uncertainty of four
# quantities: men, women, the difference and the ratio
results <- compare_women_men(one_year, n_subsamples)
print_result(results, one_year, sex_table)

# 10. Plot 3: bar chart, men against women
plot_bar_chart(results, nrow(one_year), bar_chart_file)

cat("Done. Created:", histogram_file, scatter_file, bar_chart_file, "\n")
