# Question 2: Are men or women more likely to have depression in 2017?
# Python version of the plots made by Q2_Men-vs_women.R
#
# Introduction to programming (MAT2007) - data analysis project
#
# All the analysis (cleaning, sub-sampling) is done in R. This script only
# draws the plots. It reads the two CSV files that Q2_Men-vs_women.R writes:
#   question2_data.csv    : the cleaned data, one row per country
#   question2_results.csv : the averages for men and women (and the difference
#                           and ratio), each with its uncertainty
#
# Plots:
#   1. histograms of depression in men and in women
#   2. scatter plot, men against women, one dot per country
#   3. bar chart of the average for men and for women, with error bars
#
# How to run:
#   1. Run Q2_Men-vs_women.R first (it makes the two CSV files)
#   2. In the same folder: python question2_men_women_plots.py
# Needs numpy, pandas and matplotlib (pip install numpy pandas matplotlib)

import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


# SETTINGS (the same as in the R script)

# Read and save everything in the folder this script is in
folder = os.path.dirname(os.path.abspath(__file__))

# Colours used in all plots, so men and women look the same everywhere
male_colour = "steelblue"
female_colour = "darkorange"

histogram_file = "question2_plot1_histograms_py.png"
scatter_file = "question2_plot2_scatter_py.png"
bar_chart_file = "question2_plot3_bar_chart_py.png"


# Saves the plot next to this script and then shows it
def save_and_show(file_name):
    out_file = os.path.join(folder, file_name)
    plt.savefig(out_file, dpi=200)     # save before plt.show()
    print("Saved the plot to", out_file)
    plt.show()


# READ THE RESULTS OF THE R SCRIPT

one_year = pd.read_csv(os.path.join(folder, "question2_data.csv"))
results = pd.read_csv(os.path.join(folder, "question2_results.csv"))

# Use the quantity names (male, female, difference, ratio) as row names,
# so we can write results.loc["male", "mean"], like results["male", "mean"] in R
results = results.set_index("quantity")

n_countries = results["n_countries"].iloc[0]
year = results["year"].iloc[0]
print(results)


# PLOT 1: histograms, men on the left and women on the right

# The same bars (bins) for both: from the lowest to the highest value,
# rounded to whole numbers, in steps of 0.25 %
lowest = np.floor(min(one_year["male"].min(), one_year["female"].min()))
highest = np.ceil(max(one_year["male"].max(), one_year["female"].max()))
bins = np.arange(lowest, highest + 0.25, 0.25)

# sharey=True: both histograms use the same y axis, so they can be compared
fig, (left, right) = plt.subplots(1, 2, figsize=(10, 5), sharey=True)

left.hist(one_year["male"], bins=bins, color=male_colour, edgecolor='white')
left.axvline(one_year["male"].mean(), color='black', linestyle='--', linewidth=2,
             label=f"mean = {one_year['male'].mean():.2f} %")
left.set_title("Men")
left.set_xlabel("Share of men with depression (%)")
left.set_ylabel("Number of countries")
left.legend()

right.hist(one_year["female"], bins=bins, color=female_colour, edgecolor='white')
right.axvline(one_year["female"].mean(), color='black', linestyle='--', linewidth=2,
              label=f"mean = {one_year['female'].mean():.2f} %")
right.set_title("Women")
right.set_xlabel("Share of women with depression (%)")
right.legend()

# The x axis is the same for both too
left.set_xlim(lowest, highest)
right.set_xlim(lowest, highest)

fig.suptitle(f"How common is depression in each of the {n_countries} countries? "
             f"({year})", fontweight='bold')
plt.tight_layout()
save_and_show(histogram_file)


# PLOT 2: scatter plot, men on the x axis and women on the y axis

plt.figure(figsize=(7, 6.5))
plt.scatter(one_year["male"], one_year["female"], color=female_colour,
            label="one country")

# Dashed line where men and women have the same share (y = x)
plt.plot([lowest, highest], [lowest, highest], color='black', linestyle='--',
         linewidth=2, label="same share in men and women")

# Write the names of three countries: the highest value for women, and the
# biggest and smallest ratio between women and men.
# idxmax() / idxmin() give the row of the highest / lowest value, like
# which.max() / which.min() in R
to_label = [one_year["female"].idxmax(), one_year["ratio"].idxmax(),
            one_year["ratio"].idxmin()]
for row in set(to_label):          # set() removes doubles, like unique() in R
    plt.text(one_year.loc[row, "male"] - 0.08, one_year.loc[row, "female"],
             one_year.loc[row, "country"], ha='right', va='center', fontsize=8)

plt.xlim(lowest, highest)
plt.ylim(lowest, highest)
plt.title("Depression in women against depression in men\n"
          f"Each dot is one of {n_countries} countries in {year}")
plt.xlabel("Share of men with depression (%)")
plt.ylabel("Share of women with depression (%)")
plt.legend(loc='lower right')
plt.grid(True)
plt.tight_layout()
save_and_show(scatter_file)


# PLOT 3: bar chart of the averages with error bars (numbers from R)

bar_heights = [results.loc["male", "mean"], results.loc["female", "mean"]]
bar_uncertainty = [results.loc["male", "uncertainty"],
                   results.loc["female", "uncertainty"]]

plt.figure(figsize=(6.5, 5.5))
# yerr draws the error bars, capsize gives them flat ends
plt.bar(["Men", "Women"], bar_heights, yerr=bar_uncertainty, capsize=10,
        color=[male_colour, female_colour], error_kw={'linewidth': 2})

# Write the value with its uncertainty above every bar
for i in range(2):
    plt.text(i, bar_heights[i] + bar_uncertainty[i] + 0.1,
             f"{bar_heights[i]:.2f} +- {bar_uncertainty[i]:.2f} %", ha='center')

plt.ylim(0, max(bar_heights) * 1.25)
plt.title("Depression in men and women\n"
          f"Average of {n_countries} countries in {year}, with uncertainty")
plt.ylabel("Share with depression (%)")
plt.grid(axis='y')
plt.tight_layout()
save_and_show(bar_chart_file)