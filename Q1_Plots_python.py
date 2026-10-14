# Question 1: In which countries is depression most common in 2017?
# Introduction to programming (MAT2007) - data analysis project
# All the analysis (cleaning, sub-sampling, ranking) is done in R. This script
# only draws the plot. It reads the two CSV files that Q1_Countries.R writes:
#   question1_ranking.csv : all countries, from the highest to the lowest
#   question1_average.csv : the average of all countries with its uncertainty

# How to run:
#   1. Run Q1_Countries.R first (it makes the two CSV files)
#   2. In the same folder: python question1_countries_plot.py
# Needs pandas and matplotlib (pip install pandas matplotlib)

import os

import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.lines import Line2D
from matplotlib.patches import Patch


# SETTINGS (the same as in the R script)

# Read and save everything in the folder this script is in
folder = os.path.dirname(os.path.abspath(__file__))

n_to_show = 5                  # countries shown at the top and at the bottom
highest_colour = "firebrick"   # countries with the most depression
lowest_colour = "grey"         # countries with the least depression

plot_file = "question1_plot_countries_py.png"


# READ THE RESULTS OF THE R SCRIPT

ranking = pd.read_csv(os.path.join(folder, "question1_ranking.csv"))
average = pd.read_csv(os.path.join(folder, "question1_average.csv"))

mean_all = average["mean"][0]
uncertainty_all = average["uncertainty"][0]
n_countries = average["n_countries"][0]
year = average["year"][0]

print(f"Mean of all {n_countries} countries: {mean_all:.2f} +- {uncertainty_all:.2f} %")


# PLOT: the highest and the lowest countries

# The ranking is already sorted in R, highest first
highest_countries = ranking.head(n_to_show)
lowest_countries = ranking.tail(n_to_show)
shown = pd.concat([highest_countries, lowest_countries])

# barh() draws the first bar at the bottom, so the order is reversed
# ([::-1]) to get the highest country at the top, like in the R plot
heights = list(shown["depression"])[::-1]
labels = list(shown["country"])[::-1]
colours = ([highest_colour] * n_to_show + [lowest_colour] * n_to_show)[::-1]

# Create the plot
plt.figure(figsize=(9, 6))
plt.barh(labels, heights, color=colours)

# Dashed vertical line at the average of all countries
plt.axvline(mean_all, color='black', linestyle='--', linewidth=2)

# Write the value next to every bar
for i in range(len(heights)):
    plt.text(heights[i] + 0.05, i, f"{heights[i]:.2f} %", va='center', fontsize=9)

# Add titles and labels
plt.title("Countries with the most and the least depression\n"
          f"The {n_to_show} highest and the {n_to_show} lowest of "
          f"{n_countries} countries in {year}")
plt.xlabel("Share of population with depression (%)")
plt.xlim(0, max(heights) * 1.25)

# Add legend: Patch makes a coloured box and Line2D a line, only for the
# legend (like fill = and lty = in legend() in R)
legend_items = [
    Patch(color=highest_colour, label=f"{n_to_show} highest countries"),
    Patch(color=lowest_colour, label=f"{n_to_show} lowest countries"),
    Line2D([], [], color='black', linestyle='--', linewidth=2,
           label=f"mean of all {n_countries} countries "
                 f"({mean_all:.2f} +- {uncertainty_all:.2f} %)"),
]
plt.legend(handles=legend_items, loc='lower right', fontsize=9)

# Add grid
plt.grid(axis='x')
plt.tight_layout()

# Save the plot to a file (before plt.show, otherwise the file is empty)
out_file = os.path.join(folder, plot_file)
plt.savefig(out_file, dpi=200)
print("Saved the plot to", out_file)

# Display the plot
plt.show()
