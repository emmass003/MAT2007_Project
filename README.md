
Project: Depression in numbers
Introduction to Programming (MAT2007) data analysis project
Emma Schwarz

Two question to answer:
1. In which country was depression most common in 2017?
2. Were men or women more globally affected in 2017?

Dataset:
Dataset link: https://www.kaggle.com/datasets/thedevastator/uncover-global-trends-in-mental-health-disorder/discussion?sort=recent-comments
File used: Mental health Depression Disorder Data.csv

Important info: use the CSV file as is after downloading and do ot open or save if again with Numbers or Excel because computers with Dutch or other eurpeen number setting will turn 1.832 into one thousand eight hundred thirty two, which will change the data and the results. 


Data cleaning:
Cleaning on the data was done using the scripts, with indication of how many rows are removed while running. The file included four tables stacked on top of eachother and I only used two for the two questions. The first one used was the table including the share of the population with depression (depression%, question 1) and the share of men and of women (prevalence in males (%) and prevelance in females (%), question 2). The rows removed were:
1. Rows without any depression value (41.339 rows from men and women table), because they only contain a population
2. Regions like "western europe" (980 rows) because those people were already counted in their country, avoifding counting people twice
3. There were some entries called "world" (28 rows), since this inly all the countries added together not just a single country. 
4. Finally, all years execept 2017 was removed because I was interested in the most recent data available. 

After all of these cleaning steps, 195  rows were left each representing one country.

How to run the Codes:
No extra packages are needed because the script only use base R.

How to do it:
1. Put the scripts (`Q1_Countries.R`, `Q2_Men-vs_women.R`, `Q1_Plots_python.py` and `Q2_Plots_Python.py`) and the Mental health Depression Disorder Data.csv in the same folder.
2. Open folder in VS code
3. Open a new terminal in VS code and run the R scripts in any desireable order.Each script will test its own functions, clean the data and will print the results into the terminal and will save its output in the same folder.
4. Then run the two Python scripts. They read the CSV files made by the R scripts and draw the same plots in Python. Python needs numpy, pandas and matplotlib (Put this in terminal if it doesn't work -> `pip install numpy pandas matplotlib`).

What script makes which files?


- `Q1_Countries.R` --> `question1_plot_countries.png`, `question1_ranking.csv`, `question1_average.csv` 
- `Q2_Men-vs_women.R` -->  `question2_plot1_histograms.png` (not used in the report or presentation), `question2_plot2_scatter.png`, `question2_plot3_bar_chart.png`, `question2_data.csv`, `question2_results.csv` 
- `Q1_Plots_python.py` --> `question1_plot_countries_py.png` 
- `Q2_Plots_Python.py` --> `question2_plot1_histograms_py.png` (not used in the report or presentation), `question2_plot2_scatter_py.png`, `question2_plot3_bar_chart_py.png` 

Conditions:
1. As already mentioned before the scripts must be in the same folder as the CSV file with the data and the CSV file must keep it's original name. 
2.  the file must be unedited and the original
  -> if something is missing in the file, like a missing column the script will print that there is something wrong.
3.  The settings can be changed at the top of every single script.
 -> data_ file: the name of the CVS file
 -> year_to_study: 2017 by default
 -> n_subsamples: groups for the subsampling       technique (me: 5)
 ->Q1_Countries.R  has n_to_show: how many countries should be shown in the ranking ( me: five top and five bottom)
