# RO-Wave Energy
Analysis of reverse osmosis variable-pressure wave-driven desalination systems.

To re-produce the plots and results from this project, simply run `init.R`, then `model.R`, then the other scripts in any order.

## File Structure

### ~/data
Data is stored in ~/data. This includes the raw data, stored in `.csv` files, as well as multiple `.Rdata` objects created throughout the project.

### ~/for the memes
This folder contains memes produced throughout our project.

### ~/plots
This folder contains all the plots that were produced.

### ~/src
This folder contains source code.


## Source Code Descriptions

### init.R
This script reads in the raw data from data/raw/ and cleans it, producing two dataframes: fullData.Rdata and data.Rdata. Data.Rdata has outliers removed. __The primary dataframe used for our analysis is data.Rdata__.

### eda_plots.Rmd
This file contains some exploratory data analysis as well as the adaptive lasso.

### explore.R
This file some EDA which computes summary statistics for each experiment as well as performs some bootstrapping for confidence intervals for the median.

### function_and_crosscorr.R
This script produces cross-correlation plots between several of the variables (e.g. pressure and salinity).

### sliceanddice.R
This file contains some code which determines which additional outliers to remove from the data.

### contest.R
This script produces some t-shirt design plots.

### adaptivelasso.R
This cript performs the adaptive lasso for each experiment.

### code_review.R
This script is a combination of several other scripts which we provided to another team for our code review.

### model.R
This script performs data de-trending and analysis using the Fourier Transform. It also does some exploratory principal component analysis.

### model2.R
More experimentation with the adaptive lasso.

### model3.R
In this script, we construct a linear model for permeate conductivity.

### boxplots.R
This file produces permeate conductivity boxplots.

### anova.R
This file does hypothesis testing and constructs confidence intervals for the difference in mean permeate conductivity between experiments.

### latex folder
The code in this folder compiles a documents containing all of our time-series plots for each experiment, found at plots/plots.pdf.

### plots/fft.R
This script produces Fourier Transform plots of feed pressure for each experiment.

### plots/tsplots.R
This script produces time-series plots for each variable for each experiment.



