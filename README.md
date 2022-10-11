# OSG-health-reports
### Purpose
This repository serves two functions:

1. To clean four raw data files for processing.

2. To auto-generate 32 council-specific OSG Health Reports

### Data
Four datasets are required to create these reports. These are:

* PAF-Match
* Errors/Warnings
* Property Classifications
* One Scotland Gazetteer Uploads

All raw data and time series data will be stored, as before,  in this [teams group](https://impservihub.sharepoint.com/:f:/s/GazetteerStatistics/EhllI09nuMhJofC8-g_PKUUBsmlZWA8jDYSETXe2wGvC7g?e=iXb1uy) in a new folder called *Automated Reports/*.

Every report period should have its own folder named according to which edition of the report it is (e.g. **Health Report 06**). Within which is a **Data** folder and a **Reports** folder. The data folder contains **Raw Data** for the reporting period and **Clean Data** data (generated in the scripts). 

### Instructions

#### Step 1
Clone Repository

####  Step 2
In the root directory of the  project is a file called **exmaple.yml**. Open this and save a copy within the same directory named **config.yml**. Now update the ISO date format in this new file to correspond to the current reporting period.

For example this:
```
default:
  reporting_period: "yyyy-mm-01"
```
should become:
```
default:
  reporting_period: "2027-08-01"
```
This variable will be used by the cleaning scripts in new csv file names and will also be used to populate a date column within time series datasets. Note that 'day' does not matter as data is grouped by month/year.

#### Step 3
Save the raw data for the current period within the *Clean Raw Data/Raw Data/* folder.

#### Step 4
Save the time series data (classification and uploads time series csv files generated in the previous year) within the *Clean Raw Data/Time Series Data/* folder.

#### Step 5
The cleaning scripts (within *Clean Raw Data/Data Cleaning Scripts/*) either simply clean data or clean and merge with historic time series data - this is indicated by the file names. Data file references in these .R scripts must be updated to correspond to the new file names (those added in steps 1 and 2). The read.csv() (or read.xlsx()) commands are the first lines in each script to make them easier to find and update (one change required for cleaning only scripts, two for cleaning and merging).

#### Step 6
Once file references have been updated, run the data cleaning scripts. Order does not matter. This should populate the *Make Reports/Clean Data/* folder with four .csv files whose names include the approrpriate ISO-Date for the current reporting period.

#### Step 7
Run the *Make Reports/render_reports.R* script. 
This reads in the clean data from file, reads in the plotting and helper functions (which generate the 10 barplots in the report), and then renders 32 council-specific word document reports from a parameterised Rmarkdown template (*Make Reports/health_report_template.Rmd*). Rmarkdown scrapes document style (colour, font, size, margins) from the *Make Reports/style-reference.docx* file. Instructions on how to make modifications can be found [here](https://rmarkdown.rstudio.com/articles_docx.html).

#### Step 8
Copy the clean data, raw data and the reports to the Gazetteer Statistics Teams group in a new Health Report folder for future use/reference. These will not be pushed to the repository on git hub.
