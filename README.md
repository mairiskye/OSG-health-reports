# OSG-health-reports
### Purpose
The code in this repository does two things:

1. Cleans four raw data files for processing, and merges two of them with their respective historic time-series data.

2. Auto-generates 32 council-specific OSG Health Reports (as word documents) using the above data.

### What you will need

To generate the reports, six data files are needed. The first four are the raw quarterly datasets provided by OSG Custodians (file names will vary AND FILE TYPE MAY VARY):

* PAF-Match data
* Errors/Warnings data
* Property Classifications data
* One Scotland Gazetteer Uploads data

The other two are historic time-series datasets which are generated in the previous reporting period. These can be found in the [Gazetteer Statistics sharepoint](https://impservihub.sharepoint.com/:f:/s/GazetteerStatistics/EhllI09nuMhJofC8-g_PKUUBsmlZWA8jDYSETXe2wGvC7g?e=iXb1uy). Go to *Documents/Automated Reports/Health Report* **XX***/Data/Clean Data/*. where **XX** is the latest edition of the report (e.g. Health Report **06**). The files you will need will be named as follows:

classification-time-series-yyyy-mm-01.csv
uploads-time-series-yyyy-mm-01.csv

which are dated according to the last month of data included in the last reporting period.

### Instructions
To generate a new set of reports, follow these instructions in order.

#### Step 1
Clone Repository

####  Step 2
In the root directory of the  project is a file called **example.yml**. Open this and save a copy within the same directory named **config.yml**. Now open this new file and update the date to correspond to the last month of data included in the new reporting period.

For example, for the quarter August 2022 to September 2022 inclusive, this:
```
default:
  reporting_period: "yyyy-mm-01"
```
should become:
```
default:
  reporting_period: "2022-09-01"
```
This variable will be used in csv file names generated by the data cleaning scripts, and will also be used to populate a date column when concatenating with the time series datasets.

#### Step 3
Save the four raw datasets for the current period within the *Cleaning Raw Data/Raw Data/* folder.

#### Step 4
Save the classifications and uploads time-series datasets (instructions for obtaining these under [What you will need](#what-you-will-need)) within the *Cleaning Raw Data/Time Series Data/* folder.

#### Step 5
Update file names in data cleaning scripts.

Within *Cleaning Raw Data/Data Cleaning Scripts/*, there are four data cleaning scripts each of which reads in one or two of the six required datasets in the first few lines under the comment **#READ IN DATA**. For each script navigate to where the data is read and update the file names referenced so that they match the files added in steps 3 and 4. 

<details>
  <summary>Example</summary>
  The script *clean_and_merge_classification_data.R* has these first     lines:

```
#READ IN DATA-----------------
#read in new quarterly data
new_class_data <- readxl::read_xlsx("Clean Raw Data/Raw Data/OSG Classifications 2022-08-24.xlsx", trim_ws = TRUE, skip = 1)
#read in old time-series data 
previous_class_data <- read.csv("Clean Raw Data/Time Series Data/historic-classification-data.csv") 

#-------------------------------
```

As indicated by the prefix clean_and_merge_, this script is reading    in new quarterly data, cleaning it, and merging it with clean historic time-series data. Therefore it requires both the raw quarterly classification data (referenced as "/OSG Classifications 2022-08-24.xlsx" in the example) and the historic time-series classification data (referenced as "/historic-classification-data.csv" in the example). 

You would have to: 

Update the new_class_data variable to read data from the classifications data file you added to *Cleaning Raw Data/Raw Data/*. 

Update the previous_class_data variable to read data from the classifications data file you added to *Cleaning Raw Data/Time Series Data/*. 

</details>


#### Step 6
Now that file name references have been updated, run each of the four data cleaning scripts. Order does not matter. This will populate the *Make Reports/Clean Data/* folder with four .csv files whose names include the dataset and the appropriate ISO-Date for the current reporting period (retrieved programmatically from the config.yml file made earlier).

#### Step 7 
Update contents in the markdown template.
The OSG Custodian will provide generic text to include in the report. It's best if a mock-up report is done using an older Word report or on a copy of the style-reference.docx. That way we know where each graph should be relative to the text. The graphs are generated using inline code chunks in an .Rmd template, this code should not need altered - only the surrounding markdown.
Open *Make Reports/health_report_template.Rmd* which will contain the text from the last report and replace text as appropriate. This [R markdown cheat sheet](https://rmarkdown.rstudio.com/authoring_basics.html) and [this chapter](https://bookdown.org/yihui/rmarkdown-cookbook/word.html) on R makrdown to Word output should be useful for styling.

#### Step 8
Run the *Make Reports/render_reports.R* script. 

This reads in the clean data from file, reads in the plotting and helper functions (which generate the 10 barplots in the report), and then renders 32 council-specific word document reports from a parameterised Rmarkdown template (*Make Reports/health_report_template.Rmd*). Rmarkdown scrapes document style (colour, font, text-size, heading stylings) from the *Make Reports/style-reference.docx* file. Instructions on how to make style modifications can be found [here](https://rmarkdown.rstudio.com/articles_docx.html).

<details>
  <summary>Notes</summary>
  Note that a report will fail to generate if there exists a report with the same name in *Make Reports/Final Reports/*. If you encounter this problem (for instance if you've run the code and want to re-run after spotting and correcting a typo), then you will either have to delete the previously generated report(s) from the folder OR alter the output file name dictated in the *Make Reports/render_reports.R* script.
  
  Also you must ensure that the *Make Reports/style-reference.docx* document is not open on your copmuter when this script is run.
</details>

#### Step 9
Update the [Gazetteer Statistics sharepoint](https://impservihub.sharepoint.com/:f:/s/GazetteerStatistics/EhllI09nuMhJofC8-g_PKUUBsmlZWA8jDYSETXe2wGvC7g?e=iXb1uy). 

* Go to the sharepoint and create a new Health Report XX folder in *Documents/Automated Reports/*, and create the following folder structure:

<<<<<<< HEAD
![](https://raw.githubusercontent.com/mairiskye/OSG-health-reports/main/Images/OSG%20health%20report%20series%20folder%20structure.png)
=======
![](/Images/OSG health report series folder structure.png)
>>>>>>> 4ac296ccf4474b2314c1e641717fdfa3b09ff43b
* Upload a copy of the raw quarterly datasets provided by the OSG Custodian to *Health Report XX/Data/Raw Data (Input)/*. This is the same data you would have added to *Cleaning Raw Data/Raw Data/* in the R project directory in step 3.

* From the R project directory, copy the four new clean data files from *Make Reports/Clean Data/* to *Health Report XX/Data/Clean Data (Output)/*.

* From the R project directory, copy the auto-generated reports folder  *Make Reports/Final Reports* to *Health Report XX/Reports/Draft Reports/*. If approved by the OSG Custodian and no changes need to be made, the folder can be move to *Health Report XX/Reports/Final Reports/*, otherwise make tweaks to the template and rerun the *render_reports.R* script as appropriate.

### Future work

Some exploratory code has been written which renders mock-ups of alternative visualisations for the time-series data; as line graphs instead of bar charts. This is more intuitive to read, and allows inclusion of a broader time series. This can be found in the folder *make Reports/time series graph mock-ups* if the report is updated in the future.

Current visualisation:

<<<<<<< HEAD
![](https://raw.githubusercontent.com/mairiskye/OSG-health-reports/main/Images/bar%20plot.png)

Alternative:

![](https://raw.githubusercontent.com/mairiskye/OSG-health-reports/main/Images/line%20plot.png)
=======
![](/Images/bar plot.png)

Alternative:

![](/Images/line plot.png)
>>>>>>> 4ac296ccf4474b2314c1e641717fdfa3b09ff43b
