#read reporting month date (ISO formatted String) from config.yml
session <- config::get("reporting_month")

#read in data and create council vector
uploads_data <- read.csv("Make Reports/Clean Data/uploads-time-series-2022-09-01.csv")
class_data <- read.csv("Make Reports/Clean Data/classification-time-series-2022-09-01.csv")
paf_data <- read.csv("Make Reports/Clean Data/paf-2022-09-01.csv")
errors_data <- read.csv("Make Reports/Clean Data/errors-2022-09-01.csv")
councils <- unique(paf_data$CouncilName)

#read in helper functions
source("Make Reports/Helper Functions/wrap_it.R")

#read in plotting functions 
source("Make Reports/Plotting Functions/uploads/uploads_council_group_barplot.R")
source("Make Reports/Plotting Functions/uploads/uploads_regional_summary_barplot.R")
source("Make Reports/Plotting Functions/uploads/uploads_council_group_recent_barplot.R")
source("Make Reports/Plotting Functions/classification/class_council_group_barplot.R")
source("Make Reports/Plotting Functions/classification/class_region_summary_barplot.R")
source("Make Reports/Plotting Functions/classification/class_single_council_stacked.R")
source("Make Reports/Plotting Functions/paf-match/paf_council_group_barplot.R")
source("Make Reports/Plotting Functions/paf-match/paf_region_summary_barplot.R")
source("Make Reports/Plotting Functions/error/errors_council_group_barplot.R")
source("Make Reports/Plotting Functions/error/errors_region_summary_barplot.R")

#for (council in councils) {
council <- "Midlothian"

  rmarkdown::render(
    "Make Reports/data_report_template.Rmd", 
    params = list(
      council = council,
      classifications = class_data,
      uploads = uploads_data,
      paf = paf_data,
      errors = errors_data,
      date = format(max(as.Date(session)), "%B %Y")
      ),
    output_file = paste0("Final Reports/",council, "-osg-health-check-report.docx")
    )
  #}