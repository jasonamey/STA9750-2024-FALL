# Code extracts all the links to the .zip files from the NYC tax assessment archive data
# HTML was copy/pasted and saved locally as page is generated with javascript

library(httr2)
library(rvest)

html_file_path <- "final_project/src/scripts/scraping/dept_of_finance_tax_assessment_archives.html"  # Replace with the path to your HTML file

base_url <- "https://www.nyc.gov"

webpage <- read_html(html_file_path)

# Extract all <a> tags 
links <- webpage |>
  html_elements("a") |>               
  html_attr("href")                             

# Append the base URL for relative links
full_urls <- paste0(base_url, links)

full_urls <- grep("\\.zip$", full_urls, value = TRUE)
full_urls <- grep("/tar/tc", full_urls, value = TRUE)

# Create a data frame to store the results
url_data <- data.frame(
  full_urls
)

output_csv_path <- "final_project/data/tax_class_urls.csv"  # Define the output file path
write.csv(url_data, file = output_csv_path, row.names = FALSE)

