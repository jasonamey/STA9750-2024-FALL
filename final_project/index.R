library(readr)
library(stringr)  
library(httr2)   
  

BBL <- read_csv("data/final_project/finance/avroll.csv")

finance_data_2010_residential <- finance_data_2010 |> 
  filter(TAXCLASS != "4" & TAXCLASS != "3" & B == 3)



data <- finance_data_2010 |> 
    filter(B == 3 & BLOCK == 1068 & LOT == 37)
    
glimpse(data)

park_corner <- finance_data_2010_residential |>
  filter(BLOCK == 1068 & LOT == 37)

park_corner$FULLVAL



glimpse(finance_data_2010_residential)

file_path_sample <- "data/final_project/fy24_sample.txt"

text_lines <- readLines(file_path_sample)

records <- strsplit(text_lines,"\n")

parsed_records <- lapply(records, function(record) strsplit(record, "\t"))


# Create Sampled Data



URL_2024_TC1 <-"https://www.nyc.gov/assets/finance/downloads/tar/fy24_tc1.zip"
URL_2024_TC234 <- "https://www.nyc.gov/assets/finance/downloads/tar/fy24_tc1.zip"

process_records_from_url <- function(year, tax_class, url, borough) {
  library(stringr) # For string manipulation
  library(httr)    # For downloading files
  
  # Download and unzip the target data file from URL
  temp_file <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  download.file(url, temp_file)
  unzip(temp_file, exdir = temp_dir)
  
  # Find the first extracted file
  file_path <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)[1]
  
  # Download shapefile if it does not already exist
  shapefile_name <- "nyc_borough_boundaries.zip"
  if (!file.exists(file_name)) {
    download.file(url, destfile = file_name)
  }
  
  # Read and process the target data file
  text_lines <- readLines(file_path)
  records <- strsplit(text_lines, "\n")
  
  # Parse, filter, and clean records
  parsed_records <- lapply(records, function(record) strsplit(record, "\t"))
  
  filtered_records <- lapply(parsed_records, function(record) {
    if (record[[1]][[2]] == "3") { # Adjust index as necessary
      return(record)
    } else {
      return(NULL) # Return NULL to indicate exclusion
    }
  })
  filtered_records <- filtered_records[!sapply(filtered_records, is.null)]
  
  cleaned_records <- lapply(filtered_records, function(record) {
    sapply(record, trimws)
  })
  
  # Convert to a data frame
  df <- do.call(cbind, cleaned_records)
  df <- as.data.frame(t(df))
  
  return(df)
}



URL_2024_TC234 <- "https://www.nyc.gov/assets/finance/downloads/tar/fy24_tc1.zip"

text_lines <- readLines(file_path)

records <- strsplit(text_lines,"\n")

parsed_records <- lapply(records, function(record) strsplit(record, "\t"))

filtered_records <- lapply(parsed_records, function(record) {
  if (record[[1]][[2]] == "3") {
    return(record)
  } else {
    return(NULL) # Return NULL to indicate exclusion
  }
})

filtered_records <- filtered_records[!sapply(filtered_records, is.null)]

<!-- Block: 1068 -->

<!-- Lot: 37 -->

cleaned_records <- lapply(filtered_records, function(record) {
  sapply(record, trimws)
})

df <- do.call(cbind, cleaned_records)
df <- as.data.frame(t(df))

property <- df |> 
  filter(V3 == "01068" & V4 == "0037")

glimpse(property)

write.csv(df,"data/final_project/sampled/fy24_tc234_sampled.csv", row.names = FALSE)
```


# Import real estate valuations from NYC Department of Finance 
finance_data_2010 <- read_csv("data/final_project/finance/avroll.csv")

# Import all finance data for tax classes 2, 3 and 4
finance_data_class_234 <- read_csv("data/final_project/finance/tc234_2010.csv") |>

# Limit to Brooklyn and the residential tax class of 2, and the
bk_finance_data_class_2_2010 <- finance_data_class_234 |>
  filter(BORO == 3 & TXCL == 2)

bk_finance_data_class_2_2010

# Import all finance data for tax classes 2, 3 and 4
finance_data_class_1 <- read_csv("data/final_project/finance/tc1_2010.csv")

# Limit to Brooklyn:
bk_finance_data_class_1_2010 <- finance_data_class_1 |>
  filter(BORO == 3)
  
bk_finance_data_class_1_2010 <- bk_finance_data_class_1_2010 |> 
    select()
  
View(bk_finance_data_class_1_2010)
  
# Convert EASE column to consistent data types for row binding
bk_finance_data_class_1_2010 <- bk_finance_data_class_1_2010 %>% mutate(EASE = as.character(EASE))
bk_finance_data_class_2_2010 <- bk_finance_data_class_2_2010 %>% mutate(EASE = as.character(EASE))



bk_finance_data_2010 <- bind_rows(bk_finance_data_class_1_2010, bk_finance_data_class_2_2010)

bk_finance_data_2010

#Re-create dataset with just Brooklyn data, borough 3 
brooklyn_2010 <- finance_data_2010 |>
  filter(B == 3)

#Save new dataset to csv
write_csv(brooklyn_2010, "data/final_project/brooklyn_2010.csv")

#Import Borough, Block, Lot info for PPW from The NYC Department of Planning
BBL <- read_csv("data/final_project/planning/lot_selector_PPW_PLUTO.csv")



FY24_TC234 <- "data/final_project/finance/fy24_tc234.txt"

text_lines <- readLines(FY24_TC234)

records <- strsplit(text_lines,"\n")

parsed_records <- lapply(records, function(record) strsplit(record, "\t"))

parsed_records[[1]][[1]][26]


FY24_TC1_URL <- "https://www.nyc.gov/assets/finance/downloads/tar/fy24_tc1.zip"

# Create temporary files and directories
temp_file <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

# Send GET request to download the file
response <- request(FY24_TC1_URL) |> 
  req_perform() 

# Handle the response content as a zip file
writeBin(resp_body_raw(response), temp_file)
  
# Unzip the downloaded file into a temporary directory
unzip(temp_file, exdir = temp_dir)

file_path <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE)[1]

text_lines <- readLines(file_path)
records <- strsplit(text_lines, "\n")
  
parsed_records <- lapply(records, function(record) strsplit(record, "\t"))
  
filtered_records <- lapply(parsed_records, function(record) {
  if (record[[1]][[2]] == "3") {
    return(record)
  } else {
    return(NULL)
  }
})

filtered_records <- filtered_records[!sapply(filtered_records, is.null)]

filtered_records[[1]][[1]]

write.csv(df,"data/final_project/sampled/fy24_tc234_sampled.csv", row.names = FALSE)


