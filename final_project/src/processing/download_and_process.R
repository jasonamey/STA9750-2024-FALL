library(readr)
library(stringr)  
library(httr2) 
library(dplyr)

# Identify urls scraped from NYC Dept of Finance page
urls <- read.csv("final_project/data/tax_class_urls.csv", header = TRUE)[, 1]

# Cycle through all urls to download 
for (i in 1:length(urls)){
  url <- urls[i]

  temp_file <- tempfile(fileext = ".zip")
  temp_dir <- "final_project/data/temp"

  response <- request(url) |>
    req_perform()
 
  writeBin(resp_body_raw(response), temp_file)
  
  unzip(temp_file, exdir = temp_dir)

  # Add a sleep to avoid hitting rate limits
  Sys.sleep(1) 
}


PARENT_DIR <- "final_project/data/temp"

for (i in 9:23) {
  dir_name <- paste(PARENT_DIR, i, sep = "/") 
  dir.create(dir_name, showWarnings = FALSE)  
  print(paste("Created directory:", dir_name))  
}

FILES <- list.files("final_project/data/temp",  pattern = "\\.zip$", full.names = TRUE)

for (i in 1:length(FILES)){
  
  number <- sub(".*_(\\d+)\\.zip$", "\\1", FILES[i])
  
  # Extract the number from the file name
  number <- sub(".*_(\\d+)\\.zip$", "\\1", FILES[i])
  
  # New Directory
  dir_name <- paste0("final_project/data/temp/", number)
  
  # New file path
  new_file_path <- file.path(dir_name, basename(FILES[i]))
  
  # Move the file to the corresponding directory
  file.rename(FILES[i], new_file_path)

}


BASE_DIR <- "final_project/data/temp"

# List subdirectories in the base directory
folders <- list.dirs(BASE_DIR, full.names = TRUE, recursive = FALSE)

# Loop through each folder
for (folder in folders) {
  
  # Skip if the folder name is not numeric (i.e., avoid the 'extracted' folder)
  folder_name <- basename(folder)
  if (!grepl("^\\d+$", folder_name)) {
    next
  }
  
  # List .zip files in the folder
  zip_files <- list.files(folder, pattern = "\\.zip$", full.names = TRUE)
  
  # Extract and rename files
  for (zip_file in zip_files) {
    
    # Create a subfolder named "extracted" within the current folder 
    extracted_dir <- file.path(folder, "extracted")
    if (!dir.exists(extracted_dir)) {
      dir.create(extracted_dir)
    }
    
    # Unzip the file into the 'extracted' directory within the current folder
    unzip(zip_file, exdir = extracted_dir)
    
    # List all files extracted in the 'extracted' folder
    extracted_files <- list.files(extracted_dir, full.names = TRUE)
    
    # Rename each extracted file by adding the folder name to the file
    for (extracted_file in extracted_files) {
      
      new_name <- paste0(folder_name, "_", basename(extracted_file))
      new_path <- file.path(extracted_dir, new_name)
      
      file.rename(extracted_file, new_path)
    }
  }
}

# Define the base directory
BASE_DIR <- "final_project/data/temp"

# List all subdirectories (recursive) and find those named 'extracted'
extracted_folders <- list.dirs(BASE_DIR, full.names = TRUE, recursive = TRUE)
extracted_folders <- extracted_folders[grepl("extracted$", extracted_folders)]

# Loop through each 'extracted' folder
for (folder in extracted_folders) {
  
  # List all files in the 'extracted' folder
  extracted_files <- list.files(folder, full.names = TRUE)
  
  # Move each file to the 'temp' directory
  for (file in extracted_files) {
    # Define the new location in the 'temp' directory
    new_location <- file.path(BASE_DIR, basename(file))
    
    # Move the file
    file.rename(file, new_location)
  }
  
  # Remove the now-empty 'extracted' folder
  unlink(folder, recursive = TRUE)
}

# Define the base directory
BASE_DIR <- "final_project/data/temp"

# List all items in the directory 
all_items <- list.files(BASE_DIR, full.names = TRUE)

# Filter for directories 
folders <- all_items[file.info(all_items)$isdir]

# List all .mdb and .txt files
files_to_keep <- list.files(BASE_DIR, pattern = "\\.(mdb|txt)$", full.names = TRUE)

# Loop through each folder and delete it if it's not a .mdb or .txt file
for (folder in folders) {
  if (!(folder %in% files_to_keep)) {
    unlink(folder, recursive = TRUE)
  }
}

# All the mdb files were converted to .csv files i.e : 
# MacBook-Pro temp % mdb-tables 16_16_tc1.mdb
# NameTable tc1 
# MacBook-Pro temp % mdb-export 16_16_tc1.mdb tc1 > 16_TC1.csv

# Remove all .mdb files
mdb_dir <- "final_project/data/temp/mdb"
dir.create(mdb_dir)

mdb_files <- list.files("final_project/data/temp", pattern = "\\.mdb$", full.names = TRUE)

# Move each .mdb file to the 'mdb' directory
for (file in mdb_files) {
  file.rename(file, file.path(mdb_dir, basename(file)))
}

finance_dir <- "final_project/data/finance"
dir.create(finance_dir)

csv_files <- list.files("final_project/data/temp", pattern = "\\.csv$", full.names = TRUE)

base_property_info <- read_csv(csv_files[1]) |> 
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3, TXCL %in% c("1", "2")) |> 
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI) 

base_property_info_1 <- read_csv(csv_files[1]) |> 
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3) |> 
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI) 

base_property_info_2 <- read_csv(csv_files[2]) |> 
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3, TXCL %in% c("2","2A", "2B","2C")) |> 
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI) 

base_joined <- bind_rows(base_property_info_1, base_property_info_2)

for (i in seq(1,length(csv_files),by=2)){
  number <- sub(".*\\/([0-9]+)_.+", "\\1", csv_files[i])
  col_name <- paste("VALUE", number, sep = "_")
  first <- read.csv(csv_files[i])
  second <- read.csv(csv_files[i + 1])
  first_t <- first |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name) := NEW_FV_T) |>
    select(ID, !!sym(col_name))

  second_t <- second |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3, TXCL %in% c("2","2A", "2B","2C")) |> 
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name) := NEW_FV_T) |>
    select(ID, !!sym(col_name))

  joined <- bind_rows(second_t, first_t)
  base_joined <- base_joined |>
    right_join(joined, by="ID")
}


# zip codes for Park Slope and Windsor Terrace 
park_slope_properties <- base_joined |>
  filter(ZIP %in% c(11215, 11218))

write.csv(x = park_slope_properties , "final_project/data/finance/2010_2019_park_slope_values.csv")

txt_files <- list.files("final_project/data/temp", pattern = "\\.(txt|TXT)$", full.names = TRUE)


# copy base_joined file above, add years 2020, 2021, 2022, 2023
base_joined_copy <- base_joined


for (i in seq(1, length(txt_files), by=2)){
  number <- sub(".*/(\\d+)_.*", "\\1", txt_files[i])
  col_name <- paste("VALUE", number, sep = "_")
  
  first <- readLines(txt_files[i])
  first_records <- strsplit(first, "\n")
  first_parsed <- lapply(first_records, function(record) strsplit(record, "\t"))
  first_rows <- do.call(rbind, lapply(first_parsed, function(x) unlist(x)))
  first_df <- as.data.frame(first_rows)

  second <- readLines(txt_files[i + 1])
  second_records <- strsplit(first, "\n")
  second_parsed <- lapply(second_records, function(record) strsplit(record, "\t"))
  second_rows <- do.call(rbind, lapply(second_parsed, function(x) unlist(x)))
  second_df <- as.data.frame(second_rows)

  first_t <- first_df |>
    mutate(V3 = as.numeric(V3), V4 = as.numeric(V4), V25 = as.numeric(V25)) |>
    mutate(V78 = trimws(V78)) |>
    filter(V78 %in% c("11215", "11218")) |>
    mutate(V34 = trimws(V34)) |>
    filter(V2 == "3", V34 %in% c("1","1A","1B","1C","1D","2","2A","2B","2C")) |> 
    mutate(ID = paste(V3, V4, sep = "-")) |>
    rename(!!sym(col_name) := V25)  
  
  second_t <- second_df |>
    mutate(V3 = as.numeric(V3), V4 = as.numeric(V4), V25 = as.numeric(V25)) |>
    mutate(V78 = trimws(V78)) |>
    filter(V78 %in% c("11215", "11218")) |>
    mutate(V34 = trimws(V34)) |>
    filter(V2 == "3", V34 %in% c("1","1A","1B","1C","1D","2","2A","2B","2C")) |> 
    mutate(ID = paste(V3, V4, sep = "-")) |>
    rename(!!sym(col_name) := V25)
  
  joined <- bind_rows(second_t, first_t)
  
  joined_t <- joined |>
    select(ID, starts_with("VALUE"))
  
  base_joined_copy <- joined_t |> 
    left_join(base_joined_copy, by="ID")
}
  
clean <- base_joined_copy |>
  distinct()

# Save final dataset of Park Slope properties
write.csv(x = clean , "final_project/data/finance/2010_2023_park_slope_values.csv")




##############START OF RE-CHECK########################

base_property_info_1 <- read_csv(csv_files[1]) |> 
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3) |> 
  mutate(LOT = trimws(LOT) ) |>
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  filter(ZIP %in% c(11215, 11218)) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI) 

base_property_info_2 <- read_csv(csv_files[2]) |> 
  mutate(BORO = as.numeric(BORO)) |>
  filter(BORO == 3, TXCL %in% c("2","2A", "2B","2C")) |> 
  mutate(LOT = trimws(LOT) ) |>
  mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
  filter(ZIP %in% c(11215, 11218)) |>
  select(ID, BORO, BLOCK, LOT, TXCL, ZIP, YRB, TOT_UNIT, RES_UNIT, STR_NAME, BLDGCL, HNUM_LO, HNUM_HI) 

base_joined <- bind_rows(base_property_info_1, base_property_info_2)

reference_base_2010_2019 <- base_joined 
 
for (i in seq(1,length(csv_files),by=2)){
  number <- sub(".*\\/([0-9]+)_.+", "\\1", csv_files[i])
  col_name <- paste("VALUE", number, sep = "_")
  first <- read.csv(csv_files[i])
  second <- read.csv(csv_files[i + 1])
  first_t <- first |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3) |>
    mutate(LOT = trimws(LOT) ) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name) := NEW_FV_T) |>
    filter(ZIP %in% c(11215, 11218)) |>
    select(ID, !!sym(col_name))
  
  second_t <- second |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3, TXCL %in% c("2","2A", "2B","2C")) |> 
    mutate(LOT = trimws(LOT) ) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name) := NEW_FV_T) |>
    filter(ZIP %in% c(11215, 11218)) |>
    select(ID, !!sym(col_name))
  
  joined <- bind_rows(second_t, first_t)
  
  reference_base_2010_2019 <- reference_base_2010_2019 |>
    left_join(joined, by="ID")
}

reference_base_copy <- reference_base_2010_2019

txt_files <- list.files("final_project/data/temp", pattern = "\\.(txt|TXT)$", full.names = TRUE)

for (i in seq(1,length(csv_files),by=2)){
  #change to i 
  number <- sub(".*/(\\d+)_.*", "\\1", txt_files[i])
  
  col_name <- paste("VALUE", number, sep = "_")
  
  #change to i 
  first <- readLines(txt_files[i])
  first_records <- strsplit(first, "\n")
  first_parsed <- lapply(first_records, function(record) strsplit(record, "\t"))
  first_rows <- do.call(rbind, lapply(first_parsed, function(x) unlist(x)))
  first_df <- as.data.frame(first_rows)
  
  #change to i 
  second <- readLines(txt_files[i+1])
  second_records <- strsplit(second, "\n")
  second_parsed <- lapply(second_records, function(record) strsplit(record, "\t"))
  second_rows <- do.call(rbind, lapply(second_parsed, function(x) unlist(x)))
  second_df <- as.data.frame(second_rows)
  
  nrow(first_df)
  nrow(second_df)
  
  first_t <- first_df |>
    mutate(V3 = as.numeric(V3), V4 = as.numeric(V4), V25 = as.numeric(V25)) |>
    mutate(V78 = trimws(V78)) |>
    filter(V78 %in% c("11215", "11218")) |>
    mutate(V34 = trimws(V34)) |>
    filter(V2 == "3", V34 %in% c("1","1A","1B","1C","1D","2","2A","2B","2C")) |> 
    mutate(ID = paste(V3, V4, sep = "-")) |>
    rename(!!sym(col_name) := V25) |> 
    select(ID, !!sym(col_name))
  
  second_t <- second_df |>
    mutate(V3 = as.numeric(V3), V4 = as.numeric(V4), V25 = as.numeric(V25)) |>
    mutate(V78 = trimws(V78)) |>
    filter(V78 %in% c("11215", "11218")) |>
    mutate(V34 = trimws(V34)) |>
    filter(V2 == "3", V34 %in% c("1","1A","1B","1C","1D","2","2A","2B","2C")) |> 
    mutate(ID = paste(V3, V4, sep = "-")) |>
    rename(!!sym(col_name) := V25) |>
    select(ID, !!sym(col_name))
  
  joined_2020_2023 <- bind_rows(second_t, first_t)
  
  reference_base_copy <- reference_base_copy |>
    inner_join(joined_2020_2023, by="ID")

}

print("hello")

# No understanding why, but this code is creating duplicates of ID "874-68" 
reference_base_copy_t <- reference_base_copy |> 
  filter(ID != "874-68")

View(reference_base_copy_t)

write.csv(x = reference_base_copy_t , "final_project/data/finance/2010_2023_park_slope_values.csv")

###############################CHECKS##############################

# PART 1
#### CHECKING DATA AGAINST .SHP FILES #####
base_joined <- bind_rows(base_property_info_1, base_property_info_2)

# IDs for all the properties in the point-and-click .shp files 
big_str <- "1065-31,1065-37,1066-1,1066-2,1066-4,1066-5,1066-6,1066-8,1066-9,1067-39,1067-43,1067-45,1068-3,1068-4,1068-5,1068-8,1068-7502,1071-4,1071-14,1071-26,1071-27,1071-28,1071-30,1072-5,1072-6,1072-7501,1072-7503,1073-1,1073-5,1074-38,1074-39,1074-42,1075-1,1075-5,1076-37,1076-38,1076-39,1076-40,1076-41,1076-42,1076-46,1077-1,1077-3,1077-4,1077-5,1077-7,1077-9,1078-37,1079-1,1079-5,1079-7,1079-7501,1080-35,1080-41,1080-43,1081-1,1081-3,1081-5,1082-37,1082-39,1082-7502,1083-5,1084-39,1085-1,1085-2,1085-3,1085-4,1085-6,1085-7,1085-8,1085-10,1085-7502,1086-1,1087-5,1087-6,1087-7,1088-35,1088-37,1088-39,1088-41,1089-1,1089-7,1090-35,1090-37,1090-38,1090-40,1090-42,1091-1,1091-2,1091-3,1091-4,1091-6,1091-7,1091-8,1091-7501,1092-38,1092-40,1092-41,1092-42,1092-7501,1092-7502,1093-1,1093-74,1094-41,1094-48,1094-7501,1095-1,1095-3,1095-5,1095-7,1095-7501,1095-7503,1096-38,1096-39,1096-41,1097-1,1097-2,1097-4,1097-5,1097-8,1099-1,1099-2,1099-3,1099-4,1099-6,1099-7,1099-8,1099-9,1100-44,1101-1,1102-12,1103-1,1103-7501,1106-5,1106-14,1106-16,1106-19,1106-21,1106-33,1107-5,1107-9,1107-21,1107-27,1107-33,5257-45,5258-7501,5259-6,5259-7501,5268-84,5268-87,5268-89,5268-90,5268-94,5275-7501,5279-19,5279-32,5279-43,5287-35,5287-44,5287-7501,1066-35,1068-37,1072-35,1072-40,1073-41,1075-35,1075-39,1077-32,1077-39,1079-40,1081-38,1081-43,1083-38,1083-43,1083-45,1085-37,1085-40,1085-41,1085-42,1085-44,1085-45,1085-7505,1087-38,1087-45,1087-46,1087-7501,1087-7502,1089-33,1089-35,1089-36,1089-38,1089-40,1091-35,1091-40,1091-42,1093-38,1093-7501,1093-7502,1095-40,1095-42,1095-44,1095-46,1095-47,1095-48,1095-50,1095-7502,1097-46,1097-7502,1099-37,1099-40,1099-42,1099-44,1099-46,1101-38,1101-40,1101-41,1101-42,1101-43,1101-44,1101-46,1103-37,1103-42"

str_vec_shp_IDs <- unlist(strsplit(big_str, split=","))
found_in_prop_data <- str_vec_shp_IDs %in% base_joined$ID

length(str_vec_shp_IDs[!found_in_prop_data])
#### THERE ARE 29 OF ~330 PROPERTIES IN .SHP FILES NOT IN DEPT OF FINANCE DATA ####


# PART 2 
# IDs for all the properties in the point-and-click .shp files 
big_str <- "1065-31,1065-37,1066-1,1066-2,1066-4,1066-5,1066-6,1066-8,1066-9,1067-39,1067-43,1067-45,1068-3,1068-4,1068-5,1068-8,1068-7502,1071-4,1071-14,1071-26,1071-27,1071-28,1071-30,1072-5,1072-6,1072-7501,1072-7503,1073-1,1073-5,1074-38,1074-39,1074-42,1075-1,1075-5,1076-37,1076-38,1076-39,1076-40,1076-41,1076-42,1076-46,1077-1,1077-3,1077-4,1077-5,1077-7,1077-9,1078-37,1079-1,1079-5,1079-7,1079-7501,1080-35,1080-41,1080-43,1081-1,1081-3,1081-5,1082-37,1082-39,1082-7502,1083-5,1084-39,1085-1,1085-2,1085-3,1085-4,1085-6,1085-7,1085-8,1085-10,1085-7502,1086-1,1087-5,1087-6,1087-7,1088-35,1088-37,1088-39,1088-41,1089-1,1089-7,1090-35,1090-37,1090-38,1090-40,1090-42,1091-1,1091-2,1091-3,1091-4,1091-6,1091-7,1091-8,1091-7501,1092-38,1092-40,1092-41,1092-42,1092-7501,1092-7502,1093-1,1093-74,1094-41,1094-48,1094-7501,1095-1,1095-3,1095-5,1095-7,1095-7501,1095-7503,1096-38,1096-39,1096-41,1097-1,1097-2,1097-4,1097-5,1097-8,1099-1,1099-2,1099-3,1099-4,1099-6,1099-7,1099-8,1099-9,1100-44,1101-1,1102-12,1103-1,1103-7501,1106-5,1106-14,1106-16,1106-19,1106-21,1106-33,1107-5,1107-9,1107-21,1107-27,1107-33,5257-45,5258-7501,5259-6,5259-7501,5268-84,5268-87,5268-89,5268-90,5268-94,5275-7501,5279-19,5279-32,5279-43,5287-35,5287-44,5287-7501,1066-35,1068-37,1072-35,1072-40,1073-41,1075-35,1075-39,1077-32,1077-39,1079-40,1081-38,1081-43,1083-38,1083-43,1083-45,1085-37,1085-40,1085-41,1085-42,1085-44,1085-45,1085-7505,1087-38,1087-45,1087-46,1087-7501,1087-7502,1089-33,1089-35,1089-36,1089-38,1089-40,1091-35,1091-40,1091-42,1093-38,1093-7501,1093-7502,1095-40,1095-42,1095-44,1095-46,1095-47,1095-48,1095-50,1095-7502,1097-46,1097-7502,1099-37,1099-40,1099-42,1099-44,1099-46,1101-38,1101-40,1101-41,1101-42,1101-43,1101-44,1101-46,1103-37,1103-42"

str_vec_shp_IDs <- unlist(strsplit(big_str, split=","))
found_in_prop_data <- str_vec_shp_IDs %in% reference_base_2010_2019$ID

length(str_vec_shp_IDs[!found_in_prop_data])
###################

# PART 3
# IDs for all the properties in the point-and-click .shp files 

big_str <- "1065-31,1065-37,1066-1,1066-2,1066-4,1066-5,1066-6,1066-8,1066-9,1067-39,1067-43,1067-45,1068-3,1068-4,1068-5,1068-8,1068-7502,1071-4,1071-14,1071-26,1071-27,1071-28,1071-30,1072-5,1072-6,1072-7501,1072-7503,1073-1,1073-5,1074-38,1074-39,1074-42,1075-1,1075-5,1076-37,1076-38,1076-39,1076-40,1076-41,1076-42,1076-46,1077-1,1077-3,1077-4,1077-5,1077-7,1077-9,1078-37,1079-1,1079-5,1079-7,1079-7501,1080-35,1080-41,1080-43,1081-1,1081-3,1081-5,1082-37,1082-39,1082-7502,1083-5,1084-39,1085-1,1085-2,1085-3,1085-4,1085-6,1085-7,1085-8,1085-10,1085-7502,1086-1,1087-5,1087-6,1087-7,1088-35,1088-37,1088-39,1088-41,1089-1,1089-7,1090-35,1090-37,1090-38,1090-40,1090-42,1091-1,1091-2,1091-3,1091-4,1091-6,1091-7,1091-8,1091-7501,1092-38,1092-40,1092-41,1092-42,1092-7501,1092-7502,1093-1,1093-74,1094-41,1094-48,1094-7501,1095-1,1095-3,1095-5,1095-7,1095-7501,1095-7503,1096-38,1096-39,1096-41,1097-1,1097-2,1097-4,1097-5,1097-8,1099-1,1099-2,1099-3,1099-4,1099-6,1099-7,1099-8,1099-9,1100-44,1101-1,1102-12,1103-1,1103-7501,1106-5,1106-14,1106-16,1106-19,1106-21,1106-33,1107-5,1107-9,1107-21,1107-27,1107-33,5257-45,5258-7501,5259-6,5259-7501,5268-84,5268-87,5268-89,5268-90,5268-94,5275-7501,5279-19,5279-32,5279-43,5287-35,5287-44,5287-7501,1066-35,1068-37,1072-35,1072-40,1073-41,1075-35,1075-39,1077-32,1077-39,1079-40,1081-38,1081-43,1083-38,1083-43,1083-45,1085-37,1085-40,1085-41,1085-42,1085-44,1085-45,1085-7505,1087-38,1087-45,1087-46,1087-7501,1087-7502,1089-33,1089-35,1089-36,1089-38,1089-40,1091-35,1091-40,1091-42,1093-38,1093-7501,1093-7502,1095-40,1095-42,1095-44,1095-46,1095-47,1095-48,1095-50,1095-7502,1097-46,1097-7502,1099-37,1099-40,1099-42,1099-44,1099-46,1101-38,1101-40,1101-41,1101-42,1101-43,1101-44,1101-46,1103-37,1103-42"

str_vec_shp_IDs <- unlist(strsplit(big_str, split=","))

found_in_prop_data <- str_vec_shp_IDs %in% joined_2020_2023$ID
length(str_vec_shp_IDs[!found_in_prop_data])

###################

# PART 4 

big_str <- "1065-31,1065-37,1066-1,1066-2,1066-4,1066-5,1066-6,1066-8,1066-9,1067-39,1067-43,1067-45,1068-3,1068-4,1068-5,1068-8,1068-7502,1071-4,1071-14,1071-26,1071-27,1071-28,1071-30,1072-5,1072-6,1072-7501,1072-7503,1073-1,1073-5,1074-38,1074-39,1074-42,1075-1,1075-5,1076-37,1076-38,1076-39,1076-40,1076-41,1076-42,1076-46,1077-1,1077-3,1077-4,1077-5,1077-7,1077-9,1078-37,1079-1,1079-5,1079-7,1079-7501,1080-35,1080-41,1080-43,1081-1,1081-3,1081-5,1082-37,1082-39,1082-7502,1083-5,1084-39,1085-1,1085-2,1085-3,1085-4,1085-6,1085-7,1085-8,1085-10,1085-7502,1086-1,1087-5,1087-6,1087-7,1088-35,1088-37,1088-39,1088-41,1089-1,1089-7,1090-35,1090-37,1090-38,1090-40,1090-42,1091-1,1091-2,1091-3,1091-4,1091-6,1091-7,1091-8,1091-7501,1092-38,1092-40,1092-41,1092-42,1092-7501,1092-7502,1093-1,1093-74,1094-41,1094-48,1094-7501,1095-1,1095-3,1095-5,1095-7,1095-7501,1095-7503,1096-38,1096-39,1096-41,1097-1,1097-2,1097-4,1097-5,1097-8,1099-1,1099-2,1099-3,1099-4,1099-6,1099-7,1099-8,1099-9,1100-44,1101-1,1102-12,1103-1,1103-7501,1106-5,1106-14,1106-16,1106-19,1106-21,1106-33,1107-5,1107-9,1107-21,1107-27,1107-33,5257-45,5258-7501,5259-6,5259-7501,5268-84,5268-87,5268-89,5268-90,5268-94,5275-7501,5279-19,5279-32,5279-43,5287-35,5287-44,5287-7501,1066-35,1068-37,1072-35,1072-40,1073-41,1075-35,1075-39,1077-32,1077-39,1079-40,1081-38,1081-43,1083-38,1083-43,1083-45,1085-37,1085-40,1085-41,1085-42,1085-44,1085-45,1085-7505,1087-38,1087-45,1087-46,1087-7501,1087-7502,1089-33,1089-35,1089-36,1089-38,1089-40,1091-35,1091-40,1091-42,1093-38,1093-7501,1093-7502,1095-40,1095-42,1095-44,1095-46,1095-47,1095-48,1095-50,1095-7502,1097-46,1097-7502,1099-37,1099-40,1099-42,1099-44,1099-46,1101-38,1101-40,1101-41,1101-42,1101-43,1101-44,1101-46,1103-37,1103-42"

str_vec_shp_IDs <- unlist(strsplit(big_str, split=","))

found_in_prop_data <- str_vec_shp_IDs %in% reference_base_copy$ID

length(str_vec_shp_IDs[!found_in_prop_data])

test <- reference_base_copy |> 
  group_by(ID) |> 
  summarize(count = n()) 
dublicate_IDs <- test |> 
  filter(count > 1 )

dublicate_IDs