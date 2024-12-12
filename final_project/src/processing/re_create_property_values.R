library(readr)
library(stringr)  
library(httr2) 
library(dplyr)

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

base_joined_2 <- bind_rows(base_property_info_1, base_property_info_2)

for (i in seq(1,length(csv_files),by=2)){
  number <- sub(".*\\/([0-9]+)_.+", "\\1", csv_files[i])
  col_name_C_F_T <- paste("CUR_FV_T_VALUE_", number, sep = "_")
  col_name_C_F_L <- paste("CUR_FV_L_VALUE_", number, sep = "_")
  col_name_N_F_L <- paste("NEW_FV_L_VALUE_", number, sep = "_")
  col_name_N_F_T <- paste("NEW_FV_T_VALUE_", number, sep = "_")
  first <- read.csv(csv_files[i])
  second <- read.csv(csv_files[i + 1])
  first_t <- first |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3) |>
    filter(ZIP %in% c(11215, 11218)) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name_C_F_T) := CUR_FV_T) |>
    rename(!!sym(col_name_C_F_L) := CUR_FV_L) |>
    rename(!!sym(col_name_N_F_L) := NEW_FV_L) |>
    rename(!!sym(col_name_N_F_T) := NEW_FV_T) |>
    filter(ZIP %in% c(11215, 11218)) |>
    select("ID",!!sym(col_name_N_F_L),!!sym(col_name_N_F_T), !!sym(col_name_C_F_L), !!sym(col_name_C_F_T))

  second_t <- second |>
    mutate(BORO = as.numeric(BORO)) |>
    filter(BORO == 3, TXCL %in% c("2","2A", "2B","2C")) |>
    filter(ZIP %in% c(11215, 11218)) |>
    mutate(ID = paste(BLOCK, LOT, sep = "-")) |>
    rename(!!sym(col_name_C_F_T) := CUR_FV_T) |>
    rename(!!sym(col_name_C_F_L) := CUR_FV_L) |>
    rename(!!sym(col_name_N_F_L) := NEW_FV_L) |>
    rename(!!sym(col_name_N_F_T) := NEW_FV_T) |>
    select("ID",!!sym(col_name_N_F_L),!!sym(col_name_N_F_T), !!sym(col_name_C_F_L), !!sym(col_name_C_F_T))
  
  joined <- bind_rows(second_t, first_t)
  base_joined_2 <- base_joined_2 |>
    right_join(joined, by="ID")
}

base_joined_3 <- base_joined_2 |> 
  select(
    ID, BLOCK, LOT, CUR_FV_L_VALUE__10, CUR_FV_L_VALUE__11, CUR_FV_L_VALUE__12, CUR_FV_L_VALUE__13, CUR_FV_L_VALUE__14, CUR_FV_L_VALUE__15, CUR_FV_L_VALUE__16, CUR_FV_L_VALUE__17, CUR_FV_L_VALUE__18, CUR_FV_L_VALUE__19,
    CUR_FV_T_VALUE__10, CUR_FV_T_VALUE__11, CUR_FV_T_VALUE__12, CUR_FV_T_VALUE__13, CUR_FV_T_VALUE__14, CUR_FV_T_VALUE__15, CUR_FV_T_VALUE__16, CUR_FV_T_VALUE__17, CUR_FV_T_VALUE__18, CUR_FV_T_VALUE__19,
    NEW_FV_L_VALUE__10, NEW_FV_L_VALUE__11, NEW_FV_L_VALUE__12, NEW_FV_L_VALUE__13, NEW_FV_L_VALUE__14, NEW_FV_L_VALUE__15, NEW_FV_L_VALUE__16, NEW_FV_L_VALUE__17, NEW_FV_L_VALUE__18, NEW_FV_L_VALUE__19,
    NEW_FV_T_VALUE__10, NEW_FV_T_VALUE__11, NEW_FV_T_VALUE__12, NEW_FV_T_VALUE__13, NEW_FV_T_VALUE__14, NEW_FV_T_VALUE__15, NEW_FV_T_VALUE__16, NEW_FV_T_VALUE__17, NEW_FV_T_VALUE__18, NEW_FV_T_VALUE__19, HNUM_LO, STR_NAME 
  ) 


write.csv(x = base_joined_3 , "final_project/data/finance/2010_2019_park_slope_values-CHECK-DESIGNATIONS.csv")
