################################################################################
# APPEND DATASETS 
# desc: conducts preliminary transformations of datasets to allow append
################################################################################

# Load packages
library(data.table)
library(readxl)
library(haven)
library(dplyr)
library(readr)

################################################################################
# FUNCTIONS to load and analyse variables

# Function to read files and handle labelled columns
read_file_and_print_types <- function(file, inputcols) {
  ext <- tolower(tools::file_ext(file))
  df <- NULL
  
  # Read file based on its extension
  tryCatch({
    if (ext == "csv") {
      df <- fread(file, fill = TRUE, showProgress = FALSE, data.table = FALSE)
    } else if (ext == "xlsx") {
      df <- as.data.frame(read_excel(file))
    } else if (ext == "sav") {
      df <- as.data.frame(read_sav(file))
    } else if (ext == "dta") {
      df <- as.data.frame(read_dta(file))
    }
  }, error = function(e) {
    message("Error reading file: ", file, " - ", e)
  })
  
  if (!is.null(df)) {
    # Transform column names to lowercase
    colnames(df) <- tolower(colnames(df))
    
    # Subset dataframe to include only desired columns
    df <- df[, intersect(inputcols, colnames(df)), drop = FALSE]
    
    # Create type information for each column in inputcols
    type_info <- sapply(inputcols, function(col) {
      if (col %in% colnames(df)) {
        col_data <- df[[col]]
        if (inherits(col_data, "haven_labelled")) {
          col_type <- class(as_factor(col_data))  # Preserve labels as factor
        } else {
          col_type <- class(col_data)  # Use original class
        }
        
        if (all(is.na(col_data))) {
          return("NA")  # If the entire column is NA
        } else {
          return(col_type)
        }
      } else {
        return(".")  # Column not present
      }
    })
  } else {
    type_info <- rep(".", length(inputcols))  # If file couldn't be read
  }
  
  return(type_info)
}

# Function to loop through files and create dataframes
process_folder <- function(folder_path, inputcols) {
  files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  
  combined_data <- list()  # To store data from all files
  type_rows <- list()      # To store datatype information for each file
  
  for (file in files) {
    message("Processing: ", file)
    
    # Read and process file
    file_data <- tryCatch({
      ext <- tolower(tools::file_ext(file))
      if (ext == "csv") {
        fread(file, fill = TRUE, showProgress = FALSE, data.table = FALSE)
      } else if (ext == "xlsx") {
        as.data.frame(read_excel(file))
      } else if (ext == "sav") {
        as.data.frame(read_sav(file))
      } else if (ext == "dta") {
        as.data.frame(read_dta(file))
      } else {
        NULL
      }
    }, error = function(e) {
      message("Error reading file: ", e)
      NULL
    })
    
    if (!is.null(file_data)) {
      colnames(file_data) <- tolower(colnames(file_data))
      
      # Convert haven_labelled columns to factors without forcing numeric
      file_data <- as.data.frame(lapply(file_data, function(col) {
        if (inherits(col, "haven_labelled")) {
          as_factor(col)  # Convert labelled columns to factors
        } else {
          col  # Keep other types unchanged
        }
      }))
      
      # Ensure the data frame has all the desired columns, fill missing columns with NA
      missing_cols <- setdiff(inputcols, colnames(file_data))
      if (length(missing_cols) > 0) {
        file_data[missing_cols] <- NA
      }
      
      # Subset to the exact desired columns order
      file_data <- file_data[, inputcols, drop = FALSE]
      
      combined_data[[file]] <- file_data
    }
    
    # Get datatype info and indicate missing columns with "NA"
    type_info <- sapply(inputcols, function(col) {
      if (!is.null(file_data) && col %in% colnames(file_data)) {
        col_data <- file_data[[col]]
        if (all(is.na(col_data))) {
          return("NA")  # Entire column is NA in the file
        } else {
          return(class(col_data))  # Return the class of the column
        }
      } else {
        return("NA")  # Column not present in the file
      }
    })
    
    type_rows[[file]] <- type_info
  }
  
  # Combine all files into one dataframe
  df_combined <- do.call(rbind, combined_data)
  
  # Create dataframe of datatypes for each file
  df_types <- as.data.frame(do.call(rbind, type_rows))
  colnames(df_types) <- inputcols
  rownames(df_types) <- basename(names(type_rows))
  
  return(list(df_combined = df_combined, df_types = df_types))
}

################################################################################
# Set input columns
inputcols <- c("numos", "okres", "byt", "so", "cisbytu", "cos", "reftrok", "refrok", 
                 "ctvrtleti", "ctvrtlet", "reftyd", "obvhod", "nejvzds", "isced", 
                 "vek", "zampost", "zamnace", "rodstav", "pohl", "prac1h", "prac0h",
                 "typsml", "zamiscot", "zamisco", "wgtos", "wgtdom")

################################################################################
# 2008-2012
folder_path <- file.path(CSUDATA,"2008-2012")

result0812 <- process_folder(folder_path, inputcols)

# Access combined dataframe and type dataframe
df0812 <- result0812$df_combined
df0812_check <- result0812$df_types

# Create hhnum and persnum
df0812$hhnum <- paste(df0812$okres, df0812$so, df0812$cisbytu, sep = "_")
df0812$persnum <- paste(df0812$okres, df0812$so, df0812$cisbytu, df0812$cos, sep = "_")

# Create year and quarter
df0812 <- df0812 %>%
  mutate(
    year = coalesce(reftrok, refrok),  # Takes the first non-NA value
    quarter = coalesce(ctvrtlet, ctvrtleti)  # Takes the first non-NA value
  )

#View(df0812)
#View(df0812_check)

################################################################################
# 2013-2016 (wo 2014)
folder_path <- file.path(CSUDATA,"2013-2016")

result1316 <- process_folder(folder_path, inputcols)

# Access combined dataframe and type dataframe
df1316 <- result1316$df_combined
df1316_check <- result1316$df_types

# Create hhnum and persnum
df1316$hhnum <- paste(df1316$okres, df1316$so, df1316$cisbytu, sep = "_")
df1316$persnum <- paste(df1316$okres, df1316$so, df1316$cisbytu, df1316$cos, sep = "_")

# Create year and quarter
df1316 <- df1316 %>%
  mutate(
    year = coalesce(reftrok, refrok),  # Takes the first non-NA value
    quarter = coalesce(ctvrtlet, ctvrtleti)  # Takes the first non-NA value
  )

#View(df1316)
#View(df1316_check)

################################################################################
# 2014
folder_path <- file.path(CSUDATA,"2014")

result14 <- process_folder(folder_path, inputcols)

# Access combined dataframe and type dataframe
df14 <- result14$df_combined
df14_check <- result14$df_types

# Transform okres from text to NUTS4
ciselnikokresu <- read_excel("N:/MT/data/csu/CiselnikOkresu.xlsx")

df14 <- df14 %>%
    mutate(okres_text = as.character(okres))

df14 <- df14 %>%
      left_join(ciselnikokresu, by = c("okres" = "okres")) %>%
      mutate(okres = ifelse(!is.na(NUTS4), NUTS4, okres)) %>%  # Replace okres with NUTS4 if available
      select(-NUTS4)

# Create hhnum and persnum
df14$hhnum <- paste(df14$okres, df14$byt, sep = "_")
df14$persnum <- paste(df14$okres, df14$byt, df14$cos, sep = "_")

# Create year and quarter
df14 <- df14 %>%
  mutate(
    year = coalesce(reftrok, refrok),  # Takes the first non-NA value
    quarter = coalesce(ctvrtlet, ctvrtleti)  # Takes the first non-NA value
  )

#View(df14)
#View(df14_check)

################################################################################
# 2017-2019
folder_path <- file.path(CSUDATA,"2017-2019")

result1719 <- process_folder(folder_path, inputcols)

# Access combined dataframe and type dataframe
df1719 <- result1719$df_combined
df1719_check <- result1719$df_types

# Create hhnum and persnum
df1719 <- df1719 %>%
  mutate(hhnum = substr(numos, 1, nchar(numos) - 2))
df1719$persnum <- df1719$numos

# Create year and quarter
df1719 <- df1719 %>%
  mutate(
    year = coalesce(reftrok, refrok),  # Takes the first non-NA value
    quarter = coalesce(ctvrtlet, ctvrtleti)  # Takes the first non-NA value
  )

#View(df1719)
#View(df1719_check)

################################################################################
# 2020-2021
folder_path <- file.path(CSUDATA,"2020-2021")

result2021 <- process_folder(folder_path, inputcols)

# Access combined dataframe and type dataframe
df2021 <- result2021$df_combined
df2021_check <- result2021$df_types

# Create hhnum and persnum
df2021 <- df2021 %>%
  mutate(hhnum = substr(numos, 1, nchar(numos) - 2))
df2021$persnum <- df2021$numos

# Create year and quarter
df2021 <- df2021 %>%
  mutate(
    year = coalesce(reftrok, refrok),  # Takes the first non-NA value
    quarter = coalesce(ctvrtlet, ctvrtleti)  # Takes the first non-NA value
  )

#View(df2021)
#View(df2021_check)

################################################################################
# 2022-2023
folder_path <- file.path(CSUDATA,"2022-2023")

result2223 <- process_folder(folder_path, inputcols)

# Access combined dataframe and type dataframe
df2223 <- result2223$df_combined
df2223_check <- result2223$df_types

# Create hhnum and persnum
df2223 <- df2223 %>%
  mutate(hhnum = substr(as.character(numos), 1, nchar(as.character(numos)) - 2))
df2223$persnum <- df2223$numos

# Create year and quarter
df2223 <- df2223 %>%
  mutate(
    year = coalesce(reftrok, refrok),  # Takes the first non-NA value
    quarter = coalesce(ctvrtlet, ctvrtleti)  # Takes the first non-NA value
  )

# New group Spolecnik, jednatel s.r.o. (to OSVC se zamestnanci), reassign all group #
df2223 <- df2223 %>%
  mutate(
    zampost = case_when(
      zampost == 3 ~ 2,
      zampost == 4 ~ 3,
      zampost == 5 ~ 4,
      TRUE ~ zampost
    ),
    zampost = as.numeric(zampost)
  )

#View(df2223)
#View(df2223_check)

################################################################################
# FUNCTION to transform variables

# Function to apply transformations
transform_variables <- function(df) {
  df <- df %>%
    # obvhod
    mutate(
      obvhod_aux = obvhod,
      obvhod = NA_real_,
      obvhod = case_when(
        !is.na(as.numeric(trimws(obvhod_aux))) &  # Check for valid numeric values after trimming whitespace
          as.numeric(trimws(obvhod_aux)) >= 0 & 
          as.numeric(trimws(obvhod_aux)) <= 98 ~ as.numeric(trimws(obvhod_aux)),
        TRUE ~ NA_real_  # Set NA for invalid or out-of-range values
      ),
      obvhod_aux = as.character(obvhod)
    ) %>%
    # isced
    mutate(
      isced_aux = as.character(isced),
      isced = NA_real_,
      group = case_when(
        is.na(isced_aux) ~ "NA",
        grepl("^[0-9]+$", isced_aux) ~ "numeric",
        TRUE ~ "text"
      ),
      isced = case_when(
        #is.na(isced_aux) ~ NA_real_,
        group == "numeric" & as.integer(isced_aux) %in% 0:2 ~ 1,
        group == "numeric" & as.integer(isced_aux) %in% 3:4 ~ 2,
        group == "numeric" & as.integer(isced_aux) %in% 5:9 ~ 3,
        group == "text" & grepl("ISCED", isced_aux) & grepl("ISCED 0|ISCED 1,2", isced_aux) ~ 1,
        group == "text" & grepl("ISCED", isced_aux) & grepl("ISCED 3|ISCED 3,4", isced_aux) ~ 2,
        group == "text" & grepl("ISCED", isced_aux) & grepl("ISCED 5,6", isced_aux) ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-group) %>%
    # vek
    mutate(
      vek_aux = vek,
      vek = NA_real_,
      vek = case_when(
        !is.na(as.numeric(vek_aux)) & as.numeric(vek_aux) %% 1 == 0 & as.numeric(vek_aux) >= 0 ~ as.numeric(vek_aux),
        TRUE ~ NA_real_
      ),
      vek_aux = as.numeric(vek_aux)
    ) %>%
    # zampost
    mutate(
      zampost_aux = zampost,
      zampost = NA_real_,
      zampost = case_when(
        zampost_aux %in% c(1, 2, 3, 4) ~ as.numeric(zampost_aux),
        grepl("^Zam", trimws(zampost_aux)) ~ 1, # Starts with "Zam"
        grepl("družstva", zampost_aux, ignore.case = TRUE) ~ 1,  # Contains "družstva"
        grepl(" se ", zampost_aux, ignore.case = TRUE) ~ 2, # Contains " se "
        grepl(" bez ", zampost_aux, ignore.case = TRUE) ~ 3, # Contains " bez "
        grepl("rodinný", zampost_aux, ignore.case = TRUE) ~ 4, # Contains " rodinný "
        TRUE ~ NA_real_
      ),
      zampost_aux = as.character(zampost_aux)
    ) %>%
    # zamnace
    mutate(
      zamnace_aux = zamnace,
      zamnace = NA_character_,
      zamnace = case_when(
        is.na(zamnace_aux) ~ NA_character_,
        grepl("^[A-Ua-u]", zamnace_aux) ~ substr(zamnace_aux, 1, 1),
        TRUE ~ NA_character_
      ),
      zamnace_aux = as.character(zamnace_aux)
    ) %>%
    # rodstav
    mutate(
      rodstav_aux = rodstav,
      rodstav = NA_real_,
      rodstav = case_when(
        rodstav_aux %in% c(1, 2, 3, 4) ~ as.numeric(rodstav_aux),
        grepl("^Svobod", rodstav_aux) ~ 1,
        grepl("^Ženat", rodstav_aux) ~ 2,
        grepl("^Ovdov", rodstav_aux) ~ 3,
        grepl("^Rozved", rodstav_aux) ~ 4,
        TRUE ~ NA_real_
      ),
      rodstav_aux = as.character(rodstav_aux)
    ) %>%
    # pohl
    mutate(
      pohl_aux = pohl,
      pohl = NA_real_,
      pohl = case_when(
        pohl_aux %in% c(1, 2) ~ as.numeric(pohl_aux),
        grepl("^Muž", pohl_aux) ~ 1,
        grepl("^Žena", pohl_aux) ~ 2,
        TRUE ~ NA_real_
      ),
      pohl_aux = as.character(pohl_aux)
    ) %>%
    # prac1h
    mutate(
      prac1h_aux = prac1h,
      prac1h = NA_real_,
      prac1h = case_when(
        prac1h_aux %in% c(1, 2) ~ as.numeric(prac1h_aux),
        trimws(prac1h_aux) == "Ano" ~ 1,
        trimws(prac1h_aux) == "Ne" ~ 2,
        TRUE ~ NA_real_
      ),
      prac1h_aux = as.character(prac1h_aux)
    ) %>%
    # prac0h
    mutate(
      prac0h_aux = prac0h,
      prac0h = NA_real_,
      prac0h = case_when(
        prac0h_aux %in% c(1, 2) ~ as.numeric(prac0h_aux),
        trimws(prac0h_aux) == "Ano" ~ 1,
        trimws(prac0h_aux) == "Ne" ~ 2,
        TRUE ~ NA_real_
      ),
      prac0h_aux = as.character(prac0h_aux)
    ) %>%
    # typsml
    mutate(
      typsml_aux = typsml,
      typsml = NA_real_,
      typsml = case_when(
        typsml_aux == 1 ~ 1,
        typsml_aux == 2 ~ 2,
        typsml_aux %in% c(3, 4, 5) ~ 2,
        grepl("^Na dobu ur", trimws(typsml_aux)) ~ 1,
        grepl("^Na dobu neur", trimws(typsml_aux)) ~ 2,
        TRUE ~ NA_real_
      ),
      typsml_aux = as.character(typsml_aux)
    ) %>%
    #zamisco
    mutate(
      zamisco_aux = zamisco,
      zamisco = NA_character_,
      zamisco = case_when(
        is.na(zamisco_aux) ~ NA_character_,
        nchar(as.character(zamisco_aux)) == 4 ~ "0",
        grepl("^[0-9]", substr(as.character(zamisco_aux), 1, 1)) ~ substr(as.character(zamisco_aux), 1, 1),
        TRUE ~ NA_character_
      ),
      zamisco = as.integer(zamisco) 
    ) %>%
    # reftyd, wgtos, wgtdom, year, quarter
    mutate(
      reftyd = as.numeric(reftyd),
      wgtos = as.numeric(wgtos),
      wgtdom = as.numeric(wgtdom),
      year = as.numeric(year),
      quarter = as.numeric(quarter)
    )
  
  return(df)
}

################################################################################
# TRANSFORM VARIABLES
dfs <- list(df0812, df1316, df14, df1719, df2021, df2223)

dfs_transformed <- lapply(dfs, transform_variables)

################################################################################
# Set output columns
outputcols <- c("reftyd", "obvhod", "isced", "vek", "zampost", "zamnace", 
                "rodstav", "pohl", "prac1h", "prac0h", "wgtos", "wgtdom", 
                "typsml", "zamisco", "hhnum", "persnum", "year", "quarter")

################################################################################
# APPEND
csu_data <- do.call(rbind, lapply(dfs_transformed, function(df) {
  df %>% select(all_of(outputcols))
}))

################################################################################
# Save the dataset
write.csv(csu_data, file.path(OUTPUT, "CSU_data_appended.csv"))

################################################################################
# Check

#outputcols2 <- c("reftyd", "obvhod", "isced", "vek", "zampost", "zamnace", "rodstav", 
#                "pohl", "prac1h", "prac0h", "typsml", "zamisco", "wgtos", "wgtdom", 
#                "hhnum", "persnum", "year", "quarter", "obvhod_aux", "isced_aux", 
#                "vek_aux", "zampost_aux", "zamnace_aux", "rodstav_aux", "pohl_aux", 
#                "prac1h_aux", "prac0h_aux", "typsml_aux", "zamisco_aux")
#
#df <- do.call(rbind, lapply(dfs_transformed, function(df) {
#  df %>% select(all_of(outputcols2))
#}))
#
#dim(df)
#View(df)

# hhnum to persnum
#n_distinct(df$persnum, na.rm = TRUE) / n_distinct(df$hhnum, na.rm = TRUE)
#n_distinct(df0812$persnum, na.rm = TRUE) / n_distinct(df0812$hhnum, na.rm = TRUE)
#n_distinct(df1316$persnum, na.rm = TRUE) / n_distinct(df1316$hhnum, na.rm = TRUE)
#n_distinct(df14$persnum, na.rm = TRUE) / n_distinct(df14$hhnum, na.rm = TRUE)
#n_distinct(df1719$persnum, na.rm = TRUE) / n_distinct(df1719$hhnum, na.rm = TRUE)
#n_distinct(df2021$persnum, na.rm = TRUE) / n_distinct(df2021$hhnum, na.rm = TRUE)
#n_distinct(df2223$persnum, na.rm = TRUE) / n_distinct(df2223$hhnum, na.rm = TRUE)

# obvhod
#summary(df$obvhod)
#sum(is.na(df$obvhod))
#
#summary(as.numeric(df$obvhod_aux))
#sum(is.na(as.numeric(df$obvhod_aux)))+sum(as.numeric(df$obvhod_aux)>98, na.rm = TRUE)+sum(as.numeric(df$obvhod_aux)<0, na.rm = TRUE)
#
#testing <- df %>%
#  filter(!is.na(obvhod)) %>%
#  pull(obvhod_aux) %>%
#  unique()
#print(sort(testing))

# isced
#testing <- df %>%
#  filter(isced==2) %>%
#  pull(isced_aux) %>%
#  unique()
#print(testing)
#
#rows_with_conditions <- df %>%
#  filter(is.na(isced_aux) & isced == 3)
#print(rows_with_conditions)
#
#unique(df$isced_aux)
#sum(df$isced_aux == "NaN", na.rm = TRUE)+
#  sum(df$isced_aux == "Nezjištìno", na.rm = TRUE)+
#  sum(df$isced_aux == "Nezjišteno", na.rm = TRUE)+
#  sum(df$isced_aux == "Nezjištěno", na.rm = TRUE)+
#  sum(df$isced_aux == "-2", na.rm = TRUE)+
#  sum(is.na(df$isced_aux))
#summary(df$isced)

# vek
#summary(as.numeric(df$vek_aux))
#summary(df$vek)
#unique((df$vek_aux))
#sum(df$vek_aux == "NA", na.rm = TRUE) + sum(is.na(df$vek_aux))

# zampost
#testing <- df %>%
#  filter(zampost == 1) %>%
#  pull(zampost_aux) %>%
#  unique()
#print(testing)
#
#unique(df$zampost_aux)
#summary(df$zampost)

# rodstav
#testing <- df %>%
#  filter(is.na(rodstav)) %>%
#  pull(rodstav_aux) %>%
#  unique()
#print(testing)
#
#unique(df$rodstav_aux)
#sum(df$rodstav_aux == "NaN", na.rm = TRUE)+
#  sum(is.na(df$rodstav_aux))+
#  sum(df$rodstav_aux == "Nezjištìno", na.rm = TRUE)+
#  sum(df$rodstav_aux == "Nezjišteno", na.rm = TRUE)+
#  sum(df$rodstav_aux == "Nezjištěno", na.rm = TRUE)
#summary(df$rodstav)

# pohl
#testing <- df %>%
#  filter(is.na(pohl)) %>%
#  pull(pohl_aux) %>%
#  unique()
#print(testing)
#
#summary(df$pohl)
#unique((df$pohl_aux))
#sum(df$pohl_aux == "NaN", na.rm = TRUE)+sum(is.na(df$pohl_aux))
#sum(df$pohlh_aux == "Nezjištěno", na.rm = TRUE)

# prac1h
#testing <- df %>%
#  filter(prac1h==2) %>%
#  pull(prac1h_aux) %>%
#  unique()
#print(testing)
#
#summary(df$prac1h)
#unique((df$prac1h_aux))
#sum(df$prac1h_aux == "NaN", na.rm = TRUE)+sum(is.na(df$prac1h_aux))+
#  sum(df$prac1h_aux == "Nezjištěno", na.rm = TRUE)

# prac0h
#testing <- df %>%
#  filter(prac0h==1) %>%
#  pull(prac0h_aux) %>%
#  unique()
#print(testing)
#
#summary(df$prac0h)
#unique((df$prac0h_aux))
#sum(df$prac0h_aux == "NaN", na.rm = TRUE)+sum(is.na(df$prac0h_aux))+
#  sum(df$prac0h_aux == "Nezjištěno", na.rm = TRUE)

# typsml
#testing <- df %>%
#  filter(is.na(typsml)) %>%
#  pull(typsml_aux) %>%
#  unique()
#print(testing)

# zamisco
#testing <- df %>%
#  filter(zamisco==1) %>%
#  pull(zamisco_aux) %>%
#  unique()
#print(testing)
#
#sum(df$zamisco==0, na.rm=TRUE)