# Foundations of Data Science
# Data Wrangling Exercise 1: Basic Data Manipulation
# Author: Anthony Koukoullis


load_data <- function(){
  
  result <- tryCatch({
    
    # Set the full path of your RStudio working directory to the "local_working_dir" variable if necessary,
    # and uncomment the next two lines
    local_working_dir <- "/Users/akoukoullis/Documents/Springboard\ Foundations\ of\ Data\ Science/Exercises/3\ DATA\ WRANGLING/Data\ Wrangling\ Exercise\ 1\ -\ Basic\ Data\ Manipulation"
    setwd(local_working_dir)
    
    data_filename <- "refine_original.csv"
    table <- tbl_df(read.csv(data_filename))
    
    return(table)
    
  }, error = function(e){
    
    print("Unfortunately the data couldn't be loaded: ", e)
    
    return(FALSE)
  })
  
  return(result)
}


save_data <- function(table){
  
  tryCatch({
    
    # Set the full path to your RStudio working directory to the "local_working_dir" variable if necessary,
    # and uncomment the next two lines
    local_working_dir <- "/Users/akoukoullis/Documents/Springboard\ Foundations\ of\ Data\ Science/Exercises/3\ DATA\ WRANGLING/Data\ Wrangling\ Exercise\ 1\ -\ Basic\ Data\ Manipulation"
    setwd(local_working_dir)
    
    data_filename <- "refine_clean.csv"
    write.csv(table, file = data_filename, quote = TRUE, row.names = FALSE)
    
  }, error = function(e){
    print("Unfortunately the data couldn't be loaded: ", e)
  })
}


clean_up_brand_names <- function(table){
  
  result <- tryCatch({
    
    table[c(grep(glob2rx("phil*"), table$company, ignore.case = TRUE), grep(glob2rx("phl*"), table$company, ignore.case = TRUE), grep(glob2rx("fil*"), table$company, ignore.case = TRUE)),"company"] <- "philips"
    table[grep(glob2rx("ak*"), table$company, ignore.case = TRUE),"company"] <- "akzo"
    table[grep(glob2rx("van*"), table$company, ignore.case = TRUE),"company"] <- "van houten"
    table[grep(glob2rx("uni*"), table$company, ignore.case = TRUE),"company"] <- "unilever"
    
    return(table)
    
  }, error = function(e){
    
    print("Unfortunately the brand names couldn't be cleaned up: ", e)
    
    return(FALSE)
    
  })

  return(result)
}




separate_product_code_and_number <- function(table){
  
  result <- tryCatch({
    
    table <- table %>% extract(col = "Product.code...number", into = c("product_code", "product_number"), regex = "([a-zA-Z]+)-([0-9]+)")
    
    return(table)
    
  }, error = function(e){
    
    print("Unfortunately the separate product code and numbers couldn't be separated: ", e)
    
    return (FALSE)
    
  })
  
  return(result)
}


add_product_categories <- function(table){
  
  result <- tryCatch({
    
    for (i in seq_len(nrow(table))){
      if (table[i,2] == "p"){
        table[i,"category"] = "Smartphone"
      }
      else if (table[i,2] == "v"){
        table[i, "category"] = "TV"
      }
      else if (table[i,2] == "x"){
        table[i, "category"] = "Laptop"
      }
      else if (table[i,2] == "q"){
        table[i, "category"] = "Tablet"
      }
    }
    
    return(table)
    
  }, error = function(e){
    
    print("Unfortunately the product categories couldn't be added: ", e)
    
    return(FALSE)
    
  })
  
  return (result)
}


add_full_address_for_geocoding <- function(table){
  
  result <- tryCatch({
    
    for (i in seq_len(nrow(table))){
      table[i, "full_address"] = paste(sapply(table[i, 4:6], as.character), collapse = ", ")
    }
    
    return (table)
    
  }, error = function(e){
    
    print("Unfortunately the full addresses couldn't be added: ", e)
    
    return(FALSE)
    
  })
  
  return (result)
}




create_dummy_variables_for_company_and_product_category <- function(table){
  
  result <- tryCatch({
    
    columns <- c("company", "category")
    
    for (column in columns){
    
      for (j in sapply(unique(table[, column]), as.character)){
        table[, paste(c(column, "_", tolower(j)), collapse = "")] = 0
      }
  
      for (j in seq_len(nrow(table))){
        table[j, paste(c(column, "_", tolower(sapply(table[j, column], as.character))), collapse = "")] = 1
      }
    
    }
    
    return(table)
    
  }, error = function(e){
    
    print("Unfortunately the dummy variables couldn't be created: ", e)
    
    return(FALSE)
  })
  
  return (result)
}


global_result <- tryCatch({
  
  # Install package dependencies if not already installed
  installed_packages <- installed.packages()
  if (is.element("dplyr", installed_packages[,"Package"]) == FALSE) { install.packages("dplyr") }
  if (is.element("tidyr", installed_packages[,"Package"]) == FALSE) { install.packages("tidyr") }
  library("dplyr")
  library("tidyr")
  
  # Load data from csv file
  refine <- load_data()
  
  # Functions to clean up the data
  refine <- clean_up_brand_names(refine)
  refine <- separate_product_code_and_number(refine)
  refine <- add_product_categories(refine)
  refine <- add_full_address_for_geocoding(refine)
  refine <- create_dummy_variables_for_company_and_product_category(refine)
  
  # Save clean data to different csv file
  save_data(refine)
  
  # View clean data
  View(refine)
  
}, error = function(e_global){
  
  print("Unfortunately the script didn't complete its execution: ", e_global)

})