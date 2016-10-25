# Foundations of Data Science
# Data Wrangling Exercise 2: Dealing with missing values
# Author: Anthony Koukoullis


load_data <- function(filename){
  
  result <- tryCatch({

    table <- tbl_df(read.csv(filename, stringsAsFactors = FALSE))
    
    return(table)
    
  }, error = function(e){
    
    print("Unfortunately the data couldn't be loaded: ", e)
    
    return(FALSE)
  })
  
  return(result)
}


save_data <- function(table, filename){
  
  tryCatch({
    
    write.csv(table, file = filename, quote = TRUE, row.names = FALSE)
    
  }, error = function(e){
    print("Unfortunately the data couldn't be loaded: ", e)
  })
}


clean_empty_rows <- function(table){

  result <- tryCatch({
    
    #table <- table[-which(is.na(table$pclass)),]
    table <- subset(table, !is.na(pclass))
    
    return(table)
    
  }, error = function(e){
    print("Unfortunately the embarked column couldn't be populated: ", e)
    
    return(FALSE)
  })

  return(result)  
}


port_of_embarkation <- function(table){

  result <- tryCatch({
    
    table[which(table$embarked == ""), "embarked"] <- "S"
    
    return(table)
    
  }, error = function(e){
    print("Unfortunately the embarked column couldn't be populated: ", e)
    
    return(FALSE)
  })

  return(result)  
}


age <- function(table){
  
  result <- tryCatch({
    
    mean_age <- colMeans(subset(table, is.numeric(age) & !is.na(age))[,"age"])
    
    table[is.na(table$age), "age"] <- mean_age
    
    return(table)
    
  }, error = function(e){
    print("Unfortunately the embarked column couldn't be populated: ", e)
    
    return(FALSE)
  })
  
  return(result)  
}


lifeboat <- function(table){
  
  result <- tryCatch({
    
    table[is.na(table$boat) | table$boat == "", "boat"] <- "NA"
    
    return(table)
    
  }, error = function(e){
    print("Unfortunately the embarked column couldn't be populated: ", e)
    
    return(FALSE)
  })
  
  return(result)  
}


lifeboat <- function(table){
  
  result <- tryCatch({
    
    table[is.na(table$boat) | table$boat == "", "boat"] <- "NA"
    
    return(table)
    
  }, error = function(e){
    print("Unfortunately the embarked column couldn't be populated: ", e)
    
    return(FALSE)
  })
  
  return(result)  
}


cabin <- function(table){
  
  result <- tryCatch({
    
    table[is.na(table$cabin) | table$cabin == "", "has_cabin_number"] <- 0
    table[!is.na(table$cabin) & table$cabin != "", "has_cabin_number"] <- 1
    
    return(table)
    
  }, error = function(e){
    print("Unfortunately the embarked column couldn't be populated: ", e)
    
    return(FALSE)
  })
  
  return(result)  
}


tryCatch({
  
  # Install package dependencies if not already installed
  installed_packages <- installed.packages()
  if (is.element("dplyr", installed_packages[,"Package"]) == FALSE) { install.packages("dplyr") }
  if (is.element("tidyr", installed_packages[,"Package"]) == FALSE) { install.packages("tidyr") }
  library("dplyr")
  library("tidyr")

  # Set the full path of your RStudio working directory to the "local_working_dir" variable if necessary,
  # and uncomment the next two lines
  #local_working_dir <- "/Users/akoukoullis/Documents/Springboard\ Foundations\ of\ Data\ Science/Exercises/3\ DATA\ WRANGLING/Data\ Wrangling\ Exercise\ 2\ -\ Dealing\ with\ missing\ values"
  #setwd(local_working_dir)
  
  # set the name of the original dataset filename
  original_filename <- "titanic_original.csv"

  # set the name of the clean dataset filename
  clean_filename <- "titanic_clean.csv"
  
  # Load data from csv file
  titanic <- load_data(original_filename)
  
  # Functions to clean up the data
  titanic <- clean_empty_rows(titanic)
  titanic <- port_of_embarkation(titanic)
  titanic <- age(titanic)
  titanic <- lifeboat(titanic)
  titanic <- cabin(titanic)

  # Save clean data to different csv file
  save_data(titanic, clean_filename)
  
  # View clean data
  View(titanic)
  
}, error = function(e_global){
  
  print("Unfortunately the script didn't complete its execution: ", e_global)

})