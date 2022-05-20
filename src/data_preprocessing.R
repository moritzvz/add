# this function preprocesses COMPAS dataset to be examined by ALD with, e.g., equalized odd
preprocess_compas_data <- function(check_race=TRUE, 
                                   check_age=FALSE, 
                                   check_gender=FALSE){
  # read in "raw" COMPAS data from kaggle: 
  # https://www.kaggle.com/danofer/compass/version/1
  # Originally put together by ProPublica: 
  # https://www.propublica.org/article/how-we-analyzed-the-compas-recidiv 
  data <- read.table("data/raw/compas_data.csv", sep = ",", header=TRUE)
  
  # built prediction type feature: TP (true positive), TN, FP, FN
  e_types = c("tp", "tn", "fp", "fn")
  data$tn <- as.integer(data["Two_yr_Recidivism"] == 0 & data["score_factor"] == 0)
  data$tp <- as.integer(data["Two_yr_Recidivism"] == 1 & data["score_factor"] == 1)
  data$fp <- as.integer(data["Two_yr_Recidivism"] == 0 & data["score_factor"] == 1)
  data$fn <- as.integer(data["Two_yr_Recidivism"] == 1 & data["score_factor"] == 0)
  data$error_type <-  names(data[e_types])[max.col(data[e_types])]
  
  if(check_race){
    # build race sensitive attribute
    race_feat <- c("African_American", "Asian", "Hispanic",
                 "Native_American", "Other")
    data$Caucasian <- as.integer(rowSums(data[race_feat]) == 0)
    race_feat <- c("African_American", "Asian", "Caucasian",
                 "Hispanic", "Native_American", "Other")
    data$race <- names(data[race_feat])[max.col(data[race_feat])]
  }
  
  if(check_age){
    # build gender sensitive attribute
    data$Male <- as.integer(data$Female == 0)
    data$gender <- names(data[c("Female", "Male")])[max.col(data[c("Female", "Male")])]
  }
  
  if(check_age){
    # build age sensitive attribute (three brackets)
    ages <- c("Age_Above_FourtyFive", "Age_Below_TwentyFive")
    data["TwentyFive_to_FourtyFive"] <- as.integer(rowSums(data[ages]) == 0)
    ages <- c("Age_Above_FourtyFive", "Age_Below_TwentyFive", "TwentyFive_to_FourtyFive")
    data$age <- names(data[ages])[max.col(data[ages])]
  }
  
  # select data columns to export
  col_selection <- append(c("race", "age", "gender")[c(check_race, check_age, check_gender)],
                          c("error_type"))
  data <- data[col_selection]
  
  write.csv(data, "data/processed/compas_data_preproc.csv", row.names=FALSE)
}

# this function preprocesses Adult Income dataset to be examined by ALD w.r.t. statistical parity
preprocess_adult_data <- function(check_race=TRUE, check_age=TRUE, 
                                  check_relationship=TRUE, check_sex=TRUE,
                                  check_marital_status=TRUE){
  # read in raw data from UCI ML repository
  # https://archive.ics.uci.edu/ml/datasets/adult
  col_names <- c("age", "workclass", "fnlwgt", "education", "education-num", 
                 "marital-status", "occupation", "relationship", "race", "sex", 
                 "capital-gain", "capital-loss", "hours-per-week", 
                 "native-country", "income")
  data1 <- read.table("data/raw/adult.data", sep = ",", header=FALSE)
  data2 <- read.table("data/raw/adult.test", skip = 1, sep = ",", header=FALSE)
  data <- rbind(data1, data2)
  colnames(data) <- col_names
  
  # rename columns
  for(renames in list(c("income", "outcome"), c("native-country", "native_country"),
                   c("marital-status", "marital_status"))){
    names(data)[names(data) == renames[1]] <- renames[2]
  }
  
  # outcome as integer (0 vs 1, where 1 is favorable)
  data$outcome <- as.integer(data["outcome"] == " >50K")
  
  # preprocess native country (top 20, rest is "other")
  top_22_countries <- names(sort(table(data$native_country),decreasing=T)[1:22])
  top_22_countries[top_22_countries == " ?"] <- "Other"
  data$native_country[!data$native_country %in% top_22_countries] <- "Other"
  
  # keep sensitive attributes and output
  sens_attri <- c("race", "age", "relationship", "sex", "marital_status")
  sens_attri <- sens_attri[c(check_race, check_age, check_relationship, 
                             check_sex, check_marital_status)]
  
  # select data columns to export
  col_selection <- append(c("outcome"), sens_attri)
  data <- data[col_selection]
  
  write.csv(data, "data/processed/adult_data_preproc.csv", row.names=FALSE)
}
