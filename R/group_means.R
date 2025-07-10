#' Calculate Mean Chronic Absenteeism Rates by Subgroup and Year
#' 
#' Calculates state-level mean chronic absenteeism
#' rates for each student subgroup.
#' 
#' @param data Student-level data frame with demographic columns
#' @return Data frame with mean rates by year and subgroup
#' @keywords internal
group_means <- function(data) {
 
 subgroups <- list(
   "ALL" = rep(1, nrow(data)),
   "HISPANIC" = data$HISPANIC,
   "WHITE" = data$WHITE,
   "ASIAN" = data$ASIAN,
   "BLACK" = data$BLACK,
   "ELL" = data$ELL,
   "DISADVANTAGE" = data$DISADVANTAGE,
   "DISABILITY" = data$DISABILITY,
   "MALE" = data$MALE,
   "FEMALE" = data$FEMALE
 )
 
 results <- data.frame()
 
 for (year in sort(unique(data$YEAR))) {
   year_data <- data[data$YEAR == year, ]
   
   for (subgroup_name in names(subgroups)) {
     mask <- subgroups[[subgroup_name]][data$YEAR == year] == 1
     students <- year_data[mask, ]
     
     if (nrow(students) > 0) {
       rate <- round(mean(students$CHRONIC_ABSENT) * 100, 2)
       
       results <- rbind(results, data.frame(
         YEAR = year,
         SUBGROUP = subgroup_name,
         total_students = nrow(students),
         chronically_absent_count = sum(students$CHRONIC_ABSENT),
         absenteeism_rate = rate
       ))
     }
   }
 }
 
 rownames(results) <- NULL
 
 rm(
  subgroups,
  year,
  year_data,
  subgroup_name,
  mask,
  students,
  rate
)
 
 return(results)
}
