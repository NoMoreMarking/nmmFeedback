#' Summarise data with mean and confidence intervals
#'
#' @param data data frame
#' @param var the name of the variable to summarise
#' @param ... the name of the grouping variable(s)
#' @return data frame with n, mean, sd, se, ci, upper and lower
#' @examples
#' summary.data <- summaryData(school.results, school, gradeScore)
#' @export
#' @import dplyr

summariseData <- function(data, var, ...){
  data %>%
    group_by(...) %>%
    summarise(
      data.n=n(),
      data.mean=mean({{var}},na.rm=T),
      data.sd=sd({{var}},na.rm = T),
      data.se=data.sd/(data.n^0.5),
      data.ci=1.96*data.se,
      data.upper=data.mean+data.ci,
      data.lower=data.mean-data.ci)
}

#' Create significance statement for reports using grades
#'
#' @param results results data frame
#' @return significance statement
#' @examples
#' all.significance.statement <- significanceStatement(results)
#' @export
#' @import dplyr

significanceStatement <- function(results){
  mdl <- t.test(gradeScore ~ school, data = results)
  mdl.significance <- mdl$p.value<0.05
  if(mdl.significance){
    if(mdl$estimate[1]>mdl$estimate[2]){
      significance.statement <- ' significantly above that of '
    } else {
      significance.statement <- ' significantly below that of '
    }
  } else {
    significance.statement <- ' not significantly different to '
  }
  return (significance.statement)

}

#' Create significance statement for reports using scaled scores
#'
#' @param results results data frame
#' @return significance statement
#' @examples
#' all.significance.statement <- scoreSignificanceStatement(results)
#' @export
#' @import dplyr
#' 
scoreSignificanceStatement <- function(results){
  mdl <- t.test(scaledScore ~ school, data = results)
  mdl.significance <- mdl$p.value<0.05
  if(mdl.significance){
    if(mdl$estimate[1]>mdl$estimate[2]){
      significance.statement <- ' significantly above that of '
    } else {
      significance.statement <- ' significantly below that of '
    }
  } else {
    significance.statement <- ' not significantly different to '
  }
  return (significance.statement)
  
}