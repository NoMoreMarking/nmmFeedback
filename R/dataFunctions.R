#' Set judges to exclude on national moderation tasks
#'
#' @param taskName name of tasks (uses regex)
#' @param connStr mongo connection string
#' @return nothing
#' @examples
#' cleanJudges(taskName, connStr, infit, decisions)
#' @export
#' @import dplyr
#' @import nmmMongo

cleanJudges <- function(taskName, connStr, infit=1.3, decisions=20){
  allTasks <- getTasks(taskName, connStr)

  # Remove the moderation tasks
  tasks <- allTasks %>% filter(!grepl("Moderation",name))
  results <- NULL

  judges <- NULL
  for (task in tasks$id) {
    judges <- bind_rows(judges, getJudges(task, connStr))
  }

  removeJudges <- judges %>% filter(localComparisons<=decisions | trueScore >= infit)

  # Pull down moderation data
  # Remove dodgy judges
  # Find the bad judge on the moderation task

  moderationPots <- allTasks %>% filter(grepl('Moderation',name))
  moderationPots <- moderationPots %>% filter(decisions>0)
  moderationPotIds <- moderationPots$id

  remove_judge_ids <- NULL

  for(id in moderationPotIds){
    judges <- getJudges(id,connStr)
    removeJudges <- judges %>% filter(emailLower %in% removeJudges$emailLower)
    remove_judge_ids <- c(remove_judge_ids, removeJudges$`_id`)
  }

  # Set judge on site to excluded
  for(judge_id in remove_judge_ids){
    excludeJudge(judge_id, connStr)
    cat(judge_id,' excluded\n')
  }

}



#' Get data from a series of national tasks, and clean
#'
#' @param taskName name of tasks (uses regex)
#' @param connStr mongo connection string
#' @return data frame results
#' @examples
#' results <- getData(taskName, connStr)
#' @export
#' @import dplyr
#' @import nmmMongo

getData <- function(taskName, connStr){
  tasks <- getTasks(taskName, connStr)
  # Remove the moderation tasks
  tasks <- tasks %>% filter(decisions>0,!grepl("Moderation",name))
  taskNames <- tasks$name
  results <- NULL

  for (task in taskNames) {
    persons <- getPersons(task, connStr)
    if(nrow(persons)>0){
      # Ignore rubbish
      persons <- persons %>% filter(!is.na(comparisons))
      endStr <- nchar(unique(persons$taskName))
      startStr <- endStr - 6
      persons <- persons %>%
        mutate(dfe=substr(taskName,startStr,endStr),rank=rank(theta, ties.method = 'random'),initials=
                 paste0(substr(firstName, 1, 1),
                        substr(lastName, 1, 1))
        ) %>%
        select("taskName","status","name","candidate","firstName","lastName","dob","gender","group","pp","comparisons","theta","seTheta"="seTrueScore","scaledScore","seScaledScore","level","dfe","rank","initials")
      # Add indication of which pupils were moderated
      persons$lastName[persons$seTheta==0] <-
        paste0(persons$lastName[persons$seTheta==0], '*')

      # Clean PP data
      if(typeof(persons$pp)=="logical"){
        persons$pp[which(persons$pp==TRUE)] <- 'Y'
        persons$pp[which(persons$pp==FALSE)] <- 'N'
      } else {
        persons$pp[which(persons$pp=="TRUE")] <- 'Y'
        persons$pp[which(persons$pp=="FALSE")] <- 'N'
      }
      results <-bind_rows(results,persons)
    }
  }

  # Remove absentees
  results <- results %>%
    filter(comparisons>0,status!='Absentee',status!='Anchor') %>%
    mutate(pp=recode(pp,'Y'='Y','N'='N',.missing='N',.default='N'))
}


#' Summarise data with mean and confidence intervals
#'
#' @param data data frame
#' @param by the name of the grouping variable
#' @param var the name of the variable to summarise
#' @return data frame with n, mean, sd, se, ci, upper and lower
#' @examples
#' summary.data <- summaryData(school.results, school, gradeScore)
#' @export
#' @import dplyr

summariseData <- function(data, by, var){
  data %>%
    group_by({{by}}) %>%
    summarise(
      data.n=n(),
      data.mean=mean({{var}},na.rm=T),
      data.sd=sd({{var}},na.rm = T),
      data.se=data.sd/(data.n^0.5),
      data.ci=1.96*data.se,
      data.upper=data.mean+data.ci,
      data.lower=data.mean-data.ci)
}

#' Summarise data with mean and confidence intervals, grouped on gender
#'
#' @param results results data frame
#' @return data frame with n, mean, sd, se, ci, upper and lower
#' @examples
#' summary.data <- summaryData(school.results)
#' @export
#' @import dplyr

summariseGenderData <- function(results){
  results <- results %>%
    group_by(school,gender) %>%
    summarise(
      data.n=n(),
      data.mean=mean(gradeScore,na.rm=T),
      data.sd=sd(gradeScore,na.rm = T),
      data.se=data.sd/(data.n^0.5),
      data.ci=1.96*data.se,
      data.upper=data.mean+data.ci,
      data.lower=data.mean-data.ci)
  return(results)
}

#' Summarise data with mean and confidence intervals, grouped on pupil premium
#'
#' @param results results data frame
#' @return data frame with n, mean, sd, se, ci, upper and lower
#' @examples
#' summary.data <- summarisePPData(school.results)
#' @export
#' @import dplyr

summarisePPData <- function(results){
  results <- results %>%
    group_by(school,pp) %>%
    summarise(
      data.n=n(),
      data.mean=mean(gradeScore,na.rm=T),
      data.sd=sd(gradeScore,na.rm = T),
      data.se=data.sd/(data.n^0.5),
      data.ci=1.96*data.se,
      data.upper=data.mean+data.ci,
      data.lower=data.mean-data.ci)
  return(results)
}

#' Summarise data with mean and confidence intervals, grouped on gender & pupil premium
#'
#' @param results results data frame
#' @return data frame with n, mean, sd, se, ci, upper and lower
#' @examples
#' summary.data <- summariseGenderPPData(school.results)
#' @export
#' @import dplyr

summariseGenderPPData <- function(results){
  results <- results %>%
    group_by(school,gender,pp) %>%
    summarise(
      data.n=n(),
      data.mean=mean(gradeScore,na.rm=T),
      data.sd=sd(gradeScore,na.rm = T),
      data.se=data.sd/(data.n^0.5),
      data.ci=1.96*data.se,
      data.upper=data.mean+data.ci,
      data.lower=data.mean-data.ci)
  return(results)
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