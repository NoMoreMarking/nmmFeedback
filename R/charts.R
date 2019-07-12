#' Headline box and whiskers school compared to all other schools individually
#'
#' @param results results data frame
#' @param yname label for y axis
#' @param grade.boundaries grade boundaries
#' @return plot
#' @examples
#' p <- schools.plot(results, grade.boundaries)
#' @export
#' @import ggplot2
#'
schools.plot <- function(results, yname, grade.boundaries=NULL) {
  p <- ggplot(results, aes(x = dfe, y = scaledScore, fill = school))
  p <- p + geom_boxplot()
  p <- p + scale_fill_manual(name = "", values = c('red', 'white'))
  p <- p + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
  if(!is.null(grade.boundaries)){
    p <-
      p + geom_hline(data = grade.boundaries, aes(yintercept = scaled, linetype =
                                                  grades,colour=grades))
    p <- p + scale_colour_manual(name="",values=c('#2b83ba','#1a9641'))
    p <-
      p + scale_linetype_manual(name = "",
                                values = c(2, 4),
                                guide = guide_legend())
  }
  p <- p +  theme(legend.position = "bottom")
  p <- p + scale_y_continuous(name = yname)

  return(p)
}


#' Headline box and whiskers school compared to cohort by pp
#'
#' @param results results data frame
#' @param grade.boundaries grade boundaries
#' @return plot
#' @examples
#' p <- schools.pp.plot(results, grade.boundaries)
#' @export
#' @import ggplot2
#'
schools.pp.plot <- function(results, grade.boundaries) {
  p <- ggplot(results, aes(x = school, y = scaledScore))
  p <- p + geom_boxplot()
  p <-
    p + geom_hline(data = grade.boundaries, aes(yintercept = scaled, linetype =
                                                  grades,colour=grades))
  p <- p + scale_y_continuous(name = c('Scaled Writing Score'))
  p <-
    p + scale_linetype_manual(name = "",
                              values = c(2, 4),
                              guide = guide_legend())
  p <- p +  theme(legend.position = "bottom")
  p <- p + annotate("text", x = 0.5, y = grade.boundaries$scaled[2]+0.4, label = "GDS")
  p <- p + annotate("text", x = 0.5, y = grade.boundaries$scaled[1]+0.4, label = "EXS")
  p <- p + scale_colour_manual(name="",values=c('#2b83ba','#1a9641'))
  p <- p + facet_wrap(~pp)
  return(p)
}

#' Headline box and whiskers school compared to cohort by gender
#'
#' @param results results data frame
#' @param grade.boundaries grade boundaries
#' @return plot
#' @examples
#' p <- schools.gender.plot(results, grade.boundaries)
#' @export
#' @import ggplot2
#'
schools.gender.plot <- function(results, grade.boundaries) {
  p <- ggplot(results, aes(x = school, y = scaledScore))
  p <- p + geom_boxplot()
  p <-
    p + geom_hline(data = grade.boundaries, aes(yintercept = scaled, linetype =
                                                  grades,colour=grades))
  p <- p + scale_y_continuous(name = c('Scaled Writing Score'))
  p <-
    p + scale_linetype_manual(name = "",
                              values = c(2, 4),
                              guide = guide_legend())
  p <- p +  theme(legend.position = "bottom")
  p <- p + annotate("text", x = 0.5, y = grade.boundaries$scaled[2]+0.4, label = "GDS")
  p <- p + annotate("text", x = 0.5, y = grade.boundaries$scaled[1]+0.4, label = "EXS")
  p <- p + scale_colour_manual(name="",values=c('#2b83ba','#1a9641'))
  p <- p + facet_wrap(~gender)
  return(p)
}

#' Headline box and whiskers school compared to cohort
#'
#' @param results results data frame
#' @param grade.boundaries grade boundaries
#' @return plot
#' @examples
#' p <- schools.detail.plot(results, grade.boundaries)
#' @export
#' @import ggplot2
#'
schools.detail.plot <- function(results, grade.boundaries) {
  p <- ggplot(results, aes(x = school, y = scaledScore))
  p <- p + geom_boxplot()
  p <-
    p + geom_hline(data = grade.boundaries, aes(yintercept = scaled, linetype =
                                                  grades,colour=grades))
  p <- p + scale_y_continuous(name = c('Scaled Writing Score'))
  p <-
    p + scale_linetype_manual(name = "",
                              values = c(2, 4),
                              guide = guide_legend())
  p <- p +  theme(legend.position = "bottom")
  p <- p + annotate("text", x = 0.5, y = grade.boundaries$scaled[2]+0.4, label = "GDS")
  p <- p + annotate("text", x = 0.5, y = grade.boundaries$scaled[1]+0.4, label = "EXS")
  p <- p + scale_colour_manual(name="",values=c('#2b83ba','#1a9641'))
  p <- p + theme(axis.title.x = element_blank())
  return(p)
}


#' Plot data with mean and confidence intervals
#'
#' @param data results data frame
#' @param x column for x co-ordinate
#' @param data.mean column for y co-ordinate
#' @param data.upper column for upper error bar
#' @param data.lower column for lower error bar
#' @param ylimits limits for y axis
#' @param ylabel label for y axis
#' @param fill grouping variable for fill
#' @return plot
#' @examples
#' p <- meanSePlot(pp.summary, ylimits=ylimits, ylabel=ylabel,x = school, fill=pp)
#' @export
#' @import ggplot2
#'
meanSePlot <- function(data, x, ylimits, ylabel, data.mean=data.mean, data.lower=data.lower, data.upper=data.upper,fill=NULL) {
  p <- ggplot(data, aes(x = {{x}}, y = {{data.mean}}, fill = {{fill}}))
  p <-
    p + geom_errorbar(aes(ymin = {{data.lower}}, ymax = {{data.upper}}), width = 0.2)
  p <-
    p + geom_point(
      shape = 21,
      colour = "black",
      size = 5,
      stroke = 1
    )
  p <- p + scale_shape_manual(values = 1)
  p <- p + theme_light()
  p <- p + theme(axis.title.x = element_blank())
  p <- p + scale_y_continuous(name = ylabel, limits = ylimits)
  p <- p + scale_fill_manual(values = c('black', 'white'), name='')
  p <- p + theme(legend.position = 'bottom')
  return(p)
}

#' Plot box and whisker of marks and grade boundaries
#' 
#' @param data results data frame
#' @param x x co-ordinate column name
#' @param y y co-ordinate column name
#' @param yLable label for y axis
#' @param boundaries grade boundaries data frame
#' @param cutScores cut scores for horizontal intersect
#' @param gradeLabels labels for cut scores
#' 
#' @example 
#' p <- boxPlot(results, x=school, y=scaledScore, yLabel=mark, boundaries=displayGrades, cutScores=scaledScore, gradeLabels=gradeBoundaries)
#' @export
#' @import ggplot2
#' 

boxPlot <- function(data, x, y, yLabel, boundaries, cutScores, gradeLabels) {
  # box plot of mean marks
  p <- ggplot(data, aes(x = {{x}}, y = {{y}}))
  p <-
    p <- p + geom_boxplot(colour = "black", fill = "light blue")
  p <-
    p + geom_hline(data = boundaries, aes(yintercept = {{cutScores}}, linetype =
    {{gradeLabels}}))
  p <- p + scale_y_continuous(name = yLabel)
  p <- p + theme_light()
  p <- p + theme(axis.title.x = element_blank(), legend.title = element_blank())
}

#' Plot pupil score with error and grade boundaries
#'
#' @param pupil results row from results data frame
#' @param grade.boundaries grade boundary data frame
#' @param errorScale vector for x scale limits
#' @return plot
#' @examples
#' p <- error.bar(pupil, grade.boundaries, errorScale)
#' @export
#' @import ggplot2
#'
error.bar <- function(pupil, grade.boundaries, errorScale){
  if(pupil$grade=='EXS'){
    colorscheme <- c('#377eb8')
  } else if (pupil$grade=='GDS') {
    colorscheme <- c('#4daf4a')
  } else if (pupil$grade == 'WTS') {
    colorscheme <- c('#e41a1c')
  }
  p <- ggplot(pupil, aes(x=scaledScore, y=0.5))
  p <- p + geom_vline(data = grade.boundaries, aes(xintercept = scaled),colour='black',linetype='dotted')
  p <- p + geom_point(color=colorscheme)
  p <- p + geom_errorbarh(aes(xmax = scaledScore + seScaledScore, xmin = scaledScore - seScaledScore,height = 0.5,color=colorscheme))
  p <- p + scale_x_continuous(limits = errorScale)
  p <- p + scale_y_continuous(limits=c(0,1))
  p <- p + scale_colour_manual(values=colorscheme)
  p <- p + theme_void()
  p <- p + theme(
    legend.position = 'none'
  )
  return(p)
}

#' Plot pupil score with error and grade boundaries without colour
#'
#' @param pupil results row from results data frame
#' @param grade.boundaries grade boundary data frame
#' @param errorScale vector for x scale limits
#' @return plot
#' @examples
#' p <- error.bar(pupil, grade.boundaries, errorScale)
#' @export
#' @import ggplot2
#'
error.bar.generic <- function(pupil, grade.boundaries, errorScale){
  p <- ggplot(pupil, aes(x=scaledScore, y=0.5))
  p <- p + geom_vline(data = grade.boundaries, aes(xintercept = cutOff),colour='black',linetype='dotted')
  p <- p + geom_point()
  p <- p + geom_errorbarh(aes(xmax = scaledScore + seScaledScore, xmin = scaledScore - seScaledScore,height = 0.5))
  p <- p + scale_x_continuous(limits = errorScale)
  p <- p + scale_y_continuous(limits=c(0,1))
  p <- p + scale_colour_manual()
  p <- p + theme_void()
  p <- p + theme(
    legend.position = 'none'
  )
  return(p)
}
