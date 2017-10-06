#________________________________________________________________
# check for outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
#________________________________________________________________

#________________________________________________________________
# boxplot with outlier labels
p_box_outliers <- function(df, grep_str, scale = 1){
 # select data
  df <- subset(df, select =  c(grep(grep_str, colnames(df), value=TRUE),"date", "id"))

 # rename columns
  names(df) <- gsub(grep_str, "", colnames(df))
    
 # melt and group
  df <- melt(df,  id.vars = c("date","id"), variable.name = "series")
  df <- group_by(df, series)

 # add outlier experment id
  df <- mutate(df, outlier = ifelse(is_outlier(value), as.character(id), NA))
    
 # scale
  df$value <- df$value/scale

 # plot
  p <- ggplot(df, aes(x = series, y = value)) +
       geom_boxplot() +
       theme_bw() +
       geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 0, nudge_x = 0.3, size = 4)
    
 # return plot
  return(p)
}
#________________________________________________________________

#________________________________________________________________
# stacked bar
p_stacked_bar <- function(df, grep_str = "^time_.*[^0-9]$", scale = 1){
 # subset data
  df_times <- subset(df, select =  grep(grep_str, colnames(df), value=TRUE))/scale

 # number of columns  
  cols <- ncol(df_times)
    
 # convert times
  for(i in cols:2){
    df_times[,i] <- (df_times[,i] - df_times[,i-1])
  }
  
  df_times[,1] <- 0
    
  df_times$date <- df$date
    
  df_times$id <- df$id
  
 # rename columns
  names(df_times) <- gsub("^time_", "", colnames(df_times))

 # melt and group
  df_times <- melt(df_times,  id.vars = c("date","id"), variable.name = "series")
    
  df_times <- group_by(df_times, series)
  
 # plot
  p <- ggplot(df_times, aes(x = id, y = value, fill = series)) +
       geom_bar(stat = "identity", colour = "black", position = "stack") + 
       theme_bw()
      
 # return plot
  return(p)
}
#________________________________________________________________

#________________________________________________________________
# stacked bar
p_stacked_bar_date <- function(df, grep_str = "^time_.*[^0-9]$", scale = 1){
 # subset data
  df_times <- subset(df, select =  grep(grep_str, colnames(df), value=TRUE))/scale

 # number of columns  
  cols <- ncol(df_times)

 # convert times
  for(i in cols:2){
    df_times[,i] <- (df_times[,i] - df_times[,i-1])
  }

  df_times[,1] <- 0
  
  df_times$date <- as.factor(df$date)
  
 # rename columns
  names(df_times) <- gsub("^time_", "", colnames(df_times))
  
 # melt and group
  df_times <- melt(df_times,  id.vars = c("date"), variable.name = "series")
  
  df_times <- group_by(df_times, series)
  
 # plot
  p <- ggplot(df_times, aes(x = date, y = value, fill = series)) +
       geom_bar(stat = "identity", colour = "black", position = "stack") + 
       theme_bw()

 # return plot
  return(p)
 }
#________________________________________________________________

#________________________________________________________________

lm_r2 <- function(eqn, data){
  m <- lm(eqn, data = data)
  eq <- substitute(~~R^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 2)))
  eq
}
#________________________________________________________________

#________________________________________________________________
# plot isee bar charts
# returns plot object
isee_bar_charts <- function(data,
                            x_var = "stove", x_lab = "",
                            y_var = "value", y_lab = "",
                            facet_var = "stovecat",
                            fill_var = "pol",
                            colors = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")){
  
  ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = q1, ymax = q3), width=.2) +
    theme_bw() +
    theme(#axis.text.x = element_text(vjust = 0.9, hjust = 0.9),
          text = element_text(size = 20),
          legend.title = element_blank(),
          legend.text = element_text(size = 24),
          strip.text.x = element_text(size = 20),
          axis.title.y = element_text(size = 22),
          legend.position = "top") +
    guides(fill = guide_legend(title=NULL)) +
    scale_fill_manual(values = colors) +
    facet_grid(~data[[facet_var]], scales = "free", space = "free_x") +
    ylab(y_lab) +
    xlab(x_lab)

}
