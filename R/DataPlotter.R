## ----simple, echo=TRUE-------------------------------

dp.plotter <- function(data, categories, xvar, yvar, xlab, ylab, xlim, ylim, title){
  #determine whether categorize data
  if(is.null(categories)){
    plot(data[[xvar]],data[[yvar]], xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title, col ='black',pch=19, cex=0.65, las=1)
    lmodel <- lm(data[[yvar]] ~ data[[xvar]])
    #print(lmodel)
    abline(a=coef(lmodel)[1], b=coef(lmodel)[2], col='black', lty=3, lwd=2)
  }
  else{
    dp.cat_plotter(data, categories, xvar, yvar, xlab, ylab, xlim, ylim, title)
  }
}

dp.cat_plotter <- function(data, categories, xvar, yvar, xlab, ylab, xlim, ylim, title){
  #dynamically generate title
  categories_title <- paste(categories, collapse=' and ')
  title <- paste(title, categories_title)
  plot(0,0, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title, cex.main = 1, col ='white', las=2)
  #find unique values in specified column
  unique_vals <- unique(data[categories])

  category_num <- length(unique_vals[,1])
  #make readable version for legend
  human_uniq <- vector(mode="character",length=category_num)
  #generate colour palette equal to number of unique vals
  pal <- c(1:category_num)
  i <- 1
  #iterate through unique_vals and make subsets based on category columns
  for( q in 1:category_num){
    #initalise subset as all the data
    subset <- data
    #initalise the key as empty string
    key <- ""
    for(category in categories){
      #extract first unique value
      uniq_row <- unique_vals[q,]
      #deals with unique_vals being a dataframe or a vector (depends on length of categories)
      if(typeof(uniq_row) == "list"){
        cat_val <- uniq_row[category][1,1]
      }
      else{
        cat_val <- uniq_row
      }
      #human readable legend
      key <- paste(key, cat_val)
      subset <- subset[subset[, category]==cat_val,]
    }
    #add key to vector of legible legend names
    human_uniq[q] <- key
    #add appropriate colour
    colour <- pal[i]
    #plot points
    points(subset[[xvar]], subset[[yvar]], col=colour,pch=19, cex=0.65)
    
    #line of best fit
    #only plot lines of best fit for datasets that are not all NAs
    if(!all(is.na(subset[[yvar]])) & !all(is.na(subset[[xvar]]))){
      lmodel <- lm(subset[[yvar]] ~ subset[[xvar]])
      #print(lmodel)
      #ignore line draw if gradient is infinite
      if(!is.na(coef(lmodel)[2])){
        b <- coef(lmodel)[2]
        a <- coef(lmodel)[1]
        abline(a=a, b=b, col=colour, lty=3, lwd=2)
      }
    }
    
    
    #increment colour
    i <- i +1
  }
  legend(0.8*xlim[2],ylim[2], legend=human_uniq, col=pal, lty=3, lwd=3, cex=0.8)
}

#boxplot for comparing gecko size in experimental and control quadrats
dp.boxplot <- function(data, con_var, dis_var, title, xlab, ylab){
  #find unique values in discrete variable column
  #unique_vals <- unique(data[[dis_var]])
  #print(cbind(data[[con_var]],data[[dis_var]]))
  #plot box plot
  box_plot <- boxplot(data[[con_var]]~data[[dis_var]], main=title, cex.main=1, xlab = xlab, ylab=paste(ylab,con_var), par(las=1))
  stripchart(data[[con_var]] ~ data[[dis_var]], method = "jitter", pch = 19, vertical = TRUE, add = TRUE, cex=.65)
  return(boxplot)
}

#boxplot of specific subset
dp.spec_boxplot <- function(data, spec_col, spec_var, con_var, dis_var, title, xlab, ylab){
  subset <- data[data[, spec_col]==spec_var,]
  
  boxplot <- dp.boxplot(subset, con_var, dis_var, title, xlab, ylab)
  #do a t test too
  #t_test <- dp.t_test(subset, con_var, dis_var)
  return(boxplot)
}

# t test function
dp.t_test <- function(data, con_var, dis_var){
  t_test <- t.test(data[[con_var]] ~ data[[dis_var]], data)
  result <- ""
  if(t_test$p.value > 0.05){
    result <- "no"
  }
  else{
    result <- "a significant"
  }
  print(t_test)
  sentence <- paste("There is ", result, " difference between each treatment. (p value =", t_test$p.value, ")")
  return(sentence)
}

dp.kmeans <- function(data, xvar, yvar, clusters){
  #generate palette of correct number of colours
  col_pal <- palette(rainbow(clusters))
  #create column names
  names <- c(xvar,yvar)
  #create new vector matrix for kmeans analysis and remove NAs
  dataframe <- na.omit(cbind(data[[xvar]],data[[yvar]]))
  km.rs <- kmeans(dataframe, clusters, iter.max = 10, nstart = 1)
  
  #convert dataframe into actual dataframe and name columns
  dataframe <- as.data.frame(dataframe)
  colnames(dataframe) <- names
  #plot cluster
  plot <- fviz_cluster(km.rs, data = dataframe,
               palette = col_pal, 
               geom = "point",
               ellipse.type = "convex", 
               ggtheme = theme_bw()
  )
  palette("default")
  return(plot)
  
}


# oneway anova test + diagnostic plots
dp.ow_anova <- function(data, xvar, yvar){
  #make sure data is correct format
  xcol <- data[[xvar]]
  ycol <- as.numeric(data[[yvar]])
  subset <- as.data.frame(cbind(xcol,ycol))
  colnames(subset) <- c("indep","depen")
  subset <- na.omit(subset)
  #perform anova
  res_aov <- aov(depen ~ indep, data = subset)
  
  #assumption plots
  #par(mfrow = c(1, 2))
  plot(res_aov)
  print(paste("indep refers to",xvar))
  return(summary(res_aov))
}

# twoway anova test + diagnostic plots
dp.tw_anova <- function(data, xvar1, xvar2, yvar){
  #make sure data is correct format
  xcol1 <- data[[xvar1]]
  xcol2 <- data[[xvar2]]
  ycol <- as.numeric(data[[yvar]])
  subset <- as.data.frame(cbind(xcol1, xcol2,ycol))
  colnames(subset) <- c("indep1","indep2","depen")
  subset <- na.omit(subset)
  #perform anova
  res_aov <- aov(depen ~ indep1 * indep2, data = subset)
  
  #assumption plots
  #par(mfrow = c(1, 2))
  plot(res_aov)
  print(paste("indep1 refers to", xvar1, " and indep2 refers to", xvar2))
  return(summary(res_aov))
}


# perform t test on 3 groups
trip_t_test <- function(data, dis_var, con_var){
  unique_vals <- unique(data[[dis_var]])
  for(i in 1:length(unique_vals)){
    #omit one value type in discrete column from dataset
    subset <- data[data[, dis_var] != unique_vals[i],]
    #perform t test
    t_test <- t.test(subset[[con_var]] ~ subset[[dis_var]], subset)
    result <- ""
    if(t_test$p.value > 0.05){
      result <- "no"
    }
    else{
      result <- "a significant"
    }
    #mention what categories are being tested
    phrase <- ""
    if(i == 1){
      phrase <- paste(unique_vals[i+1], "&", unique_vals[i+2])
    }
    else if(i == 2){
      phrase <- paste(unique_vals[i-1], "&", unique_vals[i+1])
    }
    else{
      phrase <- paste(unique_vals[i-2], "&", unique_vals[i-1])
    }
    print(t_test)
    sentence <- paste("There is ", result, " difference between the treatments. (", dis_var, " ", phrase, ") (p value =", t_test$p.value, ")")
    print(sentence)
  }
}

#linear regression model + t statistic
dp.regression <- function(data, xvar, yvar){
  lmodel <- lm(data[[yvar]] ~ data[[xvar]])
  plot(lmodel)
  return(summary(lmodel))
}

dp.scale_reg <- function(data, xvar, yvar){
  scaled <- as.data.frame(cbind(data[[xvar]],data[[yvar]]))
  colnames(scaled) <- c("indep", "depen")
  scaled <- scale(scaled)
  print(scaled)
  lmodel <- lm(scaled[[indep]] ~ scaled[[depen]])
  plot(lmodel)
  return(summary(lmodel))
}

dp.trip_kruskal <- function(data, dis_var, con_var){
  unique_vals <- unique(data[[dis_var]])
  for(i in 1:length(unique_vals)){
    #omit one value type in discrete column from dataset
    subset <- data[data[, dis_var] != unique_vals[i],]
    #perform t test
    kw_test <- kruskal.test(subset[[con_var]] ~ subset[[dis_var]], subset)
    result <- ""
    if(kw_test$p.value > 0.05){
      result <- "no"
    }
    else{
      result <- "a significant"
    }
    #mention what categories are being tested
    phrase <- ""
    if(i == 1){
      phrase <- paste(unique_vals[i+1], "&", unique_vals[i+2])
    }
    else if(i == 2){
      phrase <- paste(unique_vals[i-1], "&", unique_vals[i+1])
    }
    else{
      phrase <- paste(unique_vals[i-2], "&", unique_vals[i-1])
    }
    print(kw_test)
    sentence <- paste("There is ", result, " difference between the treatments. (", dis_var, " ", phrase, ") (p value =", kw_test$p.value, ")")
    print(sentence)
  }
}


dp.kruskal <- function(data, con_var, dis_var){
  kw_test <- kruskal.test(data[[con_var]] ~ data[[dis_var]], data)
  result <- ""
  if(kw_test$p.value > 0.05){
    result <- "no"
  }
  else{
    result <- "a significant"
  }
  print(kw_test)
  sentence <- paste("There is ", result, " difference between each treatment. (p value =", kw_test$p.value, ")")
  return(sentence)
}

#spearmens rank

dp.spearman <- function(data, xvar, yvar){
  corr <- cor.test(data[[xvar]], data[[yvar]], method="spearman")
  print(corr)
  if(corr$p.value > 0.05){
    result <- "no"
  }
  else{
    result <- "a significant"
  }
  sentence <- paste("There is ", result, " correllation between each treatment. (p value =", corr$p.value, ")")
  return(sentence)
}

dp.gekdate_plotter <- function(data, zone, yvar, time, xlab, ylab, xlim, ylim, title){
  #xvariable always date here
  xvar <- "date"
  #remove all rows in wrong zone and time
  subset <- na.omit(data[data$zone==zone,])
  subset <- subset[subset$time==time,]
  #Create new dataframe with total number of geckos within each zone on a given date
  sum_11th <- 0
  sum_12th <- 0
  sum_13th <- 0
  sum_14th <- 0
  for(i in 1:length(subset[[xvar]])){
    if(subset[[xvar]][i] == "11/09/2024"){
      sum_11th <- sum_11th + subset[[yvar]][i]
    }
    else if(subset[[xvar]][i] == "12/09/2024"){
      sum_12th <- sum_12th + subset[[yvar]][i]
    }
    else if(subset[[xvar]][i] == "13/09/2024"){
      sum_13th <- sum_13th + subset[[yvar]][i]
    }
    else if(subset[[xvar]][i] == "14/09/2024"){
      sum_14th <- sum_14th + subset[[yvar]][i]
    }
  }
  dates <- c(11,12,13,14)
  abundance <- c(sum_11th,sum_12th,sum_13th,sum_14th)
  new_data <- as.data.frame(cbind(dates,abundance))
  colnames(new_data) <- c("dates", "abundance")
  #boxplot <- boxplot(data[[yvar]]~data$date)#, main=title, cex.main=1, names=unique_vals, xlab = xlab, ylab=paste(ylab,con_var))
  #plot <- dp.cat_plotter(subset, category, xvar, yvar, xlab, ylab, xlim, ylim, title)
  plot <- plot(new_data$dates, new_data$abundance, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title)
  return(plot)
}


dp.mannwhitney <- function(data, con_var, dis_var){
  mwu_test <- wilcox.test(data[[con_var]] ~ data[[dis_var]], data)
  result <- ""
  if(mwu_test$p.value > 0.05){
    result <- "no"
  }
  else{
    result <- "a significant"
  }
  print(mwu_test)
  sentence <- paste("There is ", result, " difference between each treatment. (p value =", mwu_test$p.value, ")")
  return(sentence)
}