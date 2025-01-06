## ----simple, echo=TRUE-------------------------------

dp.plotter <- function(data, categories, xvar, yvar, xlab, ylab, xlim, ylim, title){
  #determine whether categorize data
  if(is.null(categories)){
    plot(data[[xvar]],data[[yvar]], xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title, col ='black',pch=19, cex=0.65)
    lmodel <- lm(data[[yvar]] ~ data[[xvar]])
    abline(a=data[[xvar]], b=data[[yvar]], col='black', lty=3, lwd=2)
  }
  else{
    dp.cat_plotter(data, categories, xvar, yvar, xlab, ylab, xlim, ylim, title)
  }
}

dp.cat_plotter <- function(data, categories, xvar, yvar, xlab, ylab, xlim, ylim, title){
  #dynamically generate title
  categories_title <- paste(categories, collapse=' and ')
  title <- paste(title, categories_title)
  plot(0,0, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title, cex.main = 1, col ='white')
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
  legend(0.85*xlim[2],ylim[2], legend=human_uniq, col=pal, lty=3, lwd=3, cex=0.8)
}
