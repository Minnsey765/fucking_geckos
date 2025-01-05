## ----simple, echo=TRUE-------------------------------

dp.plotter <- function(data, column, xvar, yvar, xlab, ylab, xlim, ylim, title){
  #determine whether categorize data
  if(is.null(column)){
    plot(data[[xvar]],data[[yvar]], xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title, col ='black',pch=19, cex=0.65)
    lmodel <- lm(data[[yvar]] ~ data[[xvar]])
    abline(a=data[[xvar]], b=data[[yvar]], col='black', lty=3, lwd=2)
  }
  else{
    dp.cat_plotter(data, column, xvar, yvar, xlab, ylab, xlim, ylim, title)
  }
}

dp.cat_plotter <- function(data, column, xvar, yvar, xlab, ylab, xlim, ylim, title){
  plot(0,0, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, main=title, col ='white')
  #find unique values in specified column
  unique_vals <- unique(data[[column]])
  #generate colour palette equal to number of unique vals
  pal <- c(1:length(unique_vals))
  i <- 1
  #iterate through unique_vals and plot data
  for( q in unique_vals){
    colour <- pal[i]
    subset <- data[data[, column]==q,]
    points(subset[[xvar]], subset[[yvar]], col=colour,pch=19, cex=0.65)
    #line of best fit
    lmodel <- lm(subset[[yvar]] ~ subset[[xvar]])
    
    #ignore line draw if gradient is infinite
    if(!is.na(coef(lmodel)[2])){
      b <- coef(lmodel)[2]
      a <- coef(lmodel)[1]
      abline(a=a, b=b, col=colour, lty=3, lwd=2)
    }
    
    
    #increment colour
    i <- i +1
  }
  legend(0.9*xlim[2],ylim[2], legend=unique_vals, col=pal, lty=3, lwd=3, cex=0.8)
}
