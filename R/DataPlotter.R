## ----simple, echo=TRUE-------------------------------

#make plotting function
dp.mega_plotter <- function(datasets, xvar, yvar, xlab, ylab, xlim, ylim, title, colour){
  #initialise plot with point at (0,0)
  plot(0, 0, xlab = xlab, ylab = ylab, xlim=c(0,xlim), ylim=c(0,ylim),main = title, col = 'red')
  #iterate through datasets
  for (name in names(datasets)){
    #plot points of all datasets in list
    data = datasets[[name]]
    points(data[[xvar]], data[[yvar]], col = colour)
  }
}

dp.multi_plotter <- function(datasets1, datasets2, datasets3, xvar, yvar, xlab, ylab, xlim, ylim, title, colour1, colour2, colour3){
  #call megaplotter function to plot first plot
  dp.mega_plotter(datasets1, xvar, yvar, xlab, ylab, xlim, ylim, title, colour1)
  #check datasets2 exists
  if (is.list(datasets2)){
    #plot 2nd dataset
    for (name in names(datasets2)){
      #plot points of all datasets in list
      data = datasets2[[name]]
      points(data[[xvar]], data[[yvar]], col = colour2)
    }
  }
  else{
    print("no secondary dataset")
  }
  #check datasets3 exists
  if (is.list(datasets3)){
    #plot 2nd dataset
    for (name in names(datasets3)){
      #plot points of all datasets in list
      data = datasets3[[name]]
      points(data[[xvar]], data[[yvar]], col = colour3)
    }
  }
  else{
    print("no secondary dataset")
  }
}

dp.multi_plotter2 <- function(datasets1, oth_datasets, xvar, yvar, xlab, ylab, xlim, ylim, title, colour1, colours){
  #call megaplotter function to plot first plot
  dp.mega_plotter(datasets1, xvar, yvar, xlab, ylab, xlim, ylim, title, colour1)
  #check oth_datasets exists
  if (is.list(oth_datasets)){
    # start a counter for i that counts each dataset list read
    i <- 1
    #iterate through list of dataset lists
    for (i in length(oth_datasets)){
      #extract each dataset list from the list of dataset lists
      datasets <- oth_datasets[[i]]
      for (name in names(datasets)){
        #plot points of all datasets in list
        data = datasets[[name]]
        points(data[[xvar]], data[[yvar]], col = colours[i])
      }
      #increase i
      i <- i + 1
    }
  }
  
  else{
    print("no secondary dataset")
  }
}