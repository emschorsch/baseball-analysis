# High Density Scatterplot with Binning
library(hexbin)
hex_plot <- function(x1, y1=NULL){
  bin<-hexbin(x1, y1, xbins=30) 
  plot(bin, main="Hexagonal Binning")
}


#assume x and y are indexed the same
mean_plot <- function (x, y){
  x_level <- c()
  mean_y <- c()
  freq <- as.data.frame(table(x))$Freq
  for( x_val in as.data.frame(table(x))$x ){
    x_level <- c(x_level, as.numeric(x_val))
    mean_y <- c(mean_y, mean( y[which(x==x_val)] ))
  }
  plot(x_level, mean_y)
  symbols(x=x_level, y=mean_y, circles=sqrt(freq/pi), 
          inches=1/3, ann=F, bg="steelblue2", fg=NULL)
  cor(mean_y, x_level)
}

mean_plot(data2$total, data2$run_total)

mean_density_plot <- function(x, y){
  #Plot the density functions of y against different x levels
  par(mfrow=c(2,2))
  
  x_levels <- as.data.frame(table(x))$x
  if( length(x_levels) > 30 ){
    stop("Too many x_levels to plot")
  }
  for( x_val in x_levels ){
    qualified <- y[which(x==x_val)]
    if( length(qualified) > 15){
      plot(density(qualified), sub=x_val)
    }
  }
  par(mfrow=c(1,1))
}

#Old code

#Plot the density functions of run_total against different ou levels
par(mfrow=c(2,2))
for( ou in temp_data ){
  qualified<-subset(data2, total==ou)
  if( length(qualified$run_total) > 15){
    plot(density(qualified$run_total), sub=ou)
  }
}
par(mfrow=c(1,1))


#the mean run level predicted by ou total
temp_data <- c()
mean_runs <- c()
for( ou in as.data.frame(table(data2$total))$Var1 ){
  temp_data <- c(temp_data, as.numeric(ou))
  mean_runs <- c(mean_runs, mean(subset(data2, total==ou)$run_total))
}
temp_data
mean_runs
#Really high correlation
plot(temp_data[1:13], mean_runs[1:13])
