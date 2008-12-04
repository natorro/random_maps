# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x, ...){
     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
     title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
       min <- Lst$zlim[1]
       max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
       yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
       xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
       title <- Lst$title
    }
  }
# check for null values
if( is.null(xLabels) ){
   xLabels <- c(1:ncol(x))
}
if( is.null(yLabels) ){
   yLabels <- c(1:nrow(x))
}

layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))

 # Red and green range from 0 to 1 while Blue ranges from 1 to 0
 ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
 ColorLevels <- seq(min, max, length=length(ColorRamp))

 # Reverse Y axis
 reverse <- nrow(x) : 1
 yLabels <- yLabels[reverse]
 x <- x[reverse,]

 # Data Map
 par(mar = c(3,5,2.5,2))
 image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
 ylab="", axes=FALSE, zlim=c(min,max))
 if( !is.null(title) ){
    title(main=title)
 }
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
 axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
 cex.axis=0.7)

 # Color Scale
 par(mar = c(3,2.5,2.5,2))
 image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col=ColorRamp,
      xlab="",ylab="",
      xaxt="n")

 layout(1)
}
# ----- END plot function ----- #

file <- /Users/natorro/Desktop/Batmaps/mexico_matrix.dat

setwd("/Users/natorro/Desktop/Batmaps/")

# We start here the function
random_map <- function(data_file, range_size) {
	   mexico_matrix <- read.table(data_file, header=TRUE)

# Let's generate a matrix full of zeroes
random_matrix <- matrix(0, nrow=175, ncol=175)

# Let's choose initial point

initial_point_passed <- FALSE
initial_point <- sample(175, 2)	

while (initial_point_passed == FALSE){ 
  if (mexico_matrix[initial_point[1], initial_point[2]] == 0)
    initial_point <- sample(175, 2) else {
      random_matrix[initial_point[1], initial_point[2]] <- 1
      initial_point_passed <- TRUE 
    } 
}

# Define vectors x_list and y_list 

x_list <- matrix(0, nrow=range_size, ncol=1)
y_list <- matrix(0, nrow=range_size, ncol=1)

x_list[1] <- initial_point[1]
y_list[1] <- initial_point[2]

# Range counter
range_counter <- 1
flag <- 0

while (sum(random_matrix) < range_size){
	cuadro_aleatorio <- floor((runif(1) * range_counter ) + 1)
	flag <- 0
	while (flag == 0) {
		
		if (mexico_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio]] == 1		||
			mexico_matrix[x_list[cuadro_aleatorio] + 1, y_list[cuadro_aleatorio]] == 1		||
			mexico_matrix[x_list[cuadro_aleatorio],		y_list[cuadro_aleatorio] + 1] == 1		||
			mexico_matrix[x_list[cuadro_aleatorio],		y_list[cuadro_aleatorio] - 1] == 1) {
			
			aleatorio <- sample(c(1, 2, 3, 4, 5, 6, 7, 8), 1)
			
			if (aleatorio == 1) {
				if(mexico_matrix[x_list[cuadro_aleatorio] + 1 , y_list[cuadro_aleatorio] ] == 1 && random_matrix [x_list[cuadro_aleatorio] + 1 , y_list[cuadro_aleatorio]] == 0) {
					random_matrix [x_list[cuadro_aleatorio] + 1 , y_list[cuadro_aleatorio] ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] + 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] 
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}
			
			if (aleatorio == 2) {
				if(mexico_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio] ] == 1 && random_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio] ] == 0) {
					random_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio] ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] - 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] 
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}

			if (aleatorio == 3) {
				if(mexico_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] + 1] == 1 && random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] + 1]  == 0) {
					random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] + 1] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio]
					y_list[range_counter] <- y_list[cuadro_aleatorio] + 1
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}
			
			if (aleatorio == 4) {
				if(mexico_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] - 1] == 1 && random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] - 1]  == 0) {
					random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] - 1] <- 1
					range_counter <- range_counter +1
					x_list[range_counter] <- x_list[cuadro_aleatorio]
					y_list[range_counter] <- y_list[cuadro_aleatorio] - 1
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}



			if (aleatorio == 5) {
				if(mexico_matrix[x_list[cuadro_aleatorio] +1, y_list[cuadro_aleatorio]+1 ] == 1 && random_matrix[x_list[cuadro_aleatorio] +1, y_list[cuadro_aleatorio]+1] == 0) {
					random_matrix[x_list[cuadro_aleatorio] +1, y_list[cuadro_aleatorio]+1 ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] +1
					y_list[range_counter] <- y_list[cuadro_aleatorio] +1
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}

			if (aleatorio == 6) {
				if(mexico_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio] -1] == 1 && random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio] -1] == 0) {
					random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio] -1] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] - 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] - 1
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}

			if (aleatorio == 7) {
				if(mexico_matrix[x_list[cuadro_aleatorio]+1, y_list[cuadro_aleatorio] -1] == 1 && random_matrix[x_list[cuadro_aleatorio]+1, y_list[cuadro_aleatorio] -1] == 0) {
					random_matrix[x_list[cuadro_aleatorio]+1, y_list[cuadro_aleatorio] -1] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] + 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] - 1
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}
			if (aleatorio == 8) {
				if(mexico_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio]+1 ] == 1 && random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio]+1] == 0 ) {
					random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio]+1 ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] - 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] + 1
#					myImagePlot(mexico_matrix + random_matrix)
					flag <- 1
				}
			}
			flag <- 1
		}
	}
}
random_matrix	
# This function just to check everything is working fine
# sum(random_matrix)
 }

