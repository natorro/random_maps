random_richness <- function(data_file, vector_of_range_sizes) {
	number_of_simulations <- length(vector_of_range_sizes)
	random_richness_matrix <- matrix(0, nrow=175, ncol=175)
	
	for (i in 1:number_of_simulations) {	
		temporary_matrix <- random_map(data_file, vector_of_range_sizes[i])
		random_richness_matrix <- random_richness_matrix + temporary_matrix
		temporary_matrix <- matrix(0, nrow=175, ncol=175)
		}
		
	random_richness_matrix

}

random_richness_with_display <- function(data_file, vector_of_range_sizes) {
	map_matrix <- read.table(data_file, header=T)
	number_of_simulations <- length(vector_of_range_sizes)
	random_richness_matrix <- matrix(0, nrow=175, ncol=175)
	
	for (i in 1:number_of_simulations) {	
		temporary_matrix <- random_map(data_file, vector_of_range_sizes[i])
		random_richness_matrix <- random_richness_matrix + temporary_matrix
		temporary_matrix <- matrix(0, nrow=175, ncol=175)
		myImagePlot(map_matrix + random_richness_matrix)
		}
		
	random_richness_matrix

}
