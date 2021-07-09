process_image <- function(image_file_name, k_list) {
  ## process_image performs k-means clustering on an image given a list of
  ## initial k number of clusters, and then collects the cluster centers
  ## obtained by k-means for each cluster, their associated RGB values, and 
  ## their nearest DMC thread color information, and stores it in a large list.
  ##
  ## Input:
  ## - image_file_name: a PNG or JPEG image
  ## - k_list: a set of the initial k number of clusters in the clustering
  ##
  ## Output:
  ## - A large list of four elements: the values of k in k_list stored as k, a 
  ##   list of the k-means output for each k stored as kclusts, a list of the 
  ##   tidied clusters for each k and their associated RGB values and nearest 
  ##   DMC thread color information stored as list_centers, and the data set of 
  ##   the image file.
  ##
  ## Example:
  ## library(imager) 
  ## library(tidyverse) 
  ## library(tidymodels) 
  ## library(sp) 
  ## library(scales)
  ## library(cowplot) 
  ## library(dmc)
  ##
  ## cluster_info <- process_image("/Desktop/iron man.jpg", c(1, 2, 5))
  
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  
  if(!require(scales)) {
    stop("The scales packages must be installed. Run install.packages(\"scales\") and then try again.")
  }
  
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }
  
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  
  
  im <- imager::load.image(image_file_name)
  
  tidy_dat <- as.data.frame(im, wide = "c")%>%
    rename(R = c.1, G = c.2, B = c.3)
  
  dat <- select(tidy_dat,c(-x,-y))
  
  kclusts <- c()
  list_centers <- c()
  
  for (value in k_list) {
    kclust = kmeans(dat, centers = value, nstart = 4)
    kclusts = append(kclusts, value=list(kclust))
    centers = tidy(kclust)
    centers = centers %>% mutate(col = rgb(R,G,B))
    centers = centers %>% mutate(dmc = map(centers$col,~dmc(.x)))
    list_centers = append(list_centers, value=list(centers))
    
  }
  
  cluster_info <- list("k" = k_list,
                       "kclusts" = kclusts,
                       "list_centers" = list_centers,
                       "tidy_dat" = tidy_dat)
  
  cluster_info
}

scree_plot <- function(cluster_info) {
  ## scree_plot produces and plots a screeplot using the kmeans output for a
  ## list of initial k clusters.
  ##
  ## Input:
  ## - cluster_info: A large list of four elements: the initial k cluster
  ##   centers stored as k, a list of the k-means output for each k stored as
  ##   kclusts, a list of the tidied clusters for each k and their associated
  ##   RGB values and nearest DMC thread color information stored a
  ##   list_centers, and the data set of the image file.
  ##
  ## Output:
  ## - A plot of the k-means objective function as a function of the number of
  ##   clusters. The x-axis of the plot is the number of clusters and the y-axis
  ##   is the total within sum of squares variability for each cluster.
  ##
  ## Example:
  ## library(imager) 
  ## library(tidyverse) 
  ## library(tidymodels) 
  ## library(sp) 
  ## library(scales)
  ## library(cowplot) 
  ## library(dmc)
  ##
  ## plotted <- scree_plot(cluster_info)
  
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  
  if(!require(scales)) {
    stop("The scales packages must be installed. Run install.packages(\"scales\") and then try again.")
  }
  
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }
  
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  
  cluster_info_subset <- cluster_info$kclusts
  
  tot.withinSS <- c()
  
  for (i in cluster_info_subset) {
    tot.withinSS = append(tot.withinSS, i["tot.withinss"])
  }
  
  tot.withinSS <- unlist(tot.withinSS)
  
  k <- c()
  
  for (i in cluster_info$k) {
    k = append(k, i)
  }
  
  tot.withinSS <- as.data.frame(tot.withinSS)
  tot.withinSS <- setNames(tot.withinSS, c("tot.withinss"))
  tot.withinSS['Number of Clusters']  = k
  
  ggplot(tot.withinSS, 
         aes(x = tot.withinSS$`Number of Clusters`, 
             y = tot.withinSS$tot.withinss)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    labs(x = 'Number of Clusters (k)', y = 'Total Within-Cluster Sum of Squares', 
         title = 'Scree plot for k-means Cluster Analysis')
  
}

color_strips <- function(cluster_info) {
  ## color_strips produces color strips with the DMC color closest to the
  ## cluster center color.
  ##
  ## Input:
  ## - cluster_info: A large list of four elements: the initial k cluster
  ##   centers stored as k, a list of the k-means output for each k stored as
  ##   kclusts, a list of the tidied clusters for each k and their associated
  ##   RGB values and nearest DMC thread color information stored a
  ##   list_centers, and the data set of the image file.
  ##
  ## Output:
  ## - A list of plots corresponding to the color strips with the DMC color
  ##   closest to the cluster center color for each k stored in cluster_info.
  ##
  ## Example:
  ## library(imager) 
  ## library(tidyverse) 
  ## library(tidymodels) 
  ## library(sp) 
  ## library(scales)
  ## library(cowplot) 
  ## library(dmc)
  ##
  ## strips <- color_strips(cluster_info)
  
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  
  if(!require(scales)) {
    stop("The scales packages must be installed. Run install.packages(\"scales\") and then try again.")
  }
  
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }
  
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  
  square <- function(x, label_size) { 
    ggplot()  + 
      coord_fixed(xlim=c(0,1), ylim = c(0,1)) + theme_void() + 
      theme(plot.background = element_rect(fill = x)) + 
      geom_text(aes(0.5,0.5),label = x , size = label_size)
  }
  
  data_subset <- cluster_info$list_centers
  
  list_hex <- c()
  
  for(i in data_subset) {
    hex = c()
    for (j in i$dmc) {
      hex = append(hex, (j$hex))
    }
    list_hex = append(list_hex, list(hex))
  }
  
  for (i in list_hex) {
    t <- tibble(colours = i,
                squares = purrr::map(colours, ~ square(.x, 24/length(colours))))
    
    print((plot_grid(plotlist = t$squares)))
    
  }
  
}

change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}

make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, 
                         background_color = NULL) {
  ## make_pattern produces a cross-stitch pattern of an image complete with a
  ## legend that has the DMC thread color name and number, and a guide grid.
  ## This function makes use of the change_resolution function in order to
  ## obtain a lower resolution of the image 
  ##
  ## Input:
  ## - cluster_info: A large list of four elements: the initial k cluster
  ##   centers stored as k, a list of the k-means output for each k stored as
  ##   kclusts, a list of the tidied clusters for each k and their associated
  ##   RGB values and nearest DMC thread color information stored a
  ##   list_centers, and the data set of the image file.
  ##
  ## - k: An integer value for the chosen cluster size
  ##
  ## - x_size: The approximate total number of possible stitches in the
  ##   horizontal direction. The number of cells in the vertical direction will
  ##   be computed to maintain the perspective. There is no guarantee that the
  ##   exact number of cells in the x-direction is x_size
  ##
  ## - black_white: Logical value (TRUE or FALSE). Prints the pattern in black
  ##   and white (TRUE) or color (FALSE, default)
  ##
  ## - background_color: Hex code corresponding to the background color, which
  ##   should not be stitched in the pattern. Default is NULL, so the background
  ##   color is stitched in the pattern.
  ##
  ## Output:
  ## - A cross-stitched pattern of an image with the specified k number of
  ##   clusters, complete with a legend that has the DMC thread color name and
  ##   number, and a guide grid. Default is to produce a colored pattern without
  ##   the background color (black_white = FALSE and background_color = NULL).
  ##
  ## Example:
  ## library(imager) 
  ## library(tidyverse) 
  ## library(tidymodels) 
  ## library(sp) 
  ## library(scales)
  ## library(cowplot) 
  ## library(dmc)
  ##
  ## cross_stitch <- make_pattern(cluster_info, k = 5, x_size = 50, black_white = FALSE, 
  ##                                          background_color = NULL)
  ##
  
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  
  if(!require(scales)) {
    stop("The scales packages must be installed. Run install.packages(\"scales\") and then try again.")
  }
  
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }
  
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  
  index = 0
  counter = 0
  
  for (i in cluster_info$k) {
    counter = counter + 1
    if (i == k) {
      index = counter
    }
  }
  
  kclust = cluster_info$kclusts[[index]]
  
  data_subset = cluster_info$list_centers
  
  list_hex = c()
  list_names = c()
  list_dmc_numbers = c()
  
  for(i in data_subset) {
    hex = c()
    name = c()
    dmc = c()
    for (j in i$dmc) {
      hex = append(hex, j$hex)
      name = append(name, j$name)
      dmc = append(dmc, j$dmc)
    }
    list_hex = append(list_hex, list(hex))
    list_names = append(list_names, list(name))
    list_dmc_numbers = append(list_dmc_numbers, list(dmc))
  }
  
  augmented_data = augment(kclust, cluster_info$tidy_dat) %>%
    rename(cluster = .cluster)
  
  low_res = change_resolution(augmented_data, x_size)
  
  color_frame = tibble(cluster = factor(c((1:k))), 
                       name = c(list_names[[index]]),
                       number = c(list_dmc_numbers[[index]]),
                       hex = c(list_hex[[index]]))
  
  color_frame$name_and_number = paste(color_frame$name, ":", color_frame$number)
  
  if (is.null(background_color)) {
    color_frame = color_frame
    low_res = low_res
  }
  
  else{
    cluster_number = 0
    counter2 = 0
    for (i in color_frame$hex){
      counter2 = counter2 + 1
      if (i == background_color) {
        cluster_number = counter
      }
    }
    color_frame = color_frame[color_frame$hex != background_color, ]
    low_res = low_res[low_res$cluster != cluster_number, ]
  }
  
  if (black_white == TRUE) {
    low_res %>%
      ggplot(aes(x=x, y = y)) +
      geom_point(aes(shape = factor(cluster))) +
      scale_shape_manual(name = "Cluster",
                         values = color_frame %>% select(cluster, cluster)
                         %>% deframe,
                         label = color_frame %>% select(cluster, name_and_number)
                         %>% deframe) +
      scale_y_reverse() + theme_void() + background_grid(major = "xy",
                                                         color.major = "black")
  }
  
  else{
    
    low_res %>%
      ggplot(aes(x=x, y = y, color = cluster)) +
      geom_point(aes(col = factor(cluster), shape = factor(cluster))) +
      scale_colour_manual(name = "Cluster",
                          values =  color_frame %>% select(cluster, hex)
                          %>% deframe,
                          label = color_frame %>% select(cluster, name_and_number)
                          %>% deframe) +
      scale_shape_manual(name = "Cluster",
                         values = color_frame %>% select(cluster, cluster)
                         %>% deframe,
                         label = color_frame %>% select(cluster, name_and_number)
                         %>% deframe) +
      scale_y_reverse() + theme_void() + background_grid(major = "xy", 
                                                         color.major = "black")
  }
  
}