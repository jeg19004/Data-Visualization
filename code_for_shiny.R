#necessary packages
library(bnlearn)
library(gRain)
library(gRbase)
library(scales)

getwd()
#setwd(getSrcDirectory()[1])
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("~/Documents/JAPPL-2020-00332_Feld_scripts")

#diagnosis 
general_info <- read.csv(file = "data_and_text/general_diagnosis.csv")
input_info <- read.csv(file = "data_and_text/input_diagnosis.csv")
output_info <- read.csv(file = 'data_and_text/output_diagnosis.csv')
#prognosis
rev_general_info <- read.csv(file = "data_and_text/general_prognosis.csv")
rev_input_info <- read.csv(file = "data_and_text/input_prognosis.csv")
rev_output_info <- read.csv(file = 'data_and_text/output_prognosis.csv')

##==================================================
popup_plot_textandimage <- cbind(as.character(output_info$html_file_name),as.character(output_info$image_name),as.character(output_info$image_source))

path_to_network_file <- as.character(general_info[general_info[,1] == "network_file_name",2])
path_to_network_file <- paste("data_and_text/network_file/",path_to_network_file,sep = "")
#nodes to watch
nodes_for_observation <- as.character(output_info$node_name)
#real names of nodes to watch
rchartlabs <- as.character(output_info$node_names_to_show)
#which class in each parameter to be considered for radar chart  

for_plot <- as.numeric(as.character(output_info$class_to_use_in_radar_chart))


#nodes to be set  (data)
evidence <- as.character(input_info$node_name)
input_question <- as.character(input_info$question)
input_help <- as.character(input_info$help_entry)
input_choices <- c()
for(i in 1:nrow(input_info)){
  temp <- input_info[i,]
  temp <- temp[!is.na(temp)]
  temp <- temp[-c(1:4)]
  temp <- c(temp, NA, "Unknown")
  temp_mat <- matrix(data = temp,nrow = 2)
  temp <- temp_mat[1,]
  names(temp) <- temp_mat[2,]
  input_choices <- c(input_choices, list(temp))
}
#max value for threshold slider (in percent)
max_threshold_scale <- as.numeric(as.character(general_info[general_info[,1] == "threshold_slider_max",2]))
min_threshold_scale <- as.numeric(as.character(general_info[general_info[,1] == "threshold_slider_min",2]))
default_threshold_scale <- as.numeric(as.character(general_info[general_info[,1] == "threshold_slider_default",2]))


title_panel <- as.character(general_info[general_info[,1] == "main_tab_title",2])
window_title <- ""
output_window_title <- as.character(general_info[general_info[,1] == "radar_chart_title",2])
barplot_exp <- as.character(output_info$barplot_explanation)

rev_popup_plot_textandimage <- cbind(as.character(rev_output_info$html_file_name),as.character(rev_output_info$image_name),as.character(rev_output_info$image_source))
rev_evidence <- as.character(rev_input_info$node_name)
rev_nodes_for_observation <- as.character(rev_output_info$node_name)
rev_rchartlabs <- as.character(rev_output_info$node_names_to_show)
rev_for_plot <- as.numeric(as.character(rev_output_info$class_to_use_in_radar_chart))
rev_input_question <- as.character(rev_input_info$question)
rev_input_help <- as.character(rev_input_info$help_entry)
rev_input_choices <- c()
for(i in 1:nrow(rev_input_info)){
  temp <- rev_input_info[i,]
  temp <- temp[!is.na(temp)]
  temp <- temp[-c(1:4)]
  temp <- c(temp, NA, "Unknown")
  temp_mat <- matrix(data = temp,nrow = 2)
  temp <- temp_mat[1,]
  names(temp) <- temp_mat[2,]
  rev_input_choices <- c(rev_input_choices, list(temp))
}
#rev_input_choices <- "" # to be made automatically at the end
rev_max_threshold_scale <- as.numeric(as.character(rev_general_info[rev_general_info[,1] == "threshold_slider_max",2]))
rev_min_threshold_scale <- as.numeric(as.character(rev_general_info[rev_general_info[,1] == "threshold_slider_min",2]))
rev_default_threshold_scale <- as.numeric(as.character(rev_general_info[rev_general_info[,1] == "threshold_slider_default",2]))
rev_window_title <- ""
rev_title_panel <- as.character(rev_general_info[rev_general_info[,1] == "main_tab_title",2])
rev_output_window_title <- as.character(rev_general_info[rev_general_info[,1] == "radar_chart_title",2])
rev_barplot_exp <- as.character(rev_output_info$barplot_explanation)
##==================================================
##if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

##BiocManager::install("graph")
#loading network 
library(graph)
library(gRain)
install.packages("gRbase")
BiocManager::install("gRbase")
BiocManager::install("Rgraphviz")
original_net <- bnlearn::read.net(file = path_to_network_file) 
#as.grain
grain_net <- bnlearn::as.grain(x = original_net)
#compile network
grain_net <- gRbase::compile(object = grain_net)


priors <- gRain::querygrain(object = grain_net, nodes = nodes_for_observation, type = "marginal")
priors <- priors[nodes_for_observation]
rev_priors <- gRain::querygrain(object = grain_net, nodes = rev_nodes_for_observation, type = "marginal")
rev_priors <- rev_priors[rev_nodes_for_observation]

temp <- rep(NA, length(evidence))
temp <- as.list(temp)
names(temp) <- evidence
evidence <- temp

temp <- rep(NA, length(rev_evidence))
temp <- as.list(temp)
names(temp) <- rev_evidence
rev_evidence <- temp

posteriors <- gRain::querygrain(object = grain_net, nodes = nodes_for_observation, type = "marginal", evidence = evidence)
rev_posteriors <- gRain::querygrain(object = grain_net, nodes = rev_nodes_for_observation, type = "marginal", evidence = rev_evidence)

#functions
plot_post_bar <- function(prior, post,scale = 0.1,title = "",asp = 5){ #scale percent of diffenerce between p and p for saturation
  att <- attributes(prior)
  dif <- post - prior
  dif[which(abs(dif) < 1e-5)] <- 0
  n <- att$dim
  dx <- (1/(2*n))
  x <- seq(from = 0, to = 1, length.out = n+1)[-1] - dx
  
  cols <- rep("lightblue",n)
  cols[which(dif < 0)] <- "red"
  par(lwd = 3)
  #barplot(dif,col = cols,ylab = "Difference in probability",ylim = c(-scale,scale),main = title,cex.axis = 1,cex = 1,cex.main = 2,cex.lab = 1)
  barplot(dif,col = cols,ylab = "Difference in probability",
          #ylim = c(-scale,scale),
          main = title,cex.axis = 1,cex = 1,cex.main = 2,cex.lab = 1)
  abline(h=0,col = scales::alpha("black",0.3))
  #box()
  # 
  # #base plot
  # plot(-1,xlim = c(0,1),ylim = c(-1,1),xpd = T, xaxs = "i",yaxs = 'i',axes = F,xlab = "",ylab = "",main = title,cex.main = 2.5)
  # box()
  # #sfsmisc::eaxis(side = 1,at = x,labels = att$dimnames[[1]],at.small = F)
  # cr <- colorRamp(colors = c("blue","white","red"))
  # cols <- (dif/scale) +  0.5
  # cols[cols > 1] <- 1
  # cols[cols < 0] <- 0
  # cols <- cr(cols)
  # for(i in 1:n){
  #   polygon(x = c(x[i]-dx,x[i]+dx,x[i]+dx,x[i]-dx),
  #           y = c(0,0,1,1),col = rgb(red = cols[i,1],green = cols[i,2],blue = cols[i,3],maxColorValue = 255)  )
  # }
  # text(x = x,y = 0.5,labels = att$dimnames[[1]],cex = 2.5,col = scales::alpha("black",0.5))
  # 
}
plot_post <- function(prior, post,scale = 0.1,title = "title",asp = 5){ #scale percent of diffenerce between p and p for saturation
  att <- attributes(prior)
  dif <- post - prior
  n <- att$dim
  dx <- (1/(2*n))
  x <- seq(from = 0, to = 1, length.out = n+1)[-1] - dx
  #base plot
  plot(-1,xlim = c(0,1),ylim = c(0,1),xpd = T, xaxs = "i",yaxs = 'i',axes = F,xlab = "",ylab = "",main = title,cex.main = 2.5)
  box()
  #sfsmisc::eaxis(side = 1,at = x,labels = att$dimnames[[1]],at.small = F)
  cr <- colorRamp(colors = c("blue","white","red"))
  cols <- (dif/scale) +  0.5
  cols[cols > 1] <- 1
  cols[cols < 0] <- 0
  cols <- cr(cols)
  for(i in 1:n){
    polygon(x = c(x[i]-dx,x[i]+dx,x[i]+dx,x[i]-dx),
            y = c(0,0,1,1),col = rgb(red = cols[i,1],green = cols[i,2],blue = cols[i,3],maxColorValue = 255)  )
  }
  text(x = x,y = 0.5,labels = att$dimnames[[1]],cex = 2.5,col = scales::alpha("black",0.5))
  
}
plot_radar_chart <- function(prior, post, pscale,for_plot, rchartlabs,col = "red"){
  #=============================== posteriors, for_plot, priors, input$threshold, rchartlabs
  labs <- names(post)
  scores <- c()
  for(i in 1:length(post)){
    scores <- c(scores , prior[[i]][for_plot[i]] - post[[i]][for_plot[i]] +0.0)
  }
  names(scores) <- NULL
  scores[scores < 0.00] <- 0
  scores_plot <- list("probability " = 1*scores)
  
  scores_plot <- scores_plot[[1]]
  scores_plot <- rbind(rep(pscale,length(scores_plot)),
                       rep(0,length(scores_plot)),
                       scores_plot)
  colnames(scores_plot) <- labs
  rownames(scores_plot) <- NULL
  scores_plot <- as.data.frame(scores_plot)
  #chartJSRadar(scores = scores_plot, labs = labs, maxScale = scale)
  
  fmsb::radarchart(df = scores_plot,
                   maxmin = T,
                   axistype = 0,
                   centerzero = T,
                   seg  = 3,
                   plwd = 2,
                   pfcol = scales::alpha(col,0.25),
                   cglcol = scales::alpha("black",0.35),
                   cglty = 1,
                   cglwd = 1,
                   axislabcol = "gray",title = "",
                   vlcex = 1.2,
                   caxislabels = "thth",vlabels = rchartlabs)
  #======================
}
table_radar_chart <- function(prior, post, pscale,for_plot, rchartlabs,columnames){
  #=============================== posteriors, for_plot, priors, input$threshold, rchartlabs
  labs <- names(post)
  scores <- c()
  for(i in 1:length(post)){
    scores <- c(scores , prior[[i]][for_plot[i]] - post[[i]][for_plot[i]] +0.0)
  }
  names(scores) <- NULL
  scores[scores < 0.00] <- 0
  scores_plot <- list("probability " = 1*scores)
  
  scores_plot <- scores_plot[[1]]
  scores_plot <- rbind(rep(pscale,length(scores_plot)),
                       rep(0,length(scores_plot)),
                       scores_plot)
  colnames(scores_plot) <- labs
  rownames(scores_plot) <- NULL
  scores_plot <- as.data.frame(scores_plot)
  #chartJSRadar(scores = scores_plot, labs = labs, maxScale = scale)
  result <- as.vector(t(scores_plot[3,]))
  result <- as.data.frame(result)
  result <- cbind(rchartlabs,result)
  rownames(result) <- NULL
  colnames(result) <- columnames
  result <- result[order(result[,2],decreasing = T),]
  result[,2] <- result[,2] * 100
  return(result)
  # fmsb::radarchart(df = scores_plot,maxmin = T,axistype = 2,centerzero = T,
  #                  seg  = 3,
  #                  plwd = 2,
  #                  pfcol = scales::alpha("red",0.15),
  #                  cglcol = "gray",
  #                  cglty = 1,
  #                  cglwd = 1,
  #                  axislabcol = "gray",title = "",
  #                  vlcex = 1.2,
  #                  caxislabels = "thth",vlabels = rchartlabs)
  #======================
}
clicked_lab <- function(rad_click,labs){
  labs_click <- rep(F,length(labs))
  n <- length(labs_click)
  r <- sqrt((rad_click$x)^2 + (rad_click$y)^2)
  
  if(rad_click$y >= 0) {
    phi <- atan2(y = rad_click$y,x = rad_click$x) * 180 / pi
  }else{
    phi <- 360 + (atan2(y = rad_click$y,x = rad_click$x) * 180 / pi)
  }
  
  phi <- (phi - 90 + (360/(2*n))) %% 360
  
  if(r > 0 && r < 100) labs_click[ceiling(phi / (360/n))] <- T
  print(paste(rad_click$x, rad_click$y, r, phi, ceiling(phi / (360/n)),labs_click,sep = "  |  "))
  return(labs_click)
}

#necessary files 
variables_for_shiny <- list(nodes_for_observation = nodes_for_observation,
                            rchartlabs = rchartlabs,
                            evidence = evidence,
                            title_panel = title_panel,
                            window_title = window_title,
                            priors = priors,
                            posteriors = posteriors,
                            input_question = input_question,
                            input_choices = input_choices,
                            max_threshold_scale = max_threshold_scale,
                            min_threshold_scale = min_threshold_scale,
                            default_threshold_scale = default_threshold_scale,
                            output_window_title = output_window_title,
                            for_plot = for_plot,
                            labs = names(priors),
                            popup_plot_textandimage = popup_plot_textandimage,
                            input_help = input_help,
                            barplot_exp = barplot_exp,
                            
                            rev_rchartlabs = rev_rchartlabs,
                            rev_evidence = rev_evidence,
                            rev_title_panel = rev_title_panel,
                            rev_window_title = rev_window_title,
                            rev_priors = rev_priors,
                            rev_posteriors = rev_posteriors,
                            rev_input_question = rev_input_question,
                            rev_input_choices = rev_input_choices,
                            rev_max_threshold_scale = rev_max_threshold_scale,
                            rev_min_threshold_scale = rev_min_threshold_scale,
                            rev_default_threshold_scale = rev_default_threshold_scale,
                            rev_output_window_title = rev_output_window_title,
                            rev_for_plot = rev_for_plot,
                            rev_labs = names(rev_priors),
                            rev_nodes_for_observation = rev_nodes_for_observation,
                            rev_popup_plot_textandimage = rev_popup_plot_textandimage,
                            rev_input_help = rev_input_help,
                            rev_barplot_exp = rev_barplot_exp
                            )


save(list = c("variables_for_shiny","grain_net","plot_post","plot_post_bar","plot_radar_chart","clicked_lab","table_radar_chart"),
     file = ("/Users/jessicagutierrez/Documents/JAPPL-2020-00332_Feld_scripts/data_for_shiny_catchment1.Rdata"))
#save(list = c("grain_net","nodes","evidence","priors","plot_post","plot_post_bar"),file = "catch_2_bar/data_for_shiny_catchment.Rdata")

