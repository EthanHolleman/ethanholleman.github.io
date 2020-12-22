library(ape)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

read_tree <- function(){
  read.tree(file='cog_global_tree.newick')
}

genomes_by_country <- function(tree){
  countries <- get_countries_from_tip_labels(tree$tip.label)
  countries <- as.data.frame(unlist(countries))
  colnames(countries) <- c("Countries")
  colors <- get_n_colors(length(tree$tip.label))
  #print(colors)
  ggplot(countries, aes(x=Countries, fill=Countries)) + geom_bar(stat="count", position = position_dodge(width=0.5)) +
    scale_fill_manual(values=colors) + theme_minimal() + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.85)) + 
    scale_y_continuous(trans='log2') + labs(x="", y="Number Sequenced Genomes")

}

plot_tree <- function(tree, file="cog_tree_by_country.png"){
  png(file, width=5000, height=5000)
  cm <- color_mapping_by_country(tree$tip.label)
  tip_colors <- tip_color_list(tree, cm)
  tc <- rep('black', Nedge(tree))
  tc[which(tree$edge[, 2] %in% 1:length(tip_colors))] <- tip_colors

  plot(tree, type="fan", no.margin=TRUE, show.tip.label=FALSE,
        show.node.label=FALSE, edge.color = tc)
  legend("topleft", fill = unlist(cm), legend = names(cm),
         cex=3)
  dev.off()
}

tip_color_list <- function(tree, color_mapping){
  tip_list <- list()
  countries <- get_countries_from_tip_labels(tree$tip.label)
  for (i in 1:length(countries)){
    tip_list[[i]] <- color_mapping[[countries[[i]]]]
  }
  unlist(tip_list)
}

get_country_from_tip_label <- function(tip_label){
  strsplit(tip_label, "/")[[1]][1]
}

get_n_colors <- function(n){
  # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r/33144808
  colors <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  colors[1] <- colors[length(colors)]
  colors
}

get_countries_from_tip_labels <- function(tip_labels){
  countries <- list()
  for (i in 1:length(tip_labels)){
    countries[i] <- get_country_from_tip_label(tip_labels[i])
  }
  countries
}

color_mapping_by_country <- function(tip_labels){
  countries <- get_countries_from_tip_labels(tip_labels)
  countries.uniq <- unique(countries)
  colors <- get_n_colors(length(countries.uniq))
  color_mapping <- list()
  for (j in 1:length(countries.uniq)){
    color_mapping[countries.uniq[[j]]] = colors[[j]]
  }
  color_mapping
}

legend_from_color_mapping <- function(color_mapping){
  legend(1, 95, col = unlist(color_mapping), legend = names(cm))
}

convert_date <- function(d){
  d1 <- as.character(d)
  as.Date(d1, format="%Y-%M-%D")
}

read_cog_metadata <- function(metadata_file){
  df <- as.data.frame(read.csv(metadata_file))
  df <- df %>% mutate(sample_date=as.Date(sample_date))
}

top_n_lineages <- function(lineages, n=10){
  counts <- list()
  lineages.uniq <- unique(lineages)
  #print(lineages)
  for (l in lineages.uniq){
    counts[[l]] = sum(lineages == l)
  }
  counts <- counts[order(unlist(counts), decreasing=TRUE)]
  counts[1:n]
}

lineages_over_time <- function(metadata_file, lineages){
  df <- read_cog_metadata(metadata_file)
  df <- subset(df, df$lineage %in% lineages)
  ggplot(df, aes(x=sample_date, fill=lineage)) + theme_minimal() + 
    geom_density(stat="count", alpha=0.6)  + 
    theme(
      text=element_text(size=15, face="bold"),
      plot.margin = unit(c(1,1,1,1), "cm")) + 
    theme(legend.position = "None") + facet_wrap(~lineage) + 
    labs(x="", y="Genome count")  
}