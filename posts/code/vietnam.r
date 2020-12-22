library(ggplot2)
#data <- read.csv('/home/ethan/data/phung_hoang_mangement_information_systems/data.csv')

# Helper functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


arrests_years_df <- function(data){
  arrest_years <- c()
  for (arrest_field in data$ARREST){
    numeric <- as.numeric(arrest_field)
    if (! is.na(numeric)){
      arrest_years <- c(arrest_years, as.numeric(substrRight(arrest_field, 2)))
    }
  }
  df <- as.data.frame(arrest_years)
  colnames(df) <- 'year'
  ggplot(df, aes(x=as.factor(year))) + geom_histogram(stat='count',color="black", fill="tan") + theme_minimal() + labs(x="Year", y="Number of Arrests")
}

opstat_plots <- function(data){
  opinfo <- data$OPINFO
}

slice_group_encoding <- function(column, names){
  splits <- list()
  i <- 1
  for (c in column){
    splits[[i]] <- strsplit(c, "")[[1]]
    i <- i + 1
  }
  splits <- do.call(rbind, splits)
  colnames(splits) <- names
  return(as.data.frame(splits))  
}

translate <- function(column, encoding_list){
  
  l <- list()
  
  for (i in 1:length(column)){
    l[[i]] = encoding_list[[column[[i]]]]
  }
  l <- as.data.frame(cbind(l))
}

title_bold <- function(){
  
}

colors <- function(){
  
  c(
    '#3F464D',
    "#71BAA9",
    "#DFDFAF",
    "#F2D676",
    "#196267",
    "#FAB560",
    "#493B6E",
    "#8D9790",
    "#E54B47",
    "#493B6E",
    "#8697A5",
    "#437BA9",
    "#B6735B",
    "#74472E",
    "#E39B2B",
    "#B79E96",
    "#CBA55C",
    "#FD7D67"
  )
  
}

opsec_by_force <- function(opinfo){
  
  ggplot(subset(opinfo, STATUS != " " & FORCE %in% names(table2())), aes(x=STATUS, fill=STATUS)) + 
    geom_bar(stat="count") + 
    facet_wrap(FORCE ~ ., labeller = as_labeller(table2A())) + 
    theme_minimal() + 
    theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      text = element_text(size=15, face="bold")
      ) +
    labs(x="", y="Number of Individuals") + 
    scale_fill_manual(values=colors(), labels=table1(), name='Status')
}

dsa_by_force <- function(data){
  names <- c("STATUS","TARGET","LEVEL","FORCE","TYPE","DETFAC")
  working_data <- slice_group_encoding(data$OPINFO, names)
  sadatx <- data$SADATX
  working_data <- cbind(working_data, sadatx)
  working_data <- subset(working_data, working_data$sadatx != "NA")
  working_data <- wd %>% mutate(sadatx=convert_date(sadatx))
  
  ggplot(wd, aes(x=sadatx, fill=FORCE), alpha=0.6) + 
    geom_density(stat = "count", alpha=0.6) + 
    scale_fill_manual(values=colors(), labels=table2(), name='Capturing Force') + 
    labs(x="", y="Number of Individuals") + 
    theme_minimal() + 
    theme(text=element_text(size=15, face="bold")) + 
    facet_wrap(~FORCE, labeller = as_labeller(table2A())) + 
    theme(legend.position = "none")
}

convert_dates <- function(dates){
  ds <- c()
  for (d in dates){
    
    ds <- c(ds, convert_date(d)) 
  }
  ds
}

convert_date <- function(d){
  d1 <- as.character(d)
  as.Date(paste0(d1, "01"), format="%y%m%d")
  
}


table1 <- function(){
  
  list(
    "K" = "Killed",
    "C" = "Captured",
    "R" = "Rallied",
    "1" = "Sentanced to < 6 Months",
    "2" = "Sentanced to > 6 Months < 1 Year",
    "3" = "Sentanced to > 1 Year < 2 Years",
    "4" = "Sentanced to > 2 Years",
    "E" = "Released as a result of processing",
    "F" = "Referred to military court",
    "G" = "Referred to civilian court",
    "H" = "Referred to another provice",
    "I" = "Banished",
    "M" = "Drafted",
    "P" = "Classified as POW",
    "T" = "Classified ad Hoi Chang",
    "U" = "Unaccounted",
    "W" = "Released before processing",
    "X" = "Died after capture",
    "Z" = "Escaped",
    "O" = "Other",
    " " = "Missing Data"
  )
}

table2 <- function(){
  
  list(
    "A" = "FWMAP, Other than US",
    "B" = "US Forces",
    "C" = "ARVN Main Forces",
    "D" = "Regional Forces",
    "E" = "Popular Forces",
    "F" = "Civilian Irregular Defense Group",
    "G" = "Provincial Reconnaissance Unit",
    "H" = "Rural Devlopment Cadre",
    "I" = "People's Self Defense Force",
    "J" = "Armed Propaganda Team",
    "K" = "Military Security Force",
    "L" = "National Police Force",
    "M" = "National Police",
    "N" = "Special Police",
    "O" = "Chieu Hoi Cadre",
    "P" = "Other"
  )
}

table2A <- function(){
  
  c(
    `A` = "FWMAP, Other than US",
    `B` = "US Forces",
    `C` = "ARVN Main Forces",
    `D` = "Regional Forces",
    `E` = "Popular Forces",
    `F` = "Civilian Irregular Defense Group",
    `G` = "Provincial Reconnaissance Unit",
    `H` = "Rural Devlopment Cadre",
    `I` = "People's Self Defense Force",
    `J` = "Armed Propaganda Team",
    `K` = "Military Security Force",
    `L` = "National Police Force",
    `M` = "National Police",
    `N` = "Special Police",
    `O` = "Chieu Hoi Cadre",
    `P` = "Other"
  )
}


force_labeler <- function(value){
  print(value)
  value$FORCE <- as.character(table2())
  
}




# status plot 1
# p <- ggplot(opinfo, aes(x=STATUS)) + geom_bar(color="black", fill="#538f54") + theme(axis.text.x=element_text(color = "black", size=11, angle=45, vjust=.8, hjust=0.8)) + scale_x_discrete(labels=table1()) + labs(y="Number of Individuals", x="") + theme_minimal()
