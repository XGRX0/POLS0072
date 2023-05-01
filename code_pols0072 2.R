
## Preliminaries ===============================================================

## empty environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, haven, vdemdata, caTools, ggpubr, ISLR2, boot, statnet, igraph)

## file_path
base_path <- file.path ("~/Desktop/UCL/POLS0072")



## Building data ===============================================================

## Ethnicity of Refugees dataset -----------------------------------------------

## loading in raw data 
er_raw <- read.csv (file.path (base_path, "ER-2021.csv"))

## cleaning data
er <- er_raw %>%
  
  # recoding countries with their ISO3 codes
  dplyr::mutate (coa = countrycode (coa, "country.name", "iso3c"),
                 coo = countrycode (coo, "country.name", "iso3c")) %>%
  
  # removing NAs in countries
  dplyr::filter (!(is.na (coa))) %>%
  dplyr::filter (!(is.na (coo))) %>%
  
  # keep only relevant vars 
  dplyr::select (year, coa, coo, totalrefugees, groupname1, groupname2, groupname3)

## Transnational Ethnic Kin dataset --------------------------------------------

## loading in raw data 
tek_raw <- read.csv (file.path (base_path, "TEK-2021.csv"))

## cleaning data
tek <- tek_raw %>%
  
  # keep only relevant vars 
  dplyr::select (c("statename", "groupname")) %>%
  
  # recoding countries with their ISO3 codes
  dplyr::mutate (statename = ifelse (statename == "Czechoslovakia", "CZE", 
                                     ifelse (statename == "German Democratic Republic", "DEU",
                                             ifelse (statename == "Korea, People's Republic of", "KOR", 
                                                     ifelse (statename == "Kosovo", "Serbia", 
                                                             ifelse (statename %in% c("Yemen (Arab Republic of Yemen)", "Yemen, People's Republic of"), "YEM", 
                                                                     countrycode (statename, "country.name", "iso3c")
                                                             )))))) %>%
  dplyr::filter (!(is.na (statename))) %>%
  
  # namually adding Roma after breaking up Czechoslovakua into CZE
  rbind (c("SVK", "Roma")) %>%
  
  # renaming pre-join
  dplyr::rename_with (
    ~ case_when (
      . == "statename" ~ "coa",
      . == "groupname" ~ "ethnic_grp",
      TRUE ~ .
    )
  ) %>%
  
  # reformatting to have one row = onw country
  dplyr::group_by (coa) %>% 
  dplyr::summarize(ethnic_grp = paste(ethnic_grp, collapse = ", "))

## Combined data with ethnic linkage binary var --------------------------------

dat1 <- er %>%
  
  # creating year-coo-coa dyad variable
  unite ("orig_dest_year", c(coo, coa, year), sep = "_", remove = F, na.rm = F) %>%
  
  # joining ER and TEK 
  dplyr::left_join (tek, by = "coa") %>%
  
  # filtering out problematic values
  dplyr::filter (!grepl('/', groupname1)) %>%
  dplyr::filter (!is.na (ethnic_grp)) %>%
  
  # fixing issues
  dplyr::mutate (groupname2 = ifelse (groupname2 == "Mandingue (and other eastern groups", "Mandingue", groupname2)) %>%
  dplyr::mutate (groupname1 = ifelse (groupname1 == "Konkomba (Northern groups", "Konkomba", groupname1))

## filling in empty spaces with NAs
grourvars <- c("groupname1", "groupname2", "groupname3")
for (i in grourvars) {
  
  dat1[,i] <- ifelse (dat1[,i] == "", NA, dat1[,i])
  
}

## computing ethnic linkgage variables 
for (i in 1:nrow (dat1)) {
  
  dat1$eth_link_1 [i] <- ifelse (grepl (dat1$groupname1 [i], dat1$ethnic_grp [i]), 1, 0)
  dat1$eth_link_2 [i] <- ifelse (grepl (dat1$groupname2 [i], dat1$ethnic_grp [i]), 1, 0)
  dat1$eth_link_3 [i] <- ifelse (grepl (dat1$groupname3 [i], dat1$ethnic_grp [i]), 1, 0)
  
}

## final cleaning 
dat1 = dat1 %>%
  
  # renaming vars 
  dplyr::rename_with (
    ~ case_when (
      . == "coa" ~ "dest",
      . == "coo" ~ "orig",
      TRUE ~ .
    )
  ) %>%
  
  # adding orig-dest pair variable
  unite ("orig_dest", c(orig, dest), sep = "_", remove = F, na.rm = F)





## Converting data into network ================================================

## cleaning data
data <- dat1 %>%
  
  # isolating vars
  dplyr::select (year, orig, dest, totalrefugees, eth_link_1, eth_link_2, eth_link_3) %>%
  
  # turn NAs into 0s
  dplyr::mutate (eth_link_1 = ifelse (is.na (eth_link_1), 0, eth_link_1), 
                 eth_link_2 = ifelse (is.na (eth_link_2), 0, eth_link_2), 
                 eth_link_3 = ifelse (is.na (eth_link_2), 0, eth_link_2)) %>%
  
  # computing single ethnic linkage var per year
  dplyr::group_by (orig, dest, year) %>%
  dplyr::summarise (link = ifelse (eth_link_1 == 1 | eth_link_2 == 1 | eth_link_3 == 1, 1, 0)) %>%
  
  # computing summary linkage var 
  dplyr::group_by (orig, dest) %>%
  dplyr::summarise (link = mean (link, na.rm = T)) %>%
  dplyr::mutate (link = ifelse (link == 0.00000000, 0, 1))

## vector containing the states
states = unique (c (as.character (data$orig), as.character (data$dest)))

## creating empty matrix
datMat <- matrix (0,nrow = length (states), ncol = length (states))
rownames(datMat) <- states
colnames(datMat) <- states

## filling the matrix
for (i in 1:dim (data)[1]){
  datMat [as.character (data$orig[i]), as.character (data$dest[i])] <-
    datMat [as.character (data$orig[i]), as.character(data$dest[i])] + 1
}

## converting diagnoal to 0
diag (datMat) <- 0 

## converting to network
network <- network (datMat, directed = T)
network.vertex.names (network) <- states

## indegree centrality 
in.n <- sna::degree (network, cmode = "indegree")
centrality <- data.frame (states = states,
                          indegree = in.n)





## Creating plot (Figure 2) ====================================================

library (igraph)

## node characteristics
nodes = data.frame (states = states,
                    continent = NA) %>%
  dplyr::mutate (continent = countrycode (states, "iso3c", "continent")) %>%
  dplyr::left_join (centrality, by = "states") %>%
  dplyr::mutate (colour = ifelse (continent == "Asia", "#C84630", 
                                  ifelse (continent == "Africa", "#5DA271",
                                          ifelse (continent == "Europe", "#D4A0A7",
                                                  ifelse (continent == "Americas", "#898989",
                                                          ifelse (continent == "Oceania", "#E3E3E3", NA    
                                                          ))))))

## color pallete
colours = c ("#C84630", "#5DA271", "#D4A0A7", "#898989", "orange")
colours = c ("#BD4E2B", "#6785A1", "#FD9D01", "#05525D", "#FECA77") # diss

## creating igraph object
g1 <- graph_from_adjacency_matrix (datMat, mode = "directed", diag = F)

## creating separate objects with continent classifications
asia = unique (nodes$states [nodes$continent == "Asia"])
africa = unique (nodes$states [nodes$continent == "Africa"])
europe = unique (nodes$states [nodes$continent == "Europe"])
americas = unique (nodes$states [nodes$continent == "Americas"])
oceania = unique (nodes$states [nodes$continent == "Oceania"])

## mapping colour onto vertices 
V(g1)$color <- ifelse (V(g1)$name %in% asia, "#C84630",
                       ifelse (V(g1)$name %in% africa, "#5DA271",
                               ifelse (V(g1)$name %in% europe, "#D4A0A7",
                                       ifelse (V(g1)$name %in% americas, "#898989", "orange"))))
## plotting
plot (g1,
      frame = T,
      main = "Figure 2: Refugee-specific Ethnic Linkage Network",
      edge.arrow.size = 0.2, 
      vertex.size = igraph::degree (g1, mode = "in"),
      vertex.label.cex = 0.6,
      vertex.label.color = "black",
      edge.curved = .2)

## adding legend
legend ("bottomleft", 
        legend = c("Asia", "Africa", "Americas", "Europe", "Oceania"),
        col = colours, 
        bty = "n", 
        pch=20 , 
        pt.cex = 3, 
        cex = 1.5, 
        text.col = colours, 
        horiz = FALSE, 
        inset = c(0.1, 0.1))


## creating subgraph of CEECs

# subgraph
europe_plot <- igraph::subgraph (graph = g1, 
                          vids = which (V(g1)$name %in% europe))

# change tie colour to show reciprocity
E(europe_plot)$arrow.color <- ifelse (igraph::is.mutual (europe_plot), "red", "grey")

# plot
plot (europe_plot,
      frame = T,
      main = "Figure 3: Sub-graph showing reciprocity in the CEE region",
      edge.arrow.size = 0.2, 
      #vertex.size = igraph::degree (europe_plot, mode = "in"),
      vertex.label.cex = 0.6,
      vertex.label.color = "black",
      edge.curved = .2,
      edge.color = E(europe_plot)$arrow.color)

## computing network statistics ================================================

# edge density 
edge_density (g1)

# transitivity 
transitivity (g1)

# reciprocity 
reciprocity (g1)











