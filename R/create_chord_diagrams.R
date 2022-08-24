# source: https://jokergoo.github.io/circlize_book/book/
# https://www.data-to-viz.com/graph/chord.html
# circos.ca/documentation/tutorials/ideograms/

#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
library("circlize") # for circular layout
library("RColorBrewer") # for importing colors
suppressPackageStartupMessages(library(ComplexHeatmap))# for legend
library(dplyr)
library(stringr)
# ==============================================================


########################################################################
# It splits a given text into two parts for the sake of readibility in the plot.
# This function is used for formatting a long text >> making 2-line displaying in the plot.
# It splits a text based on a slash or a space.
#
########################################################################
split.text.into.two.parts = function(x){
    pos = which(strsplit(x, "")[[1]]=="/")[1]
    if(is.na(pos))
        pos = which(strsplit(x, "")[[1]]==" ")[1]
        if(is.na(pos))  
            pos = nchar(x)/2
    parts = substring(x, c(1,pos), c(pos-1,nchar(x)));
    return(parts)
}



########################################################################
# It retrieves unique values of the column 'type_source' of two datasets.
# The retrieved information will be mainly used for coloring pruposes, i.e.
#   assigning the same colors to the same source types in both two figures.
#
########################################################################
retrieve.group.names = function(info.csv.filepath1){
    df.info1 = read.csv(file=info.csv.filepath1, sep=";", header=T, check.names=F, stringsAsFactors=F)
    groups = unique(c(df.info1$type_source))
    return(groups)
}



########################################################################
# It prepares the colors list for 'type_source'.
#
########################################################################
# create.colors.group.association = function(groups){
#     colors = brewer.pal(n = length(groups), name = "RdBu")
#     colors[6] = "khaki3"
#     colors = c(colors, "green", "cyan", "gray") # extend the color list
#     names(colors) = unique(groups)
#     return(colors)
# }

create.colors.group.association = function(groups){
 # colors = brewer.pal(n = length(groups), name = "Dark2")
  colors <- c(brewer.pal(7,'Dark2'), brewer.pal(name="Paired", n = length(groups)-7))
 # colors = c(colors, "green", "cyan", "gray") # extend the color list
  names(colors) = unique(groups)
  return(colors)
}
########################################################################
# It retrieves unique values of the column 'geographical_focus' of two datasets.
# The retrieved information will be mainly used for coloring pruposes, i.e.
#   assigning the same colors to the same geographic focus types in both two figures.
#
########################################################################
retrieve.geo.focus = function(info.csv.filepath1){
    df.info1 = read.csv(file=info.csv.filepath1, sep=";", header=T, check.names=F, stringsAsFactors=F)
    geo.focus = unique(c(df.info1[,"geographical_focus"]))
    geo.focus[which(geo.focus == "")] = "unspecified"
    return(geo.focus)
}



########################################################################
# It prepares the colors list for 'geographical_focus'.
#
########################################################################
create.colors.geo.focus.association = function(geo.focus){
    colors = c("blueviolet","chartreuse3", "burlywood4", "yellow")
    names(colors) = unique(geo.focus)
    return(colors)
}





########################################################################
# It retrieves unique values of the column 'specialization' of two datasets.
# The retrieved information will be mainly used for coloring pruposes, i.e.
#   assigning the same colors to the same specialization types in both two figures.
#
########################################################################
retrieve.specialization = function(info.csv.filepath1, info.csv.filepath2){
    df.info1 = read.csv(file=info.csv.filepath1, sep=";", header=T, check.names=F, stringsAsFactors=F)
    df.info2 = read.csv(file=info.csv.filepath2, sep=";", header=T, check.names=F, stringsAsFactors=F)
    specialization = unique(c(df.info1[,"specialization"],  df.info2[,"specialization"]))
    specialization[which(specialization == "")] = "unspecified"
    return(specialization)
}

retrieve.specialization = function(info.csv.filepath1){
  df.info1 = read.csv(file=info.csv.filepath1, sep=";", header=T, check.names=F, stringsAsFactors=F)
  specialization = unique(df.info1[,"specialization"])
  specialization[which(specialization == "")] = "unspecified"
  return(specialization)
}

########################################################################
# It prepares the colors list for 'specialization'.
#
########################################################################
create.colors.specialization.association = function(specialization){
    colors = c("pink","goldenrod2","yellow")
    names(colors) = unique(specialization)
    return(colors)
}





########################################################################
# Link 1: https://jokergoo.github.io/circlize_book/book/advanced-usage-of-chorddiagram.html#compare-two-chord-diagrams
# For comparability reasons, we need to adjust the segment widths of the second plot based on those of the first plot.
#   Unfortunately, we cannot use the solution proposed in Link1, because there is some overlaps between the first and second columns of our data frame.
#   That is why we need to do a manual calculation of the relative gap.
#
# The idea: we sum up all gap degrees between adjacent segments. If we substract this sum from 360, this gives us the total degree remained for the segments.
#   Let us have a = (the total degree remained for the segments of the first plot)/(the total degree remained for the segments of the second plot)
#   Let us also have b = (the total link weights in the first plot) / (the total link weights in the second plot)
#   Finally, the condition a = b must hold, for the sake of comparability.
#
########################################################################
calculate.relative.big.gap = function(padiweb.links.csv.filepath, healthmap.links.csv.filepath, small.gap, big.gap){
    df.links.padiweb = read.csv(file=padiweb.links.csv.filepath, sep=";", header=T, check.names=F, stringsAsFactors=F)
    vars1.padiweb = df.links.padiweb[,"Var1"]
    names(vars1.padiweb) = df.links.padiweb[,"Group1"]
    vars2.padiweb = df.links.padiweb[,"Var2"]
    names(vars2.padiweb) = df.links.padiweb[,"Group2"]
    vars.padiweb = c(vars1.padiweb,vars2.padiweb)
    vars.padiweb = vars.padiweb[!duplicated(vars.padiweb)]

    df.links.healthmap = read.csv(file=healthmap.links.csv.filepath, sep=";", header=T, check.names=F, stringsAsFactors=F)
    vars1.healthmap = df.links.healthmap[,"Var1"]
    names(vars1.healthmap) = df.links.healthmap[,"Group1"]
    vars2.healthmap = df.links.healthmap[,"Var2"]
    names(vars2.healthmap) = df.links.healthmap[,"Group2"]
    vars.healthmap = c(vars1.healthmap,vars2.healthmap)
    vars.healthmap = vars.healthmap[!duplicated(vars.healthmap)]

    sum.weight.padiweb = sum(df.links.padiweb[,"value"])
    sum.weight.healthmap = sum(df.links.healthmap[,"value"])
    group.freq.padiweb = table(names(vars.padiweb))
    group.freq.healthmap = table(names(vars.healthmap))
    sum.gap.padiweb = sum(group.freq.padiweb - 1)*small.gap + length(group.freq.padiweb)*big.gap
    sum.gap.healthmap = sum(group.freq.healthmap - 1)*small.gap
    percent = sum.weight.healthmap / sum.weight.padiweb
    sum.tracks.padiweb = 360 - sum.gap.padiweb
    sum.tracks.healthmap = sum.tracks.padiweb * percent
    result = (360 - sum.tracks.healthmap - sum.gap.healthmap)/length(group.freq.healthmap)
    return(result)
}



########################################################################
# It prepares a specific csv file structure for plotting a chord diagram, and writes into file.
# Note that it also handles the empty cells.
#
########################################################################
prepare.input.for.chord.diagram = function(edges.csv.filepath, info.csv.filepath, links.filepath){

    df.edges = read.csv(file=edges.csv.filepath, sep=";", header=T, check.names=F, stringsAsFactors=F)
    df.info = read.csv(file=info.csv.filepath, sep=";", header=T, check.names=F, stringsAsFactors=F)
    df.info = df.info %>% mutate(label = str_trim(label))
    # ================================================================

    df = df.edges[,c("Source","Target", "weight")] %>% mutate(Var1 = str_trim(Target)) %>% mutate(Var2 = str_trim(Source)) %>% 
      rename(value=weight)%>% select(Var1, Var2, value)

    df <- merge(df, df.info, by.x = "Var1", by.y = "label", all.x = T) %>% select(-id) %>% 
      rename(Group1=type_source) %>% 
      rename(Focus1=geographical_focus) %>%
      rename(Specialization1=specialization) 
    
    df <- merge(df, df.info, by.x = "Var2", by.y = "label", all.x = T) %>% select(-id) %>% 
      rename(Group2=type_source) %>% 
      rename(Focus2=geographical_focus) %>%
      rename(Specialization2=specialization) 
    
   # df <-df %>% filter(!Group1=="local person" & !Group2=="local person")

    #df$Group1 = sapply(df$Group1, function(x){ parts = split.text.into.two.parts(x); return(paste0(parts[1],"\n",parts[2]))}) # make 2-line texts for the sake of visibility in the plot
   # df$Group2 = sapply(df$Group2, function(x){ parts = split.text.into.two.parts(x); return(paste0(parts[1],"\n",parts[2]))}) # make 2-line texts for the sake of visibility in the plot
    
  #  groups.rm = sapply(c("laboratory", "local person", "private/company", "social platform", "national official authority"), function(x){ parts = split.text.into.two.parts(x); return(paste0(parts[1],"\n",parts[2]))}) # make 2-line texts for the sake of visibility in the plot
    
  #  df.links <- df.links %>% dplyr::filter(!Group1 %in% groups.rm)
   # df.links <- df.links %>% dplyr::filter(!Group2 %in% groups.rm)
    write.table(df, links.filepath, sep=";", row.names=F, col.names=T)
}




########################################################################
# It reads the csv file created by the function 'prepare.input.for.chord.diagram()', and plots the chord diagram with grouping information.
#  Currently, there are 3 grouping information: 1) source types, 2) geographic focus, 3) specialization
#
########################################################################
create.chord.diagram = function(links.filepath, output.filepath, output.nodes.legend, output.df.diagram, colors.for.groups, small.gap, big.gap, include_legend = T, label = "A)"){

    # ========================================================================================
    # prepare the data: combine both data frames
    # ========================================================================================

    df.links = read.csv(file=links.filepath, sep=";", header=T, check.names=F, stringsAsFactors=F)
    uniques = unique(c(df.links[,c("Var1")],df.links[,c("Var2")]))
    vertex.ids = seq(1,length(uniques))
    names(vertex.ids) = uniques
    df.links[,c("Var1")] = as.character(vertex.ids[df.links[,c("Var1")]])
    df.links[,c("Var2")] = as.character(vertex.ids[df.links[,c("Var2")]])

    df1 = df.links[,c("Var1","Group1","Focus1","Specialization1","value")]
    df2 = df.links[,c("Var2","Group2","Focus2","Specialization2","value")]
    names(df1) = c("Var","Group","Focus","Specialization","value")
    names(df2) = c("Var","Group","Focus","Specialization","value")
    df.new = rbind(df1,df2)
    levels = c( "EBS\n tool", "international\n vet auth.","national\n vet auth.","local\n vet auth." ,
                  "national\n off. auth.", "local\n off. auth.", "press\n agency",  "social\n platform", "radio,\n TV",
                  "online\n news source" , "labo\nratory"  ,"research\n org.", "local\n person"  )
    levels <- levels[levels%in%unique(df.new$Group)]
    df.group = aggregate(df.new$value, by=list(Var=df.new$Var, Group=df.new$Group, Focus=df.new$Focus, Specialization=df.new$Specialization), FUN=sum)
    df.group = df.group[order(df.group$Group,df.group$Focus,df.group$Specialization),]
    group = structure(df.group$Group, names = df.group$Var)
  
    grid.col.for.group = colors.for.groups[df.group$Group]
    names(grid.col.for.group) = df.group$Var
    
    # ========================================================================================
    # plot chord diagram
    # ####========================================================================================

   # pdf(output.filepath)
    png(output.filepath, units="in", width=10, height=10, res=300)
    
    # directional = 1,
    dfDiagram <- chordDiagram(df.links[,c("Var1","Var2","value")], group = group, grid.col = grid.col.for.group, preAllocateTracks = list(list(track.height = 0.05),list(track.height = 0.05),list(track.height = 0.1),list(track.height = 0.025)), annotationTrack = "grid", small.gap = small.gap, big.gap = big.gap)

    circos.track(track.index = 4, panel.fun = function(x, y) {
        circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, col = "black", cex=0.33,
                    # facing = "bending.inside", niceFacing = TRUE,
                    facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5)
    )
    }, bg.border = NA) # here set bg.border to NA is important

    text(-0.8, 0.8, label, cex = 1.5)

   for(i in 1:length(df.group$Group)){
       g = df.group$Group[i]
        highlight.sector(sector.index = df.group$Var[which(df.group$Group == g)], track.index = 3, col = grid.col.for.group[i], text = "", cex = 0.6, text.col = "black")
    }
    
    # ================
    # create a legend 
    # ================
    if (include_legend) {
      lgd2 = Legend(at = names(colors.for.groups), type = "points", legend_gp = gpar(col = colors.for.groups, lwd = 6), 
                    labels_gp = gpar(fontsize = 13),
                    grid_height = unit(6, "mm"),
                    grid_width = unit(10, "mm"), 
                    size = unit(5, "mm"), 
                    nrow = 3, ncol = 5,
                    title_gp = gpar(fontsize = 13, fontface = "bold"), title_position = "topleft", title = "Type of sources:")
     # draw(lgd2, x = unit(1, "mm"), y = unit(4, "mm"), just = c("left", "bottom"))
      draw(lgd2, x = unit(0.5, "npc"), y = unit(0.05, "npc"))
    }

    dev.off()
    circos.clear()
    
    
    # ===============================
    # Export the legend of nodes id
    # ===============================
    dfLegendNodes <- data.frame("id" = as.character(vertex.ids), "source" = uniques)
    dfDiagram <- merge(dfDiagram, dfLegendNodes, by.x = "rn", by.y = "id")
    colnames(dfDiagram)[ncol(dfDiagram)] <- "receptor_name"
    dfDiagram <- merge(dfDiagram, dfLegendNodes, by.x = "cn", by.y = "id", all.x = T)
    colnames(dfDiagram)[ncol(dfDiagram)] <- "emitter_name"
    dfDiagram <- dfDiagram %>% rename(receptor_id = rn, emitter_id = cn, value = value1)%>% 
      select(receptor_id, emitter_id, receptor_name, emitter_name, value)
    write.table(dfLegendNodes, output.nodes.legend, sep=";", row.names = F, col.names = T)
    write.table(dfDiagram, output.df.diagram, sep=";", row.names = F, col.names = T)

}

