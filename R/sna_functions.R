#Extract outbreak data from Empres-i csv file
#Adapted from (c) CIRAD-INRA UMR 117 ASTRE

open_official_AI <- function(xlFile, date_format =  "%Y-%m-%d") {
  rawDF <- read_delim(
    xlFile,
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    progress = F,
    show_col_types = F
  )
  
  colnames(rawDF) <- toupper(colnames(rawDF))
  reference <- as.character(rawDF$ID)
  
  disease <- rawDF$DISEASE
  
  serotype <- as.character(rawDF$SEROTYPES)
  indNoSero <- which(serotype == "null")
  if (!is.na(indNoSero[1]))
    serotype[indNoSero] <- NA
  
  tempDF <-
    data.frame(disease, serotype, reference, stringsAsFactors = FALSE)
  colnames(tempDF) <- c("Disease", "Serotype", "Reference")
  nbRow <- dim(tempDF)[1]
  
  #Retrieve Latitude and Longitude
  tempDF$Latitude <- as.numeric(rawDF$LATITUDE)
  tempDF$Longitude <- as.numeric(rawDF$LONGITUDE)
  
  #Region (continent), country and administrative zone
  tempDF$Region <- rawDF$REGION
  tempDF$Country <- rawDF$COUNTRY
  tempDF$Admin <- rawDF$ADMIN1
  tempDF$Locality <- rawDF$LOCALITYNAME
  
  #If Observation.Date column contains "Unknown" values, data in this column are "character" format
  tempDF$ObservationDate <-
    rawDF$OBSERVATIONDATE %>% as.Date(format = "%Y-%m-%d")
  tempDF$ReportingDate <-
    rawDF$REPORTINGDATE %>% as.Date(format = "%Y-%m-%d")
  
  #Species from Empres-i
  tempDF$Host <- as.character(rawDF$SPECIESDESCRIPTION)
  
  #Domestics species
  dom <-
    which(str_detect(tempDF$Host, "domestic") |
            str_detect(tempDF$Host, "captive"))
  
  # Wild species
  wild <- which(str_detect(tempDF$Host, "wild"))
  
  # Environmental
  envir <- which(str_detect(tempDF$Host, "environmental"))
  
  # Unspecified
  unsp <- which(is.na(tempDF$Host))
  
  tempDF$type_host <- NA
  tempDF$type_host[dom] <- 'domestic'
  tempDF$type_host[wild] <- 'wild'
  tempDF$type_host[envir] <- 'environmental'
  tempDF$type_host[unsp] <- 'unspecified'
  
  
  tempDF$Source <- "FAO Empres-i"
  
  #Cases
  tempDF$at_risk <- rawDF$SUMATRISK
  tempDF$cases <- rawDF$SUMCASES
  tempDF$dead <- rawDF$SUMDEATHS
  tempDF$killed <- rawDF$SUMSLAUGHTERED
  tempDF$destroyed <- rawDF$SUMDESTROYED
  names(tempDF) <- tolower(names(tempDF))
  return(tempDF)
  
}


calc_primary_source <- function(PATHS, SOURCE) {
  primary_sources <- c()
  id_path <- c()
  for (i in as.vector(unique(PATHS$id_path))) {
    primary <- filter(PATHS, id_path == i) %>%
      slice(nrow(filter(PATHS, id_path == i))) %>%
      select(source_2) %>% pull()
    
    primary_sources <- c(primary_sources, primary)
    id_path <- c(id_path, i)
    
  }
  
  tab_primary <-
    merge(
      data.frame(source = primary_sources, id_path = id_path),
      select(SOURCE, label, type_source),
      by.x = 'source',
      by.y = 'label',
      all.x = TRUE,
      all.y = FALSE
    )
  return(tab_primary)
  
}


calc_secondary_source <- function(PATHS, SOURCE) {
  secondary_sources <- c()
  for (i in as.vector(unique(PATHS$id_path))) {
    # we select only path with at least two edges
    n_edges <- nrow(filter(PATHS, id_path == i))
    
    if (n_edges >= 2) {
      secondary <- filter(PATHS, id_path == i) %>%
        slice(nrow(filter(PATHS, id_path == i))) %>%
        select(source_1) %>% pull()
      
      secondary_sources <- c(secondary_sources, secondary)
    }
    
  }
  tab_secondary <- merge(
    data.frame(source = secondary_sources),
    select(SOURCE, label, type_source),
    by.x = 'source',
    by.y = 'label',
    all.x = TRUE,
    all.y = FALSE
  )
  
}



create_df_network <- function(PATHS, SOURCE) {
  PATHS_SNA <- select(PATHS, source_1, source_2)
  
  PATHS_SNA <- merge(
    PATHS_SNA,
    select(SOURCE, label, type_source),
    by.x = 'source_1',
    by.y = 'label',
    all.x = TRUE,
    all.y = FALSE
  ) %>%
    rename(Target_type = type_source) %>% rename(Target = source_1)
  
  PATHS_SNA <- merge(
    PATHS_SNA,
    select(SOURCE, label, type_source),
    by.x = 'source_2',
    by.y = 'label',
    all.x = TRUE,
    all.y = FALSE
  ) %>%
    rename(Source_type = type_source) %>% rename(Source = source_2)
  
  PATHS_SNA <- PATHS_SNA %>% distinct()
  return(PATHS_SNA)
  
}

aggregate_source <-
  function(df, field = "type_source", exclude = NULL) {
    df <- df %>% filter(!type_source %in% exclude)
    df_aggr <- df %>% select(all_of(field)) %>%
      count(across(all_of(field))) %>%
      arrange(desc(n)) %>% mutate(prop = prop.table(n))
    df_aggr
  }


create_sp_focus_plot <-
  function(df_late_pw,
           df_late_hm,
           df_early_pw,
           df_early_hm,
           column,
           levels,
           size_font = 14,
           common_label = "Primary sources") {
    font_param <- element_text(color = 'black',  size = size_font)
    
    df_late <-
      bind_cols(bind_rows(
        bind_cols(df_late_pw, "EBS" = "PADI-web"),
        bind_cols(df_late_hm, "EBS" = "HealthMap")
      ), detection = "Late detection")
    df_early <-
      bind_cols(bind_rows(
        bind_cols(df_early_pw, "EBS" = "PADI-web"),
        bind_cols(df_early_hm, "EBS" = "HealthMap")
      ), detection = "Early detection")
    
    df <- bind_rows(df_late, df_early)
    df <-
      bind_cols(df, variable =  factor(df[, column], levels = levels))
    df <-
      bind_cols(df, common_facet = common_label)
    
    plot_df <- df %>%
      ggplot(aes(
        x = variable,
        y = prop,
        group = EBS,
        fill = EBS
      )) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c(0, 0.25, 0.5, 0.75, 1)
      ) +
      theme(
        axis.text = element_text(color = 'black',  size = size_font - 1),
        axis.title = font_param,
        # legend.position= "none",
        legend.title = font_param,
        legend.text = font_param,
        panel.grid.major = element_blank(),
        strip.text.x = font_param
      ) +
      
      geom_bar(stat = "identity",
               width = 0.5,
               position = "dodge") +# facet_wrap(detection ~ .) + 
      facet_nested(~ common_facet + detection, nest_line = element_line(linetype = 1)) +
    theme(strip.background = element_blank()) +
      scale_fill_manual(values = c("#6b7cb6", "#FF5733")) +
      guides(fill = guide_legend(title = ""))    +
      xlab("") + ylab("Frequency")
    return(plot_df)
    
  }

calc_reactivity <- function(df_paths) {
  reactivity <- c()
  path <- c()
  for (i_path in unique(df_paths$id_path)) {
    tmp <- df_paths %>% filter(id_path == i_path) %>% arrange(id)
    if (nrow(tmp) > 1) {
      r <- as.numeric(difftime(tmp$date_edge[1],
                               tmp$date_edge[nrow(tmp)],
                               units = "days"))
      
      reactivity <- c(reactivity, r)
      path <- c(path, i_path)
    }
  }
  df_reactivity <-
    data.frame("id_path" = path, "reactivity" = reactivity)
  return(df_reactivity)
}

convert_source_plot <-
  function(df1, df2, field, table_convert, exclude = NULL) {
    df1 <- df1 %>% select(all_of(field)) %>%
      count(across(all_of(field))) %>%
      arrange(desc(n)) %>% mutate(prop = prop.table(n))
    
    df1  <-
      merge(df1, table_convert, all.x = TRUE, all.y = TRUE) %>%
      filter(!is.na(type_source)) %>% arrange(desc(n)) %>%
      replace(is.na(.), 0) %>% filter(!type_source %in% exclude)
    
    df2 <- df2 %>% select(all_of(field)) %>%
      count(across(all_of(field))) %>%
      arrange(desc(n)) %>% mutate(prop = prop.table(n))
    
    
    df2 <- merge(df2, table_convert, all.x = TRUE, all.y = TRUE) %>%
      filter(!is.na(type_source)) %>%
      replace(is.na(.), 0)  %>% filter(!type_source %in% exclude)
    
    
    new_order <- df1 %>% pull(name_style)
    
    tab_bind <- bind_rows(df1 %>% mutate(type = "primary"),
                          df2 %>% mutate(type = "secondary")) %>%
      mutate(name_style = factor(name_style, levels = new_order))
    
    tab_bind <- tab_bind %>%
      mutate(prop = if_else(type == "primary", -prop, prop))
    return(tab_bind)
    
  }



create_source_plot <-
  function(df,
           min_prop = 0,
           order_source = NULL,
           position_x_axis = "bottom",
           size_text = 12,
           label = "A)",
           y_axis = T,
           x_label = 10,
           y_label = 0.7,
           legend = T) {
    sum_abs <- function(x) {
      sum(abs(x))
    }
    
    df_filt <-  df %>% group_by(type_source) %>%
      summarize(s = sum_abs(prop))
    source_filt <- pull(filter(df_filt, s >= min_prop), type_source)
    
    df <- df %>% filter(type_source %in% source_filt) %>%
      filter(!is.na(type_source)) #%>% arrange(desc(prop))
    if (!is.null(order_source)) {
      df <-
        df %>%  mutate(name_style = factor(name_style, levels = order_source))
      
    }
    
    if (legend) {
      legend.position <-  c(0.15, 0.91)
    }  else {
      legend.position <- "none"
    }
    if (y_axis) {
      y_axis = "\nType of sources"
    }  else {
      y_axis <- ""
    }
    
    df %>%
      ggplot(aes(
        x = name_style,
        y = prop,
        group = type,
        fill = type
      )) +
      geom_bar(stat = "identity", width = 0.8) +
      scale_y_continuous(breaks = seq(-1, 1, 0.2), labels = abs(seq(-1, 1, 0.2))) +
      scale_x_discrete(position = position_x_axis) +
      annotate(
        "text",
        x = x_label,
        y = y_label,
        label = label,
        size = 5
      ) +
      coord_flip() +
      theme(
        axis.title.x = element_text(size = size_text),
        axis.title.y = element_text(size = size_text),
        axis.text.x = element_text(color = "black", size = size_text -
                                     1),
        axis.text.y = element_text(color = "black", size = size_text -
                                     1),
        panel.grid.major = element_blank(),
        
        axis.line = element_line(colour = "black"),
        legend.position = legend.position,
        legend.text = element_text(colour = "black", size = 11),
        legend.box.background = element_blank(),
        legend.background = element_blank()
      ) +
      guides(fill = guide_legend(title = "Role of sources:")) +
      
      xlab(y_axis) + ylab("\nFrequency") +
      scale_fill_manual(values = c("#CC6666", "#66CC99"))
    
    
  }

create_timeliness_plot <-
  function(df,
           min_timeliness = -35,
           max_timeliness = 30,
           size_font = 14,
           label = "a)") {
    font_param <- element_text(color = 'black',  size = size_font)
    
    
    
    timeliness_plot <-
      ggplot(df %>% filter(timeliness <= max_timeliness) %>%
               filter(timeliness > min_timeliness)) + geom_bar(aes(x = timeliness, y = n, fill = Host),
                                                               stat = "identity",
                                                               width = .7) + scale_fill_manual(values = c("#6b7cb6", "#FF5733", "#4D9F8B", "#6EE0D6")) +
      #c("#E69F00","#56B4E9",  "#00CC66", "#999999")
      xlab("Timeliness (days)") +
      ylab("Frequency of events") +
      scale_y_continuous(limits = c(-0.5, 18)) +
      scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) +
      annotate(
        "text",
        x = -30,
        y = 18,
        label = label,
        size = 7
      ) +
      annotate(
        "text",
        x = -16,
        y = 17,
        label = "Early detection",
        size = 5
      )  +
      geom_hline(yintercept = 0,
                 color = "grey",
                 size = 0.8) +
      geom_vline(
        xintercept = -0.5,
        linetype = "dashed",
        color = "black",
        size = 0.5
      ) +
      geom_segment(aes(
        x = -0.6,
        y = 16,
        xend = -30,
        yend = 16
      ),
      arrow = arrow(length = unit(0.2, "cm"))) +
      theme(
        axis.text = font_param,
        axis.title = font_param,
        # legend.position= "none",
        legend.title = font_param,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.position = c(0.15, 0.85),
        legend.text = font_param,
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(
          size = 0.6,
          linetype = "solid",
          colour = "black"
        )
      )  +
      guides(fill = guide_legend(title = "Host:"))
    return(timeliness_plot)
  }


df_nodes_importance <- function(g, node_remove = NULL) {
  id_vertices <- V(g)
  if (!is.null(node_remove)) {
    id_vertices <-
      id_vertices[!id_vertices %in% c(as.numeric(V(g)[node_remove]))]
    
  }
  
  # Node all degree
  de_all <- igraph::degree(g, mode = "all", v = id_vertices)
  # In and out degree
  de_in <- igraph::degree(g, mode = c("in"), v = id_vertices)
  de_out <- igraph::degree(g, mode = c("out"), v = id_vertices)
  
  # Betweenness
  bet <- igraph::betweenness(g, v = id_vertices)
  
  # Closeness
  close <- igraph::closeness(g, vids = id_vertices, mode = "all")
  
  # Eigenvector centrality
  #  deg_centr <- centr_eigen(g, directed = T)$vector[-c(as.numeric(V(g)[node_remove]))]
  
  df_nodes <- data.frame(
    "in_degree" = as.vector(de_in),
    "out_degree" = as.vector(de_out),
    "all_degree" = de_all,
    "betweeness" = as.vector(bet),
    "closeness" = as.vector(close)
  )
  
  df_nodes <-
    df_nodes %>% dplyr::mutate("source" = row.names(df_nodes))
  return(df_nodes)
  
}
boxplot_sources <- function(nodes_type, type = "in") {
  in_degree_aggr <-
    nodes_type %>% filter(type_source != "local person") %>% select(type_source, in_degree) %>% group_by(type_source) %>%
    mutate(median = median(in_degree)) %>% filter(median > 0) %>% select(type_source, median) %>% unique()
  
  out_degree_aggr <-
    nodes_type %>% filter(type_source != "local person") %>% select(type_source, out_degree) %>% group_by(type_source) %>%
    mutate(median = median(out_degree)) %>% filter(median > 0) %>% select(type_source, median) %>% unique()
  
  all_degree_aggr <-
    nodes_type %>% select(type_source, all_degree) %>% group_by(type_source) %>%
    mutate(median = median(all_degree)) %>% filter(median > 0) %>% select(type_source, median) %>% unique()
  
  
  new_levels <-
    c(
      "international vet auth.",
      "EBS tool",
      "social platform",
      "press agency",
      "online news source",
      "research org.",
      "radio, TV",
      "national vet auth.",
      "laboratory",
      "local off. auth.",
      "local vet auth.",
      "private company",
      "national off. auth."
    )
  
  # Fill colour
  myColors <-
    c(brewer.pal(8, 'Dark2'),
      brewer.pal(name = "Paired", n = length(new_levels) - 8))
  names(myColors) <- new_levels
  
  # Contour colour
  colScale <-
    scale_fill_manual(name = "type_source", values = myColors)
  
  myColorsContourIn <- c(
    as.character(myColors[1]),
    #  as.character(myColors[2]),
    rep("black", 6),
    as.character(myColors[8:11]),
    rep("black", 2)
  )
  myColorsContourOut <- c(
    as.character(myColors[1]),
    as.character(myColors[2]),
    rep("black", 4),
    as.character(myColors[7]),
    rep("black", 4),
    as.character(myColors[12]),
    "black"
  )
  
  myColorsContourAll <- c(as.character(myColors[1]),
                          # as.character(myColors[2]),
                          rep("black", 10),
                          as.character(myColors[12]),
                          "black")
  
  names(myColorsContourIn) <- new_levels
  names(myColorsContourOut) <- new_levels
  names(myColorsContourAll) <- new_levels
  
  colContourIn <-
    scale_colour_manual(name = "type_source", values = myColorsContourIn)
  colContourOut <-
    scale_colour_manual(name = "type_source", values = myColorsContourOut)
  colContourAll <-
    scale_colour_manual(name = "type_source", values = myColorsContourAll)
  
  #nodes_type_in <- nodes_type %>% filter(type_source %in% sources_keep_in_degree)
  nodes_type <-
    nodes_type %>% mutate(type_source = factor(type_source,
                                               levels = new_levels)) %>% filter(!is.na(type_source))
  #  oie_y_in <- nodes_type_in %>% filter(source == "OIE") %>% pull(in_degree)
  # oie_y_out <- nodes_type_out %>% filter(source == "OIE") %>% pull(out_degree)
  
  # new_y_indegree <- 16.5
  # nodes_type_in$in_degree[nodes_type_in$source == "OIE"] <- new_y_indegree
  # new_y_outdegree <- 21
  # nodes_type_out$out_degree[nodes_type_out$source == "OIE"] <- new_y_outdegree
  
  # y1 <- new_y_indegree - 1
  #  y2 <- y1 +0.5
  #  y3 <- y1 - 0.2
  #  y4 <- y3 +0.5
  
  
  if (type == "in") {
    plot_in <-
      ggplot(
        nodes_type,
        aes(
          x = type_source,
          y = in_degree,
          fill = type_source,
          colour = type_source
        )
      ) +
      geom_boxplot() +
      annotate(
        "text",
        x = 13,
        y = 128,
        label = "A)",
        size = 6
      ) +
      
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(trans = 'log2') +
      ylab("in-degree") +
      guides(fill = guide_legend(title = "Type of sources:"))  + colScale  + colContourIn + guides(colour =
                                                                                                     "none")
    return(plot_in)
  } else if (type == "out") {
    plot_out <-
      ggplot(
        nodes_type,
        aes(
          x = type_source,
          y = out_degree,
          fill = type_source,
          colour = type_source
        )
      ) +
      geom_boxplot() +
      annotate(
        "text",
        x = 13,
        y = 28,
        label = "B)",
        size = 6
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(trans = 'log2') +
      ylab("out-degree") +
      guides(fill = guide_legend(title = "Type of sources:"))  + colScale  + colContourOut + guides(colour =
                                                                                                      "none")
    return(plot_out)
  }  else if (type == "all") {
    plot_all <-
      ggplot(
        nodes_type,
        aes(
          x = type_source,
          y = all_degree,
          fill = type_source,
          colour = type_source
        )
      ) +
      geom_boxplot() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_continuous(trans = 'log2') +
      ylab("all-degree") +
      guides(fill = guide_legend(title = "Type of source:")) + colScale + colContourAll + guides(colour =
                                                                                                   "none") + guides(colour = "none")
    return(plot_all)
  }
}
