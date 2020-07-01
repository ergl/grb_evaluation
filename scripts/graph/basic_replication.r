#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                        #  "grid",
                         "getopt",
                         "proto",
                         "gridExtra",
                         "ggplot2",
                         "scales")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        library(p, character.only=TRUE)
    }
}

df <- read.csv("../../basic_replication_test/general.csv")

y_format_thousand_comma <- function(x) {
    return(format(x, big.mark = ",", scientific = FALSE))
}

no_br_color <- "#F2818F"
br_color <- "#1C5BD0"

legend_title <- "Local communication" # No title
legend_breaks <- c(0, 1)
legend_labels <- c("All-to-all", "Broadcast Tree (fanout 2)")
legend_values <- c(no_br_color, br_color)

throughput <- ggplot(df, aes(x=factor(clusters), y=throughput, fill=factor(broadcast_tree))) +
    geom_bar(position="dodge", stat="identity", colour="black", size=0.25, width=0.5) +

    scale_y_continuous(breaks=seq(0, 1000000, by=50000),
                       labels=y_format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,1000000)) +

    scale_fill_manual(name=legend_title,
                      breaks=legend_breaks,
                      labels=legend_labels,
                      values=legend_values) +

    labs(title = "Readonly blue transactions with 3 nodes per cluster",
         x = "Number of clusters",
         y = "Maximum Throughput (tps)") +

    theme_minimal(base_size=10) +

    theme(plot.title = element_text(size=10, margin=margin(10,0,10,0), hjust=0.5),
          plot.margin = margin(0,10,0,0),

          axis.title.x = element_text(size=9, margin=margin(10,0,0,0)),
          axis.title.y = element_text(size=9, margin=margin(0,10,0,10)),
          axis.text.x =  element_text(size=7, margin=margin(10,0,0,0)),
          axis.text.y =  element_text(size=7, margin=margin(0,8,0,10)),
          axis.line =    element_line(color="black", size=0.5),
          axis.ticks =   element_line(color="black"),

          panel.grid.minor = element_line(colour="#EBEBEB", size=0.25),
          panel.grid.major = element_line(colour="#EBEBEB", size=0.25),
          panel.spacing =    unit(1, "lines"),

          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.box.background = element_rect(color="white", fill="white"))

ggsave(filename = "./general_bench.png",
       plot = throughput,
       device = "png",
       width = 5,
       height = 4.5,
       dpi = 150)
