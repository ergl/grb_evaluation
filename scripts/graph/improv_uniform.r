#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                         "grid",
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

format_thousand_comma <- function(x) {
    return(format(x/1000, big.mark = ",", scientific = FALSE))
}

plot_theme <- theme_minimal(base_size=10) +
    theme(plot.title = element_text(size=9, margin=margin(0,0,10,0)),
          plot.margin = margin(15,20,15,0),

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
          legend.title = element_text(size=7),
          legend.text = element_text(size=6),
          legend.box.background = element_rect(color="black", fill="white"))

df <- read.csv("../../replication_comparison/general.csv")
df_replicas <- df[df$exp == "replicas", ]
df_replicas <- df_replicas[df_replicas$replication != "pvc", ]

replica_plot <- ggplot(df_replicas, aes(x=factor(replicas), y=throughput,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("cure", "uniform", "delay_send_clocks", "remote_uvc"),
        labels=c("Cure", "Uniform Replication", "Delay Clocks", "Delay uniformVC computation"),
        values=c("red", "blue", "green", "orange")
    ) +
    labs(title="Replication Comparison", x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

ggsave(filename = "./replication_comparison.pdf",
       plot = replica_plot,
       device = "pdf",
       width = 5,
       height = 5,
       dpi = 300)
