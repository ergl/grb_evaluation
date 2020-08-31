#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                         "grid",
                         "getopt",
                         "proto",
                         "gridExtra",
                         "reshape2",
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
    theme(plot.title = element_text(size=12, margin=margin(0,0,10,0)),
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

wonly_legend_params <- scale_colour_manual(name="",
                                           labels=c("No Barrier", "Post-commit barrier"),
                                           breaks=c("wonly_median", "wonly_barrier_median"),
                                           values=c("red", "blue"))

df <- read.csv("../../uniform_barrier/results.csv")
df <- df[df$exp == "write_barrier", ]

throughput_plot <- ggplot(df, aes(x=factor(replication), y=throughput,
                                  group=replication, color=exp)) +
    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=5000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,10000)) +
    labs(x = "Replication Interval", y = "Throughput (Ktps)") +
    plot_theme

latencies_plot <- ggplot(df, aes(x=factor(replication), y=wonly_barrier_median,
                                 group=replication, color=exp)) +
    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 150, by=10), expand=c(0,0)) +
    coord_cartesian(ylim=c(0,150)) +
    labs(x = "Replication Interval", y = "Median Latency (ms)") +
    plot_theme

combined <- grid.arrange(throughput_plot + theme(legend.position = "none"),
                         latencies_plot + theme(legend.position = "none"),
                         nrow=1)

ggsave(filename = "./barrier.pdf",
       plot = combined,
       device = "pdf",
       width = 15,
       height = 8,
       dpi = 300)
