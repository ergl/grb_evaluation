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

df <- read.csv("../../red_coord/results.csv")
to_keep <- c("cure", "uniform", "uniform_red")
df <- df[df$replication %in% to_keep, ]
df <- df[df$replicas != 1, ]
df_readonly <- df[df$exp == "reads", ]

title_text <- "Reads only, 1 object, 20ms RTT"

scales <- scale_colour_manual(  name=""
                              , breaks=to_keep

                              , labels=c("Cure",
                                         "Uniform (100% blue)",
                                         "Uniform (100% red)")

                              , values=c("red",
                                         "blue",
                                         "green"))

throughput_plot <- ggplot(df_readonly, aes(x=factor(replicas), y=throughput,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=50000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,1000000)) +
    scales +
    labs(title=title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

latency_plot <- ggplot(df_readonly, aes(x=factor(replicas), y=ronly_median,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 40, by=1),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,40)) +
    scales +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

get_legend <- function(arg_plot) {
    tmp <- ggplot_gtable(ggplot_build(arg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

combined_legend <- get_legend(throughput_plot)
combined <- grid.arrange(grid.arrange(throughput_plot + theme(legend.position = "none"),
                                      latency_plot + theme(legend.position = "none"),
                                      nrow=1),
                         combined_legend,
                         ncol=1,
                         heights=c(0.9,0.1))

ggsave(filename = "./red_read.pdf",
       plot = combined,
       device = "pdf",
       width = 12,
       height = 8,
       dpi = 300)
