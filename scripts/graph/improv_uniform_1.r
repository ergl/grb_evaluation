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

df <- read.csv("../../replication_comparison-1/general.csv")
df <- df[df$replication != "uniform_nouvc", ]
df <- df[df$replication != "improved_nouvc_pretx", ]
df_readonly <- df[df$exp == "reads", ]
df_updates <- df[df$exp == "updates", ]

scales <- scale_colour_manual(  name=""
                              , breaks=c("cure",
                                         "uniform",
                                         "improved",
                                         "improved_nouvc",
                                         "improved_nouvc_remtx")

                              , labels=c("Cure",
                                         "Uniform",
                                         "Modified Uniform",
                                         "Modified Uniform (no uniformVC)",
                                         "Modified Uniform (no uniformVC + remove Txs during repl.)")

                              , values=c("red",
                                         "blue",
                                         "green",
                                         "orange",
                                         "black"))

reads_plot <- ggplot(df_readonly, aes(x=factor(replicas), y=throughput,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=50000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,1000000)) +
    scales +
    labs(title="Reads only, 1 object", x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

updates_plot <- ggplot(df_updates, aes(x=factor(replicas), y=throughput,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,300000)) +
    scales +
    labs(title="Updates only, 1 object", x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

get_legend <- function(arg_plot) {
    tmp <- ggplot_gtable(ggplot_build(arg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

combined_legend <- get_legend(reads_plot)
combined <- grid.arrange(grid.arrange(reads_plot + theme(legend.position = "none"),
                                     updates_plot + theme(legend.position = "none"),
                                     nrow=1),
                         combined_legend,
                         ncol=1,
                         heights=c(0.9,0.1))

ggsave(filename = "./replication_comparison_1.pdf",
       plot = combined,
       device = "pdf",
       width = 15,
       height = 8,
       dpi = 300)