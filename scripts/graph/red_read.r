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

          axis.title.x = element_text(size=9, margin=margin(10,0,-15,0)),
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

df <- read.csv("../../red_coord_2/results.csv")
df <- df[df$replicas != 1, ]
to_keep <- c("cure", "old_uniform", "uniform_blue", "uniform_red")
df <- df[df$replication %in% to_keep, ]
df_readonly <- df[df$exp == "reads", ]
df_updates <- df[df$exp == "updates", ]

reads_title_text <- "Reads only, 1 object, 20ms RTT"
updates_title_text <- "Updates only, 1 object, 20ms RTT"

scales <- scale_colour_manual(  name=""
                              , labels=c(`cure` = "Cure",
                                         `old_uniform` = "Uniform",
                                         `uniform_blue` = "RedBlue (100% blue)",
                                         `uniform_red` = "RedBlue (100% red)",
                                         `z_red_commit` = "RedBlue (100% red), commit latency")

                              , values=c(`cure` = "red",
                                         `old_uniform` = "blue",
                                         `uniform_blue` = "green",
                                         `uniform_red` = "orange",
                                         `z_red_commit` = "brown"))

throughput_plot <- ggplot(df_readonly, aes(x=factor(replicas), y=throughput,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=50000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,800000)) +
    scales +
    labs(title=reads_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

throughput_writes_plot <- ggplot(df_updates, aes(x=factor(replicas), y=throughput,
                                        group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 500000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,200000)) +
    scales +
    labs(title=updates_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

latency_plot <- ggplot(df_readonly, aes(x=factor(replicas), group=replication)) +

    geom_point(size=1.5, aes(y=ronly_median, color=replication)) +
    geom_line(aes(y=ronly_median, color=replication)) +

    geom_point(size=1.5, aes(y=red_commit_median, color="z_red_commit")) +
    geom_line(aes(y=red_commit_median, color="z_red_commit")) +

    geom_point(size=1.5, aes(y=red_ronly_median, color=replication)) +
    geom_line(aes(y=red_ronly_median, color=replication)) +

    scale_y_continuous(breaks=seq(0, 50, by=2),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(1,40)) +
    scales +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

latency_writes_plot <- ggplot(df_updates, aes(x=factor(replicas), group=replication, color=replication)) +

    geom_point(size=1.5, aes(y=wonly_median, color=replication)) +
    geom_line(aes(y=wonly_median, color=replication)) +
    geom_point(size=1.5, aes(y=red_wonly_median, color=replication)) +
    geom_line(aes(y=red_wonly_median, color=replication)) +
    scale_y_continuous(breaks=seq(0, 50, by=2),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(1,45)) +
    scales +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

df_red_updates <- df_updates[df_updates$replication == "uniform_red", ]
abort_plot <- ggplot(df_red_updates, aes(x=factor(replicas), y=abort_ratio)) +
    geom_bar(stat='identity') +
    scale_y_continuous(breaks=seq(0,1,by=0.05), expand=c(0,0)) +
    coord_cartesian(ylim=c(0,0.25)) +
    scales +
    labs(title="Red Updates only / Abort ratio", x = "Replicas", y = "Abort ratio") +
    plot_theme

total_red_update_thr_plot <- ggplot(df_red_updates, aes(x=factor(replicas), group=replication))+
    geom_point(size=1.5, aes(y=throughput, color=replication)) +
    geom_line(aes(y=throughput, color=replication)) +
    geom_point(size=1.5, aes(y=total_throughput, color=replication)) +
    geom_line(aes(y=total_throughput, color=replication)) +
    scale_y_continuous(breaks=seq(0, 500000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,65000)) +
    scales +
    labs(title=updates_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

get_legend <- function(arg_plot) {
    tmp <- ggplot_gtable(ggplot_build(arg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

combined_legend <- get_legend(latency_plot)

reads <- grid.arrange(throughput_plot + theme(legend.position = "none"),
                      latency_plot + theme(legend.position = "none"),
                      nrow=1)

updates <- grid.arrange(throughput_writes_plot + theme(legend.position = "none"),
                        latency_writes_plot + theme(legend.position = "none"),
                        nrow=1)

reads_combined <- grid.arrange(reads, combined_legend,
                               ncol=1,
                               heights=c(0.9,0.1))

ggsave(filename = "./red_read.pdf",
       plot = reads_combined,
       device = "pdf",
       width = 10,
       height = 5,
       dpi = 300)

ggsave(filename = "./red_abort.pdf",
       plot = abort_plot,
       device = "pdf",
       width = 5,
       height = 5,
       dpi = 300)
