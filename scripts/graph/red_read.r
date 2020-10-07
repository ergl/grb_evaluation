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

df <- read.csv("../../redblue_evaluation/results.csv")
df$replication <- factor(df$replication, levels=c('cure', 'uniform',
                                                  'redblue_blue', 'redblue_red'))
df$cluster <- factor(df$cluster, levels=c('all', 'virginia',
                                          'oregon', 'ireland', 'california'))
df_reads <- df[df$op == "read", ]
df_reads_overall <- df_reads[df_reads$cluster == "all", ]
df_reads_each <- df_reads[df_reads$cluster != "all", ]
df_updates <- df[df$op == "update", ]

reads_title_text <- "R=1, 20ms RTT"
updates_title_text <- "W=1, 20ms RTT"

replica_labels <- c(`california` = "Replica C",
                    `ireland` = "Replica B",
                    `oregon` = "Replica A",
                    `virginia` = "Leader")

replica_colors <- c(`california` = "#FE7B4B",
                    `ireland` = "#4DE481",
                    `oregon` = "#1C5BD0",
                    `virginia` = "#F2818F")

scales <- scale_colour_manual(  name=""
                              , labels=c(`cure` = "Cure",
                                         `uniform` = "Uniform",
                                         `redblue_blue` = "RedBlue (100% blue)",
                                         `redblue_red` = "RedBlue (100% red)")

                              , values=c(`cure` = "red",
                                         `uniform` = "blue",
                                         `redblue_blue` = "green",
                                         `redblue_red` = "orange"))

thr_r_plot <- ggplot(df_reads_overall, aes(x=factor(replicas), y=throughput,
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

thr_w_plot <- ggplot(df_updates, aes(x=factor(replicas), y=throughput,
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

lat_r_plot <- ggplot(df_reads_overall, aes(x=factor(replicas), group=replication)) +

    geom_point(size=1.5, aes(y=ronly_median, color=replication)) +
    geom_line(aes(y=ronly_median, color=replication)) +

    geom_point(size=1.5, aes(y=red_ronly_median, color=replication)) +
    geom_line(aes(y=red_ronly_median, color=replication)) +
    scale_y_continuous(breaks=seq(0, 50, by=1),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0.1,30)) +
    scales +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

lat_r_each_plot <- ggplot(df_reads_each, aes(x=factor(cluster),
                                             y=red_ronly_median, fill=cluster)) +
    geom_bar(stat='identity', position="dodge") +
    geom_hline(yintercept=20, size=0.5, color="#807F80") +
    geom_hline(yintercept=30, size=0.5, color="#807F80") +
    facet_rep_wrap(~replicas, nrow=1, scales="free_x", labeller=labeller(
        replicas = c(
            `2` = "Active: 2 Replicas",
            `3` = "Active: 3 Replicas",
            `4` = "Active: 4 Replicas"
        ))
    ) +
    scale_x_discrete(labels=replica_labels) +
    scale_fill_manual(labels=replica_labels,
                      values=replica_colors) +
    scale_y_continuous(breaks=seq(0,40,by=2),
                       expand=c(0,0),
                       sec.axis = dup_axis(name="")) +
    coord_cartesian(ylim=c(0,34)) +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme +
    theme(legend.position = "none",
          axis.title.x = element_text(size=9, margin=margin(10,0,0,0)))

lat_w_plot <- ggplot(df_updates, aes(x=factor(replicas), y=wonly_median,
                                     group=replication, color=replication)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 3, by=0.25),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,3)) +
    scales +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

get_legend <- function(arg_plot) {
    tmp <- ggplot_gtable(ggplot_build(arg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

combined_legend <- get_legend(thr_r_plot)

reads <- grid.arrange(thr_r_plot + theme(legend.position = "none"),
                      lat_r_plot + theme(legend.position = "none"),
                      nrow=1)

updates <- grid.arrange(thr_w_plot + theme(legend.position = "none"),
                        lat_w_plot + theme(legend.position = "none"),
                        nrow=1)

# combined <- grid.arrange(reads, updates, combined_legend, ncol=1,
#                          heights=c(0.50,0.48,0.1))

combined <- grid.arrange(reads, combined_legend, ncol=1,
                         heights=c(0.9,0.1))

ggsave(filename = "./red_read.pdf",
       plot = combined,
       device = "pdf",
       width = 10,
       height = 6,
       dpi = 300)

ggsave(filename = "./red_read_each.pdf",
       plot=lat_r_each_plot,
       width=8,
       height=5,
       dpi=300)

# ggsave(filename = "./red_abort.pdf",
#        plot = abort_plot,
#        device = "pdf",
#        width = 5,
#        height = 5,
#        dpi = 300)
