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

# ugly hack, learn how to use geom_abline
add_linear_regresion <- function(frame, offset, transport_name, new_transport_name) {
    base <- subset(frame, transport == transport_name & replicas == 1)$throughput
    a <- frame[offset, ]
    b <- frame[offset + 1, ]
    c <- frame[offset + 2, ]
    a$transport <- new_transport_name
    a$throughput <- base
    b$transport <- new_transport_name
    b$throughput <- base * 2
    c$transport <- new_transport_name
    c$throughput <- base * 3
    frame[nrow(frame) + 1, ] <- a
    frame[nrow(frame) + 1, ] <- b
    frame[nrow(frame) + 1, ] <- c
    return(frame)
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

df <- read.csv("../../validate_improved_replication/general.csv")

df_replicas <- df[df$exp == "replicas", ]
# add new experiment name factors, so we can add rows with new exp. names
df_replicas$transport <- factor(
    df_replicas$transport,
    levels=c(levels(df_replicas$transport),
             "shackle_linear",
             "socket_linear",
             "pvc_linear",
             "shackle_delay_linear")
)

df_replicas <- add_linear_regresion(df_replicas, 1, "shackle", "shackle_linear")
df_replicas <- add_linear_regresion(df_replicas, 4, "socket", "socket_linear")
df_replicas <- add_linear_regresion(df_replicas, 7, "pvc", "pvc_linear")
df_replicas <- add_linear_regresion(df_replicas, 10, "shackle_delay", "shackle_delay_linear")

replica_plot <- ggplot(df_replicas[df_replicas$transport %in% c("shackle", "socket", "pvc", "shackle_delay"), ],
                      aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("shackle", "socket", "pvc", "shackle_delay"),
        labels=c("New Uniform Repl.", "Old Uniform Repl.", "Basic Repl.", "Delay clocks"),
        values=c("red", "blue", "orange", "black")
    ) +
    labs(title="Replication Comparison", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot1 <- ggplot(df_replicas[df_replicas$transport %in% c("shackle", "shackle_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("shackle", "shackle_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="New Uniform Replication", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot2 <- ggplot(df_replicas[df_replicas$transport %in% c("socket", "socket_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("socket", "socket_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="Old Uniform Replication", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot3 <- ggplot(df_replicas[df_replicas$transport %in% c("pvc", "pvc_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("pvc", "pvc_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="Basic Replication", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot4 <- ggplot(df_replicas[df_replicas$transport %in% c("shackle_delay", "shackle_delay_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("shackle_delay", "shackle_delay_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="New Uniform Replication (10s clocks)", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme


combined <- grid.arrange(grid.arrange(replica_plot4,
                                      replica_plot1, nrow=1),
                        grid.arrange(replica_plot2,
                                      replica_plot3, nrow=1),
                        ncol=1)

ggsave(filename = "./uniform_bench_improved_general.pdf",
       plot = replica_plot,
       device = "pdf",
       width = 5,
       height = 5,
       dpi = 300)

ggsave(filename = "./uniform_bench_improved_comparison.pdf",
       plot = combined,
       device = "pdf",
       width = 10,
       height = 10,
       dpi = 300)
