#' @title create_deviation_plot
#' @return
#' @author Aaron Conway
#' @export

#' @importFrom dplyr mutate case_when inner_join select
#' filter desc n group_by pull lag
#' @importFrom ggplot2 ggplot aes facet_wrap labeller
#' geom_point geom_polygon alpha geom_vline geom_segment
#' scale_x_continuous scale_y_continuous scale_color_manual
#' labs geom_curve arrow unit geom_label theme element_blank
#' element_text element_line margin ggsave
#' @importFrom ggtext element_markdown
#' @importFrom here here
#' @importFrom tidyr gather
#'
create_deviation_plot <- function(data_ptds, coded_descriptions) {
    coded_descriptions <- coded_descriptions %>%
        mutate(fasting_type = case_when(
            npo_midnight == 1 ~ 1,
            npo_morning == 1 ~ 2,
            npo_hrsprior == 1 ~ 3,
            npo_dinner == 1 ~ 1,
            separate_fooddrink == 1 ~ 4,
            unclear_patient == 1 ~ 6,
            missing == 1 ~ 7
        )) %>%
        mutate(fasting_type_text = case_when(
            fasting_type %in% 1 ~ "\"No food or drink after midnight\"",
            fasting_type == 2 ~ "\"No food or drink after __ AM\"",
            fasting_type == 3 ~ "\"No food or fluids __ hours prior\"",
            fasting_type == 4 ~ "\"No food __ hours prior and no\ndrink after __ AM this morning\"",
            fasting_type == 6 ~ "Unclear instructions",
            fasting_type == 7 ~ "Missing"
        ))

    # Join coded data to original data
    coded_descriptions$id <- as.character(coded_descriptions$id)
    coded_data <- data_ptds %>%
        inner_join(coded_descriptions %>%
            select(id, fasting_type, fasting_type_text), by = c("id" = "id"))

    # Compute food and fluids fasting deviations
    coded_data <- coded_data %>% mutate(
        food_dur = as.double(date_time - food),
        fluids_dur = as.double(date_time - fluids)
    )

    # Filter out screening arm and select only relevant columns
    coded_data <- coded_data %>%
        #   filter(redcap_event_name == "t3_arm_1") %>%
        select(id, fasting_type, fasting_type_text, food_dur, fluids_dur)



    ### Prepare plotting data

    # category spacing
    grp_space <- 30

    coded_data <- coded_data %>%
        drop_na()

    # Omit "missing" and "unclear" categories
    plot_data <- coded_data %>%
        arrange(desc(fasting_type)) %>%
        mutate(y = 1:n()) %>%
        mutate(y = y + grp_space * (max(fasting_type) - fasting_type)) %>%
        group_by(fasting_type) %>%
        mutate(ymean = mean(y))

    # Convert to long format to allow use of facet_wrap()
    plot_data <- gather(plot_data, key = "food_fluid", value = "value", c("food_dur", "fluids_dur"))

    # Add in deviation from recommendation value
    plot_data <- plot_data %>%
        mutate(
            recommendation = case_when(
                food_fluid == "food_dur" ~ 6,
                food_fluid == "fluids_dur" ~ 2
            ),
            mypalette = case_when(
                food_fluid == "food_dur" ~ "orangered",
                food_fluid == "fluids_dur" ~ "dodgerblue"
            )
        ) %>%
        mutate(deviation = value - recommendation)


    # Facet labels
    plot_labels <- c(food_dur = "Food", fluids_dur = "Fluids")

    # Shaded regions
    shading <- data.frame(y = plot_data %>%
        group_by(fasting_type) %>%
        filter(y == max(y)) %>%
        pull(y) %>%
        unique())
    shading$y <- shading$y + grp_space / 2
    shading$yend <- lag(shading$y)
    shading <- shading[c(2, 4), ]

    shading1 <- data.frame(y = rep(t(shading[1, ]), c(2, 2)))
    shading1$x <- c(-5, 20, 20, -5)

    shading2 <- data.frame(y = rep(t(shading[2, ]), c(2, 2)))
    shading2$x <- c(-5, 20, 20, -5)

    # Textbox and arrow data: largest deviation coords
    highlight <- plot_data %>%
        filter(
            food_fluid == "fluids_dur", deviation == max(deviation),
            fasting_type_text == "\"No food or drink after midnight\""
        ) %>%
        select(food_fluid, deviation, y)
    highlight$food_fluid <- factor(highlight$food_fluid,
        levels = c("food_dur", "fluids_dur")
    )

    ### GGPLOT code

    deviation_plot <- ggplot(
        plot_data,
        aes(
            x = deviation,
            y = y
        )
    ) +
        facet_wrap(~food_fluid,
            nrow = 1,
            labeller = labeller(food_fluid = plot_labels)
        ) +
        geom_point(aes(color = food_fluid),
            show.legend = FALSE
        ) +
        geom_polygon(
            data = shading1,
            aes(x = x, y = y),
            fill = alpha(1, 0.05)
        ) +
        geom_polygon(
            data = shading2,
            aes(x = x, y = y),
            fill = alpha(1, 0.05)
        ) +
        geom_vline(aes(
            xintercept = 0,
            color = food_fluid,
            group = mypalette
        ),
        linetype = "longdash",
        size = 1
        ) +
        geom_vline(aes(
            xintercept = 0,
            color = food_fluid,
            group = mypalette
        ),
        linetype = "longdash", size = 1
        ) +
        geom_segment(aes(
            x = 0,
            xend = deviation,
            y = y,
            yend = y,
            color = food_fluid
        ),
        alpha = 0.2,
        show.legend = FALSE
        ) +
        scale_x_continuous(
            breaks = c(0, 5, 10, 15, 20),
            labels = paste(c(0, 5, 10, 15, 20), "hr"),
            limits = c(-5, 20),
            position = "top"
        ) +
        scale_y_continuous(
            breaks = unique(plot_data$ymean),
            labels = unique(plot_data$fasting_type_text),
            limits = c(0, max(plot_data$y))
        ) +
        scale_color_manual(
            name = "",
            values = c("dodgerblue", "orangered"),
            labels = c("2 hr. recomm. \nfor fluids", "6 hr. recomm. \nfor food")
        ) +
        labs(
            title = "**Deviation from recommended fasting durations for <span style='color:dodgerblue'>fluids (2 hours)</span> and <span style='color:orangered'>food (6 hours)</span> according to type<br> of instructions received**",
            y = element_blank()
        ) +
        #geom_curve(
        #    data = highlight,
        #    aes(
        #        xend = deviation + 0,
        #        yend = y - 0.5,
        #        x = deviation,
        #        y = y - 80
        #    ), # coordinates for arrow
        #    curvature = .15,
        #    arrow = arrow(length = unit(2, "mm"))
        #) +
        #geom_label(
        #    data = highlight,
        #    aes(
        #        x = deviation - 8,
        #        y = y - 80
        #    ),
        #    label = paste0(
        #        "This patient was fasted ",
        #        round(highlight$deviation),
        #        " hours\n longer than required"
        #    ), hjust = "center"
        #) +
        theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            panel.background = element_blank(),
            panel.grid.major.x = element_line(colour = alpha(1, 0.075)),
            panel.grid.minor.x = element_line(colour = alpha(1, 0.05)),
            panel.spacing = unit(1, "line"),
            legend.position = "none",
            strip.text.x = element_blank(),
            strip.background = element_blank(),
            plot.title = element_markdown(size = 14, margin = margin(0, 0, 20, 0)),
            plot.title.position = "plot",
                      text = element_text( size = 18)

        )

}
