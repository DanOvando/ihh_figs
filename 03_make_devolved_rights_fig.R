


source("00_setup.R")

devolved_rights <- read_excel(here("data","4_figure_governance", "Devolved_rights.xlsx"))

devolved_rights$no_rights_catch <-
  devolved_rights$total_catch * devolved_rights$no_rights_per
devolved_rights$omgmt_catch <-
  devolved_rights$total_catch * devolved_rights$omgmt_per
devolved_rights$mgmt_excl_OR_mgmt_ali_catch <-
  devolved_rights$total_catch * devolved_rights$mgmt_excl_OR_mgmt_ali_per
devolved_rights$mgmt_excl_ali_catch <-
  devolved_rights$total_catch * devolved_rights$mgmt_excl_ali_per

plot <- data.frame(matrix(nrow = 4, ncol = 4))

colnames(plot) <-
  c("type_right", "catch", "total_catch", "percentage")

plot$type_right = c("no_rights",
                    "omgmt",
                    "mgmt_excl_OR_mgmt_ali",
                    "mgmt_excl_ali")

plot$catch <-
  colSums(devolved_rights[, c(
    "no_rights_catch",
    "omgmt_catch",
    "mgmt_excl_OR_mgmt_ali_catch",
    "mgmt_excl_ali_catch"
  )])

plot$total_catch <- colSums(devolved_rights[, c("total_catch")])

plot$percentage <- round(plot$catch / plot$total_catch, 1)

plot$type_right <-
  factor(
    plot$type_right,
    levels = c(
      "no_rights",
      "omgmt",
      "mgmt_excl_OR_mgmt_ali",
      "mgmt_excl_ali"
    )
  )


devoloved_rights_plot <-
  ggplot (plot, aes(y = percentage, x = type_right)) +
  
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "#154360") +
  
  scale_y_continuous (
    expand = c(0, 0),
    limits = c(0, 100),
    labels = function(x)
      paste0(x, "%")
  ) +
  
  labs(x = "Tenure rights", y = "Percentage of total SSF catch", fill = "") +
  
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.6) +
  
  theme_classic() + theme(
    text = element_text(size = 13),
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 12,
      b = 0,
      l = 0
    )),
    # distance between text and label y axis
    axis.title.x = element_text(margin = margin(
      t = 12,
      r = 0,
      b = 0,
      l = 0
    )),
    # distance between text and label y axis
    axis.title = element_text(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  file.path(fig_dir, glue::glue("devolved_rights_plot.pdf")),
  devoloved_rights_plot,
  width = 180,
  height = 170,
  units = "mm"
)
