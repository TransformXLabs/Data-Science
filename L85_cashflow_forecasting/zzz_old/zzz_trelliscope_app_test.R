

library(trelliscopejs)
library(dplyr)
library(ggplot2)
library(tidyr)

ui <- shinyUI(fluidPage(
    trelliscopeOutput(outputId = "plot")
))

server <- shinyServer(function(input, output) {
    output$plot <- renderTrelliscope({

        mpg %>%
            nest(data = !one_of(c("manufacturer", "class"))) %>%
            mutate(
                panel = map_plot(data, ~
                                     qplot(cty, hwy, data = .) + xlab("cty") + ylab("hwy") +
                                     xlim(7, 37) + ylim(9, 47) + theme_bw())) %>%
            trelliscope(name = "tidy_gg", self_contained = TRUE, path = "www", )
    })
})

shinyApp(ui = ui, server = server)
