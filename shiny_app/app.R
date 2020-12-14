# ----------------------------------- SETUP --------------------------------

# Library Imports

library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(rstanarm)
library(gt)
library(gtsummary)
library(shinythemes)

# import our rds files

data <- readRDS("rds_files/data.rds")

starting_pitcher_stats <- readRDS("rds_files/sp_stats.rds")

whip_speed_fit <- readRDS("rds_files/whip_speed_fit.rds")

era_speed_fit <- readRDS("rds_files/era_speed_fit.rds")

whip_sl_fit <- readRDS("rds_files/whip_sl_fit.rds")

era_sl_fit <- readRDS("rds_files/era_sl_fit.rds")

whip_cu_fit <- readRDS("rds_files/whip_cu_fit.rds")

era_cu_fit <- readRDS("rds_files/era_cu_fit.rds")


# ----------------------------- HELPER FUNCTIONS ---------------------------

# function to get pitchers top 4 pitches

get_pitcher_data <- function(pitcher_name) {
    
    data %>% 
        
        filter(name == pitcher_name)
    
}

# gets a pitchers top 4 pitches, given the data submitted

get_top4 <- function(pitcher_data) {
    
    pitcher_data %>% 
        
        group_by(pitch_type) %>%
        
        summarise(count = n(), .groups = "drop") %>%
        
        arrange(desc(count)) %>%
        
        slice(1:4) %>%
        
        select(pitch_type) %>%
        
        as_vector()
    
}

# returns a vector of colors for the pitches

get_pitch_color <- function(pitch) {
    
    case_when(
        
        pitch == "CH" ~ "blue4",
        
        pitch == "CU" ~ "darkorchid",
        
        pitch == "EP" ~ "Eephus",
        
        pitch == "FC" ~ "Cutter",
        
        pitch == "FF" ~ "red3",
        
        pitch == "FO" ~ "Pitchout",
        
        pitch == "FS" ~ "Splitter",
        
        pitch == "FT" ~ "Two-seam Fastball",
        
        pitch == "IN" ~ "Intentional ball",
        
        pitch == "KC" ~ "darkorchid",
        
        pitch == "KN" ~ "Knuckeball",
        
        pitch == "PO" ~ "Pitchout" ,
        
        pitch == "SC" ~ "Screwball",
        
        pitch == "SI" ~ "Sinker",
        
        pitch == "SL" ~ "forestgreen",
        
        pitch == "UN" ~ "Unknown"
        
    )
    
}

# manual scale color to equal pitch

scale_color_pitch <- function(){
    ggplot2:::manual_scale(
        'color',
        values = setNames(c("dodgerblue3",
                            "darkorchid2",
                            "darkseagreen3",
                            "darkorange2",
                            "firebrick2",
                            "gray1",
                            "darkslategray2",
                            "firebrick4",
                            "gray",
                            "darkorchid4",
                            "darkslateblue",
                            "ghostwhite" ,
                            "royalblue3",
                            "tomato3",
                            "darkgreen",
                            "whitesmoke"), 
                          c("Changeup",
                            "Curveball",
                            "Eephus",
                            "Cutter",
                            "Four-seam Fastball",
                            "Pitchout",
                            "Splitter",
                            "Two-seam Fastball",
                            "Intentional ball",
                            "Knuckle curve",
                            "Knuckeball",
                            "Pitchout" ,
                            "Screwball",
                            "Sinker",
                            "Slider",
                            "Unknown"))
    )
}

# manual scale color to equal pitch

scale_fill_pitch <- function(){
    ggplot2:::manual_scale(
        'fill',
        values = setNames(c("dodgerblue3",
                            "darkorchid2",
                            "darkseagreen3",
                            "darkorange2",
                            "firebrick2",
                            "gray1",
                            "darkslategray2",
                            "firebrick4",
                            "gray",
                            "darkorchid4",
                            "darkslateblue",
                            "ghostwhite" ,
                            "royalblue3",
                            "tomato3",
                            "darkgreen",
                            "whitesmoke"), 
                          c("Changeup",
                            "Curveball",
                            "Eephus",
                            "Cutter",
                            "Four-seam Fastball",
                            "Pitchout",
                            "Splitter",
                            "Two-seam Fastball",
                            "Intentional ball",
                            "Knuckle curve",
                            "Knuckeball",
                            "Pitchout" ,
                            "Screwball",
                            "Sinker",
                            "Slider",
                            "Unknown"))
    )
}


# returns a vector with the full names of pitch abbreviations passed in

get_pitch_name <- function(pitch) {
    
    case_when(
        
        pitch == "CH" ~ "Changeup",
        
        pitch == "CU" ~ "Curveball",
        
        pitch == "EP" ~ "Eephus",
        
        pitch == "FC" ~ "Cutter",
        
        pitch == "FF" ~ "Four-seam Fastball",
        
        pitch == "FO" ~ "Pitchout",
        
        pitch == "FS" ~ "Splitter",
        
        pitch == "FT" ~ "Two-seam Fastball",
        
        pitch == "IN" ~ "Intentional ball",
        
        pitch == "KC" ~ "Knuckle curve",
        
        pitch == "KN" ~ "Knuckeball",
        
        pitch == "PO" ~ "Pitchout" ,
        
        pitch == "SC" ~ "Screwball",
        
        pitch == "SI" ~ "Sinker",
        
        pitch == "SL" ~ "Slider",
        
        pitch == "UN" ~ "Unknown"
        
    )
    
}

# simplify codes to strke, ball, in play / hit and in play / out 

simplify_code <- function(code) {
    
    case_when(
        
        code == "B" ~ "Ball",
        
        code == "*B" ~ "Ball",
        
        code == "S" ~ "Strike",
        
        code == "C" ~ "Strike",
        
        code == "F" ~ "Foul",
        
        code == "T" ~ "Foul",
        
        code == "L" ~ "Foul",
        
        code == "I" ~ "Ball",
        
        code == "W" ~ "Strike",
        
        code == "M" ~ "Strike",
        
        code == "P" ~ "Ball",
        
        code == "Q" ~ "Strike",
        
        code == "R" ~ "Foul",
        
        code == "E" ~ "In Play: Hit",
        
        code == "D" ~ "In Play: Hit",
        
        code == "X" ~ "In Play: Out",
        
        code == "H" ~ "In Play: Hit"
        
    )
    
    
}

# given a pitchers data, generates a graph of that pitchers top 4 pitches

gen_break_speed_graph <- function(pitcher_data) {
    
    pitcher_data %>%
        
        filter(pitch_type %in% get_top4(pitcher_data)) %>%
        
        ggplot(aes(y = start_speed, x = break_length, color = get_pitch_name(pitch_type))) +
        
        geom_jitter(alpha = 0.5, height = 0, width = 1.25, size = 2) + 
        
        labs(x = "Breaklength (in.)", y = "Velocity (MPH)", 
             title = paste("Speed and Break of ",
                           as.character(pitcher_data$name[1]),
                           "'s Pitches", 
                           sep = ""),
             color = "Pitch") +
        
        theme_minimal() + 
        
        scale_color_pitch() 
    
}

# Graph that shows pitch selection relative to pressure

gen_selection_graph <- function(pitcher_data) {
    
    pitcher_data %>%
        
        filter(pitch_type %in% get_top4(pitcher_data)) %>% 
        
        mutate(count = as.factor(paste0(as.character(b_count), 
                                        as.character(s_count)))) %>%
        
        group_by(count, pitch_type) %>%
        
        mutate(count = fct_relevel(count, "30", "31", "20", "32", "21", "10", "00", 
                                   "11", "01", "22", "12", "02")) %>%
        
        summarise(sum = n(), .groups = "drop_last") %>%
        
        mutate(freq = sum / sum(sum)) %>%
        
        ggplot(aes(fill = fct_relevel(get_pitch_name(pitch_type)
                                      , rev(map_chr(get_top4(pitcher_data), get_pitch_name))),
                   y = freq, x = count)) + 
        
        geom_bar(position="stack", stat="identity") + 
        
        labs(x = "Pitch count from hitter's to pitcher's advatage",
             y = "Frequency (%)", 
             title = paste("Pitch Selection of ",
                           as.character(pitcher_data$name[1]),
                           "'s Pitches", 
                           sep = ""),
             fill = "Pitch") +
        
        theme_minimal() + 
        
        scale_fill_pitch() 
    
}

# Graph that shows outcomes relative to pressure

gen_outcome_graph <- function(pitcher_data) {
    
    pitcher_data %>% 
        
        mutate(code = simplify_code(code)) %>%
        
        mutate(count = as.factor(paste0(as.character(b_count), as.character(s_count)))) %>%
        
        group_by(count, code) %>%
        
        mutate(count = fct_relevel(count, "30", "31", "20", "32", "21", "10", "00", 
                                   "11", "01", "22", "12", "02")) %>%
        
        summarise(sum = n(), .groups = "drop_last") %>%
        
        mutate(freq = sum / sum(sum)) %>%
        
        ggplot(aes(fill = code, y = freq, x = count)) + 
        
        geom_bar(position="stack", stat="identity") + 
        
        scale_fill_manual(name = "Outcome",
                          labels = c("Ball", "Foul", "In Play - Hit", 
                                     "In Play - Out", "Strike", "NA"),
                          values = c("dodgerblue3", "darkgreen", "darkorchid2", 
                                     "darkorange2", "firebrick4", "black")) +
        
        labs(x = "Pitch count from hitter's to pitcher's advatage",
             y = "Frequency (%)", 
             title = paste("Pitch Outcomes of ",
                           as.character(pitcher_data$name[1]),
                           "'s Pitches", 
                           sep = ""),
             fill = "Outcome") + 
        
        theme_minimal()
    
}

# Graph that shows how they compare to other pitchers

gen_comp_graph <- function(pitcher_name) {
    
    starting_pitcher_stats %>%
        
        mutate(name = ifelse(name == pitcher_name, TRUE, FALSE)) %>%
        
        mutate(label = ifelse(name == TRUE, pitcher_name, "")) %>%
        
        mutate(WHIP = 1 - (WHIP - min(WHIP))/ (max(WHIP) - min(WHIP))) %>%
        
        mutate(ERA = 1 - (ERA - min(ERA))/ (max(ERA) - min(ERA))) %>%
        
        mutate(W = (W - min(W))/ (max(W) - min(W))) %>%
        
        mutate(average_ff = (average_ff - min(average_ff))/ 
                   (max(average_ff) - min(average_ff))) %>%
        
        rename("Avg. FB Vel." = "average_ff") %>%
        
        pivot_longer(cols = c("WHIP", "ERA", "W", "Avg. FB Vel."), 
                     names_to = "stat",
                     values_to = "normalized_value") %>%
        
        ggplot(aes(x = normalized_value,
                   y = stat,
                   label = label,
                   color = ifelse(name == TRUE, "gold2", "black"),
                   shape = ifelse(name == TRUE, "star", "circle"),
                   size = ifelse(name == TRUE, 3, 2))) +
        
        geom_point(alpha = 0.6) +
        
        geom_text(nudge_y = 0.3, color = "gold2", size = 4) +
        
        scale_color_manual(values = c("darkgreen", "gold1")) +
        
        scale_shape_manual(values = c(16, 18)) +
        
        labs(title = paste(as.character(pitcher_name),
                           " Compared to Other Starting Pitchers", 
                           sep = ""),
             y = "Statistic",
             x = "") + 
        
        scale_x_continuous(breaks = c(0, 1),
                           labels = c("Worst", "Best")) +
        
        theme_minimal() + 
        
        theme(legend.position = "none")
    
}

# ------------------------------ SHINY APP UI ------------------------------



ui <- fluidPage(theme = shinytheme("paper"),
    
    navbarPage("MLB - 2019",
    
               
               tabPanel("About",
                        
                        tags$div(style = "width:80%; margin:auto",
                                 htmlOutput("about_title"),
                                 # imageOutput("yankee_stadium"),
                                 htmlOutput("about_page")
                        )
                        
               ),
               
               tabPanel("Data",
                        
                        sidebarPanel(
                            
                            # select a players whose stats you want to see
                            
                            selectInput("player_select",
                                        label = "Select a Starting Pitcher",
                                        choices = as.vector(starting_pitcher_stats$name),
                                        selected = "Gerrit Cole"),
                            
                            radioButtons("graph_select",
                                         label = "Select a Graph Type",
                                         choices = c("Stats Compared to Other Starters",
                                                     "Speed and Break of Top 4 Pitches",
                                                     "Pitch Selection under Pressure",
                                                     "Pitch Outcomes under Pressure"),
                                         selected = "Stats Compared to Other Starters")
                            
                        ),
                        
                        mainPanel(
                            
                            plotOutput("data_graph"),
                            
                            htmlOutput("graph_explination")
                        )
                        
               ),
               
               tabPanel("Model",
                        
                        sidebarPanel(
                            
                            # select a model
                            
                            selectInput("stat_select",
                                        label = "Select an Outcome Variable",
                                        choices = c("WHIP", "ERA"),
                                        selected = "ERA"),
                            
                            radioButtons("var_select",
                                         label = "Select a variable to measure by",
                                         choices = c("Fastball Velocity", 
                                                     "Slider Break",
                                                     "Curveball Break"),
                                         selected = "Fastball Velocity")
                            
                        ),  
                        
                        mainPanel(
                            
                            gt_output("model_output"),
                            
                            htmlOutput("model_explination")
                        )
                        
               )
               
    )
)


# ------------------------------------ HTML ELEMENTS -----------------------

about_title_html <- HTML(
    paste(
        "<h3 style=\"text-align:center\">Starting Pitchers: A pitch by 
        pitch analysis of the 2019 MLB season</h1>",
        sep = ""
    )
)

about_page_html <- HTML(
    paste("<div style=\"overflow: hidden;\">",
        "<h4>Background</h2>",
        "<p>Baseball in the past decade has gone through a data revolution. 
        People who have \"an eye for the game\" have slowly been pushed aside 
        by data analysts who use predictive models and advanced statistic to 
        know what players to put on a roster, what pitches to throw at a given 
        batter, and even where your fielders should stand on the field to be 
        more likely to catch the ball. This web app is designed to give you 
        a way to look at some of the data these analysts use, as well as 
        demo</p>", 
        "<h4>The Data</h2>",
        "<p>We use data from two sources; 
        <a href=\"https://www.kaggle.com/pschale/mlb-pitch-data-20152018\">
        data</a> scraped by a Kaggle user 
        from an official MLB source on ball tracking data for every pitch 
        thrown in the 2019 season, and the 
        <a href=\"https://www.rotowire.com/baseball/stats.php\">
        official MLB statistics</a> for the 2019 season. The pitch Data, which 
        you can explore on the data tab, gives us 
        the velocity, break, count, pitch type (fastball / slider), and pitch 
        outcome (strike, ball, hit) of each pitch thrown, along with 
        information about the pitcher, batter, and at-bat it was thrown in. Our
        statistical Model looks at the relationship between granular pitch 
        metrics like pitch speed and break and performance related statistics
        such as Earned Run Average (ERA) and Walks and Hits per Inning Pitched 
        (WHIP). 
        </p>", 
        "<h4>About Me</h2>",
        "<p>My name is Will Rowley, and I am a student at Harvard studying 
        Computer Science with a secondary in Government. This project is my 
        final project for Gov 50, an intro to data science course. I used R and
        Shiny to create this basic web app that allows you to explore some 
        pitch data from the 2019 Baseball season. Check out my 
        <a href=\"https://github.com/wrowley42\">Github</a> and feel 
        free to reach me at wrowley@college.harvard.edu </p>",
        "</div>",
        sep = ""
    )
)

# data graph captions

comp_explain_html <- HTML(
    paste(
        "<h3 style=\"text-align:center\">Comparison Graph</h3>",
        "<p>This graph represents the selected players distribution
        amongst other starting pitchers in 4 given statistics. The further 
        to the right the player is, the better they stack up against 
        others in that particular stat. The worst performing pitcher is placed 
        zero on the x-axis, while the best pitcher is placed at 1, with 
        all other pitchers having their values normalized between the best 
        and the worst.</p>", 
        sep = ""
    )
)

speed_explain_html <- HTML(
    paste(
        "<h3 style=\"text-align:center\">Speed vs. Break</h3>",
        "<p>This graph shows the speed and break length of a pitchers 
        top 4 most thrown pitches. Typically, breaking balls such as a 
        curveball or a slider are supposed to break more while having
        less velocity, making it harder for the hitter to know where to 
        swing. Pitches like the two and four-seam-fastball on the other hand 
        have less break but go fast enough to make it hard for a hitter to 
        time their swing. Some pitchers have a clear division in velocity 
        and break length of their different pitches, while others have a lot of 
        overlap. Trends you can look for in good modern day starting pitchers 
        are a fastball that stays consistently above 90 mph, a mix of 
        breaking pitches that break around 8 inches, and an off-speed 
        pitch like a curveball or change up that is slower than all the rest 
        and breaks more than 10 inches. </p>", 
        sep = ""
    )
)

selection_explain_html <- HTML(
    paste(
        "<h3 style=\"text-align:center\">Pitch Selection Under Pressure</h3>",
        "<p>This graph represents what pitch a pitcher goes to depending
        on what count the at bat is in. The more balls there are in an a bat,
        the more of advantage the hitter has, since they can be more selective 
        on what pitch to swing at and the pitcher has less lee-way for mistakes. 
        The more strikes there are, the advantage goes to the pitcher, since 
        they have the ability to waste pitches, trying to get the batter to 
        swing at bad pitches that are outside of the strikezone. Conventional 
        baseball wisdom suggests that counts like 3-0 and 3-1 are \"Fastball\"
        counts, since the pitcher will usually lean on their fastball to get 
        a strike as it's easier to control. On counts like 0-2 and 1-2, you'd 
        expect to see more breaking pitches, as the pitcher is using their lead
        in the atbat to put the ball off the plate so that the batter swings and
        misses.</p>", 
        "<p>*note: some pitchers have a low sample size on rarer counts, 
        particuarlly counts like 3-2 which might not occur in an entire game 
        </p>",
        sep = ""
    )
)

outcome_explain_html <- HTML(
    paste(
        "<h3 style=\"text-align:center\">Pitch Outcome Under Pressure</h3>",
        "<p>This graph represents what pitch a pitcher goes to depending
        on what count the at bat is in. The more balls there are in an a bat,
        the more of advantage the hitter has, since they can be more selective 
        on what pitch to swing at and the pitcher has less lee-way for mistakes. 
        The more strikes there are, the advantage goes to the pitcher, since 
        they have the ability to waste pitches, trying to get the batter to 
        swing at bad pitches that are outside of the strikezone. As the counts 
        go from left to right, you'd expect to see more balls, less strikes,
        less balls put in play, and more foul balls. This would reflect the 
        pressure that the pitcher or hitter would feel given the count, with 
        a pitcher's count meaning the hitter will be swinging and missing more 
        while a pitcher throws more balls, while a hitter's count would 
        result in more balls being put into play, with the pitcher feeling
        the pressure to throw more strikes. </p>", 
        "<p>*note: some pitchers have a low sample size on rarer counts, 
        particuarlly counts like 3-2 which might not occur in an entire game 
        </p>",
        sep = ""
    )
)

# model captions

whip_speed_html <- HTML(
    paste(
        "<h4 style=\"text-align:center\">WHIP ~ FB Velocity</h4>",
        "<p>This model takes a look at WHIP (Walks and Hits per Inning
        Pitched) as a function of fastball velocity. This model shows 
        that there is a negative correlation between fast ball velocity and 
        WHIP, meaning the faster you throw, the lower (better) your WHIP will 
        be.</p>", 
        sep = ""
    )
)

whip_slider_html <- HTML(
    paste(
        "<h4 style=\"text-align:center\">WHIP ~ Slider Break</h4>",
        "<p>This model takes a look at WHIP (Walks and Hits per Inning
        Pitched) as a function of slider break (for pitcher's who throw at 
        least 10% sliders). This model shows a small positive correlation
        between WHIP and slider break, suggesting having a further 
        breaking slider doesn't help your WHIP, and infact may make it 
        worse. </p>", 
        sep = ""
    )
)

whip_curve_html <- HTML(
    paste(
        "<h4 style=\"text-align:center\">WHIP ~ Curveball Break</h4>",
        "<p>This model takes a look at WHIP (Walks and Hits per Inning
        Pitched) as a function of curveball break (for pitcher's who throw at 
        least 10% curveballs). This model shows a fairly clear negative 
        correlation between the two, meaning that pitchers who have curveballs
        that \"curve\" more see better (lower) WHIPs.</p>", 
        sep = ""
    )
)


era_speed_html <- HTML(
    paste(
        "<h4 style=\"text-align:center\">ERA ~ FB Velocity</h4>",
        "<p>This model takes a look at ERA (Earned Run Average)
        as a function of fastball velocity. This model shows 
        that there is a negative correlation between fast ball velocity and 
        ERA, meaning the faster you throw, the lower (better) your ERA will 
        be.</p>", 
        sep = ""
    )
)

era_slider_html <- HTML(
    paste(
        "<h4 style=\"text-align:center\">ERA ~ Slider Break</h4>",
        "<p>This model takes a look at ERA (Earned Run Average)
        as a function of slider break (for pitcher's who throw at 
        least 10% sliders). This model shows a small positive correlation
        between ERA and slider break, suggesting having a further 
        breaking slider doesn't help your ERA, and infact may make it 
        worse. </p>", 
        sep = ""
    )
)

era_curve_html <- HTML(
    paste(
        "<h4 style=\"text-align:center\">ERA ~ Curveball Break</h4>",
        "<p>This model takes a look at ERA (Earned Run Average)
        as a function of curveball break (for pitcher's who throw at 
        least 10% curveballs). This model shows a fairly clear negative 
        correlation between the two, meaning that pitchers who have curveballs
        that \"curve\" more see better (lower) ERAs.</p>", 
        sep = ""
    )
)

# ----------------------------- SHINY APP SERVER LOGIC ---------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$about_page <- renderUI(about_page_html)
    
    output$about_title <- renderUI(about_title_html)
    
    output$yankee_stadium <- renderImage({
        list(src = "./yankee.jpg",
             style = "float: right;",
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$data_graph <- renderPlot ({
        
        p_name <- input$player_select
        
        pitcher_data <- get_pitcher_data(p_name)
        
        graph_choice <- input$graph_select
        
        if (graph_choice == "Stats Compared to Other Starters") {
            gen_comp_graph(p_name) 
        } else if(graph_choice == "Speed and Break of Top 4 Pitches") {
            gen_break_speed_graph(pitcher_data) 
        } else if(graph_choice == "Pitch Selection under Pressure") {
            gen_selection_graph(pitcher_data)
        } else {
            gen_outcome_graph(pitcher_data) 
        }
        
    })
    
    output$graph_explination <- renderUI({
        
        graph_choice <- input$graph_select
        
        if (graph_choice == "Stats Compared to Other Starters") {
            
            comp_explain_html
            
        } else if(graph_choice == "Speed and Break of Top 4 Pitches") {
            
            speed_explain_html 
            
        } else if(graph_choice == "Pitch Selection under Pressure") {
            
            selection_explain_html
            
        } else {
            
            outcome_explain_html
            
        }
        
    })
    
    output$model_output <- render_gt({
        
        stat_choice <- input$stat_select
        
        var_choice <- input$var_select
        
        if (stat_choice == "WHIP" &
            var_choice == "Fastball Velocity") {
            
            whip_speed_fit
            
        } else if(stat_choice == "WHIP" &
                  var_choice == "Slider Break") {
            
            whip_sl_fit 
            
        } else if(stat_choice == "WHIP" &
                  var_choice == "Curveball Break") {
            
            whip_cu_fit
            
        } else if(stat_choice == "ERA" &
                  var_choice == "Fastball Velocity") {
            
            era_speed_fit
            
        } else if(stat_choice == "ERA" &
                  var_choice == "Slider Break") {
            
            era_sl_fit
            
        } else {
            
            era_cu_fit
            
        }
        
    })
    
    output$model_explination <- renderUI({
        
        stat_choice <- input$stat_select
        
        var_choice <- input$var_select
        
        if (stat_choice == "WHIP" &
            var_choice == "Fastball Velocity") {
            
            whip_speed_html
            
        } else if(stat_choice == "WHIP" &
                  var_choice == "Slider Break") {
            
            whip_slider_html
            
        } else if(stat_choice == "WHIP" &
                  var_choice == "Curveball Break") {
            
            whip_curve_html
            
        } else if(stat_choice == "ERA" &
                  var_choice == "Fastball Velocity") {
            
            era_speed_html
            
        } else if(stat_choice == "ERA" &
                  var_choice == "Slider Break") {
            
            era_slider_html
            
        } else {
            
            era_curve_html
            
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
