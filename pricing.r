width = 1000
height = 750
line_thickness = 2

# Values taken from http://aws.amazon.com/ec2/pricing/
# For an m1.large ("Large") instance in Virginia or Oregon
on_demand_hourly = 0.24

# Turningon 3 year calculations makes chart very difficult to read
# Furthermore, Amazon drops prices often so a 3 year contract probably
# isn't worth it (https://twitter.com/jordansissel/status/297405690174111745)
three_year = FALSE

# Light utilization
light = TRUE
reserve_1year_light = 276
reserve_1year_light_hourly = 0.156
reserve_3year_light = 425
reserve_3year_light_hourly = 0.124
# Medium utilization
medium = TRUE
reserve_1year_medium = 640
reserve_1year_medium_hourly = 0.096
reserve_3year_medium = 1000
reserve_3year_medium_hourly = 0.076
# Heavy utilization
heavy = TRUE
reserve_1year_heavy = 780
reserve_1year_heavy_hourly = 0.064
reserve_3year_heavy = 2400
reserve_3year_heavy_hourly = 0.052

on_demand_daily = on_demand_hourly * 24
reserve_1year_light_daily = reserve_1year_light_hourly * 24
reserve_3year_light_daily = reserve_3year_light_hourly * 24
reserve_1year_medium_daily = reserve_1year_medium_hourly * 24
reserve_3year_medium_daily = reserve_3year_medium_hourly * 24
reserve_1year_heavy_daily = reserve_1year_heavy_hourly * 24
reserve_3year_heavy_daily = reserve_3year_heavy_hourly * 24

x <- c(0, 365)
y <- on_demand_daily * x

# Calculate day of break-even point reserve vs on-demand rates
break_1year_light_x = reserve_1year_light / (on_demand_daily - reserve_1year_light_daily)
break_3year_light_x = reserve_3year_light / (on_demand_daily - reserve_3year_light_daily)
break_1year_medium_x = reserve_1year_medium / (on_demand_daily - reserve_1year_medium_daily)
break_3year_medium_x = reserve_3year_medium / (on_demand_daily - reserve_3year_medium_daily)
break_1year_heavy_x = reserve_1year_heavy / (on_demand_daily - reserve_1year_heavy_daily)
break_3year_heavy_x = reserve_3year_heavy / (on_demand_daily - reserve_3year_heavy_daily)

png(filename = "ec2_m1large_cost.png", width = width, height = height)
plot(x,y, type="l", col='red', xlab="", ylab="cost ($USD)")

# Title with summary information
title("EC2 cost analysis for m1.large",
      sprintf("(days)\nLight Utilization: 1 year is cheaper than on-demand after %.0f days, 3 year after %.0f days\nMedium Utilization: 1 year is cheaper than on-demand after %.0f days, 3 year after %.0f days\nHeavy Utilization: 1 year is cheaper than on-demand after %.0f days, 3 year after %.0f days", break_1year_light_x, break_3year_light_x, break_1year_medium_x, break_3year_medium_x, break_1year_heavy_x, break_3year_heavy_x))

# Arrays for legend
legend_names <- c(sprintf("On Demand ($%.2f/hour)", on_demand_hourly))
legend_colors <- c("red")
legend_line_types <- c(1)

# Light utilization
light_color="green"
if (light) {
    abline(reserve_1year_light, reserve_1year_light_daily, lty=1, lwd=line_thickness, col=light_color)
    point_y = reserve_1year_light + reserve_1year_light_daily * break_1year_light_x
    points(break_1year_light_x, point_y)
    text(break_1year_light_x, point_y, labels = sprintf("%.0f days", break_1year_light_x), pos=1)

    legend_names <- cbind(legend_names, sprintf("1 year light utilization ($%.0f+$%.2f/hour)", reserve_1year_light, reserve_1year_light_hourly))
    legend_colors <- cbind(legend_colors, light_color)
    legend_line_types <- cbind(legend_line_types, 1)


    if (three_year) {
        abline(reserve_3year, reserve_3year_light_daily, lty=2, lwd=line_thickness, col=light_color)
        point_y = reserve_3year + reserve_3year_light_daily * break_3year_light_x
        points(break_3year_light_x, point_y)
        text(break_3year_light_x, point_y, labels = sprintf("%.0f days", break_3year_light_x), pos=1)

        legend_names <- cbind(legend_names, sprintf("3 year light utilization ($%.0f+$%.2f/hour)", reserve_3year_light, reserve_3year_light_hourly))
        legend_colors <- cbind(legend_colors, light_color)
        legend_line_types <- cbind(legend_line_types, 2)
    }
}

# Medium utilization
medium_color="blue"
if (medium) {
    abline(reserve_1year_medium, reserve_1year_medium_daily, lty=1, lwd=line_thickness, col=medium_color)
    point_y = reserve_1year_medium + reserve_1year_medium_daily * break_1year_medium_x
    points(break_1year_medium_x, point_y)
    text(break_1year_medium_x, point_y, labels = sprintf("%.0f days", break_1year_medium_x), pos=1)

    legend_names <- cbind(legend_names, sprintf("1 year medium utilization ($%.0f+$%.2f/hour)", reserve_1year_medium, reserve_1year_medium_hourly))
    legend_colors <- cbind(legend_colors, medium_color)
    legend_line_types <- cbind(legend_line_types, 1)

    if (three_year) {
        abline(reserve_3year, reserve_3year_medium_daily, lty=2, lwd=line_thickness, col=medium_color)
        point_y = reserve_3year + reserve_3year_medium_daily * break_3year_medium_x
        points(break_3year_medium_x, point_y)
        text(break_3year_medium_x, point_y, labels = sprintf("%.0f days", break_3year_medium_x), pos=1)

        legend_names <- cbind(legend_names, sprintf("3 year medium utilization ($%.0f+$%.2f/hour)", reserve_3year_medium, reserve_3year_medium_hourly))
        legend_colors <- cbind(legend_colors, medium_color)
        legend_line_types <- cbind(legend_line_types, 2)
    }
}

#heavy utilization
heavy_color="orange"
if (heavy) {
    abline(reserve_1year_heavy, reserve_1year_heavy_daily, lty=1, lwd=line_thickness, col=heavy_color)
    point_y = reserve_1year_heavy + reserve_1year_heavy_daily * break_1year_heavy_x
    points(break_1year_heavy_x, point_y)
    text(break_1year_heavy_x, point_y, labels = sprintf("%.0f days", break_1year_heavy_x), pos=1)

    legend_names <- cbind(legend_names, sprintf("1 year heavy utilization ($%.0f+$%.2f/hour)", reserve_1year_heavy, reserve_1year_heavy_hourly))
    legend_colors <- cbind(legend_colors, heavy_color)
    legend_line_types <- cbind(legend_line_types, 1)

    if (three_year) {
        abline(reserve_3year, reserve_3year_heavy_daily, lty=2, lwd=line_thickness, col=heavy_color)
        point_y = reserve_3year + reserve_3year_heavy_daily * break_3year_heavy_x
        points(break_3year_heavy_x, point_y)
        text(break_3year_heavy_x, point_y, labels = sprintf("%.0f days", break_3year_heavy_x), pos=1)

        legend_names <- cbind(legend_names, sprintf("3 year heavy utilization ($%.0f+$%.2f/hour)", reserve_3year_heavy, reserve_3year_heavy_hourly))
        legend_colors <- cbind(legend_colors, heavy_color)
        legend_line_types <- cbind(legend_line_types, 2)
    }
}

# Legend
legend("topleft", legend_names, cex=1.2, col=legend_colors, lty=legend_line_types, lwd=line_thickness, bty="n")

dev.off()
quit()
