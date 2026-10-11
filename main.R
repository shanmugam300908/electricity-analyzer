
# ============================================================
# SMART ELECTRICITY CONSUMPTION ANALYZER
# R ANALYSIS + AUTOMATIC WEB DASHBOARD
# ============================================================

# 1. PROJECT FOLDERS

project_folder <- getwd()

output_folder <- file.path(
  project_folder,
  "Electricity_Analysis_Results"
)

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# 2. SETTINGS

set.seed(123)

tariff <- 7

appliances <- c(
  "Air Conditioner",
  "Water Heater",
  "Washing Machine",
  "Refrigerator",
  "Electric Fan",
  "Television"
)

# 3. GENERATE SAMPLE ELECTRICITY DATA

days <- 1:20

electricity_data <- expand.grid(
  Day = days,
  Appliance = appliances
)

base_usage <- c(8.5, 8, 2.5, 0.8, 0.5, 0.5)

electricity_data$Energy_kWh <- round(
  rep(base_usage, each = length(days)) *
    runif(nrow(electricity_data), 0.7, 1.3),
  2
)

electricity_data$Estimated_Cost <- round(
  electricity_data$Energy_kWh * tariff, 2
)

electricity_data$Usage_Category <- cut(
  electricity_data$Energy_kWh,
  breaks = c(-Inf, 2, 6, Inf),
  labels = c("Low", "Moderate", "High")
)

# 4. CREATE SUMMARIES

appliance_summary <- aggregate(
  cbind(Energy_kWh, Estimated_Cost) ~ Appliance,
  data = electricity_data,
  FUN = sum
)

appliance_summary <- appliance_summary[
  order(appliance_summary$Energy_kWh, decreasing = TRUE),
]

daily_summary <- aggregate(
  Energy_kWh ~ Day,
  data = electricity_data,
  FUN = sum
)

category_summary <- as.data.frame(
  table(electricity_data$Usage_Category)
)

names(category_summary) <- c("Usage_Category", "Records")

# 5. SAVE CSV FILES

write.csv(
  electricity_data,
  file.path(output_folder, "Electricity_Dataset.csv"),
  row.names = FALSE
)

write.csv(
  appliance_summary,
  file.path(output_folder, "Appliance_Summary.csv"),
  row.names = FALSE
)

write.csv(
  daily_summary,
  file.path(output_folder, "Daily_Summary.csv"),
  row.names = FALSE
)

write.csv(
  category_summary,
  file.path(output_folder, "Usage_Category_Summary.csv"),
  row.names = FALSE
)

# 6. GRAPH 1: ENERGY BY APPLIANCE

png(
  file.path(output_folder, "01_Appliance_Consumption.png"),
  width = 1000,
  height = 700
)

par(mar = c(9, 5, 4, 2))

barplot(
  appliance_summary$Energy_kWh,
  names.arg = appliance_summary$Appliance,
  las = 2,
  col = "steelblue",
  main = "Energy Consumption by Appliance",
  ylab = "Energy (kWh)",
  xlab = ""
)

dev.off()

# 7. GRAPH 2: DAILY ENERGY CONSUMPTION

png(
  file.path(output_folder, "02_Daily_Consumption.png"),
  width = 1000,
  height = 650
)

plot(
  daily_summary$Day,
  daily_summary$Energy_kWh,
  type = "o",
  pch = 16,
  col = "darkgreen",
  main = "Daily Electricity Consumption",
  xlab = "Day",
  ylab = "Energy (kWh)"
)

dev.off()

# 8. GRAPH 3: ENERGY DISTRIBUTION

png(
  file.path(output_folder, "03_Energy_Distribution.png"),
  width = 900,
  height = 650
)

hist(
  electricity_data$Energy_kWh,
  breaks = 15,
  col = "orange",
  border = "white",
  main = "Distribution of Energy Consumption",
  xlab = "Energy (kWh)",
  ylab = "Number of Records"
)

dev.off()

# 9. GRAPH 4: COST BY APPLIANCE

png(
  file.path(output_folder, "04_Appliance_Cost.png"),
  width = 1000,
  height = 700
)

par(mar = c(9, 5, 4, 2))

barplot(
  appliance_summary$Estimated_Cost,
  names.arg = appliance_summary$Appliance,
  las = 2,
  col = "tomato",
  main = "Estimated Electricity Cost by Appliance",
  ylab = "Estimated Cost (Rs.)",
  xlab = ""
)

dev.off()

# 10. CALCULATE DASHBOARD VALUES

total_energy <- sum(electricity_data$Energy_kWh)
total_cost <- sum(electricity_data$Estimated_Cost)
average_energy <- mean(electricity_data$Energy_kWh)

# 11. CREATE HTML DASHBOARD

dashboard_file <- file.path(project_folder, "index.html")

chart_files <- c(
  "01_Appliance_Consumption.png",
  "02_Daily_Consumption.png",
  "03_Energy_Distribution.png",
  "04_Appliance_Cost.png"
)

chart_titles <- c(
  "Energy by Appliance",
  "Daily Energy Consumption",
  "Energy Distribution",
  "Estimated Cost by Appliance"
)

chart_descriptions <- c(
  "Compare energy use across appliances.",
  "Review the daily energy consumption trend.",
  "Explore the distribution of sample readings.",
  "Compare estimated costs using the sample tariff."
)

chart_html <- paste(
  vapply(seq_along(chart_files), function(i) {
    paste0(
      '<article class="chart"><h3>',
      chart_titles[i],
      '</h3><p>',
      chart_descriptions[i],
      '</p><img src="Electricity_Analysis_Results/',
      chart_files[i],
      '" alt="',
      chart_titles[i],
      '"></article>'
    )
  }, character(1)),
  collapse = "\n"
)

table_rows <- paste(
  vapply(seq_len(nrow(appliance_summary)), function(i) {
    paste0(
      "<tr><td>", appliance_summary$Appliance[i],
      "</td><td>", sprintf("%.2f", appliance_summary$Energy_kWh[i]),
      "</td><td>Rs. ", sprintf("%.2f", appliance_summary$Estimated_Cost[i]),
      "</td></tr>"
    )
  }, character(1)),
  collapse = "\n"
)

html <- paste0(
  '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Smart Electricity Consumption Analyzer</title>
<style>
*{box-sizing:border-box}
body{margin:0;background:#08111f;color:#edf5ff;font-family:Arial,sans-serif;line-height:1.6}
header{padding:55px 7%;background:linear-gradient(120deg,#102238,#10534b)}
header p{color:#c0d7dd}
h1{font-size:clamp(2rem,5vw,3.8rem);margin:10px 0}
h1 span{color:#7cf2c3}
main{max-width:1150px;margin:auto;padding:30px 22px}
h2{margin-top:35px}
.stats{display:grid;grid-template-columns:repeat(3,1fr);gap:16px}
.card,.chart,.tablebox{background:#111e30;padding:20px;border:1px solid #29394d;border-radius:15px}
.card p,.chart p{color:#a8bbcf}
.card strong{font-size:clamp(1.4rem,3vw,2rem);color:#7cf2c3}
.charts{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:16px}
.chart img{display:block;width:100%;height:auto;background:white;border-radius:8px}
.tablebox{overflow-x:auto}
table{width:100%;border-collapse:collapse;min-width:450px}
th,td{text-align:left;padding:13px;border-bottom:1px solid #29394d}
th{color:#7cf2c3}
.note{background:#102238;color:#c0d0df;padding:14px;border-radius:10px}
footer{padding:25px 0;color:#91a7bd}
@media(max-width:700px){.stats,.charts{grid-template-columns:1fr}header{padding:38px 22px}}
</style>
</head>
<body>
<header>
<p>ENERGY INSIGHTS DASHBOARD</p>
<h1>Smart Electricity<br><span>Consumption Analyzer</span></h1>
<p>Explore appliance consumption, daily energy trends and estimated electricity costs.</p>
</header>
<main>
<p class="note">This dashboard uses simulated sample data generated by R, not live meter readings. The tariff of Rs. 7 per kWh is illustrative.</p>
<h2>Project Overview</h2>
<section class="stats">
<article class="card"><p>Total Energy</p><strong>',
  sprintf("%.2f", total_energy),
  ' kWh</strong><p>Sample dataset</p></article>
<article class="card"><p>Estimated Cost</p><strong>Rs. ',
  sprintf("%.2f", total_cost),
  '</strong><p>At Rs. 7 per kWh</p></article>
<article class="card"><p>Average per Record</p><strong>',
  sprintf("%.2f", average_energy),
  ' kWh</strong><p>Sample average</p></article>
</section>
<h2>Energy Analysis Graphs</h2>
<section class="charts">',
  chart_html,
  '</section>
<h2>Appliance Summary</h2>
<div class="tablebox"><table>
<thead><tr><th>Appliance</th><th>Energy (kWh)</th><th>Estimated Cost</th></tr></thead>
<tbody>',
  table_rows,
  '</tbody></table></div>
<h2>Key Insight</h2>
<p class="note">Use the appliance summary to identify which devices account for the most energy in this simulated dataset. Actual household results will depend on your own readings and usage.</p>
<footer>Smart Electricity Consumption Analyzer | Created using R, HTML and CSS</footer>
</main></body></html>'
)

writeLines(html, dashboard_file)

# 12. SAVE TEXT REPORT

report <- c(
  "SMART ELECTRICITY CONSUMPTION ANALYZER",
  "--------------------------------------",
  paste("Total energy:", round(total_energy, 2), "kWh"),
  paste("Estimated cost: Rs.", round(total_cost, 2)),
  paste("Average energy per record:", round(average_energy, 2), "kWh"),
  paste("Illustrative tariff: Rs.", tariff, "per kWh"),
  "",
  "Note: The dataset is simulated, not actual meter data."
)

writeLines(
  report,
  file.path(output_folder, "Electricity_Analysis_Report.txt")
)

# 13. OPEN THE WEBPAGE

cat("\nPROJECT COMPLETED SUCCESSFULLY!\n")
cat("Dashboard created at:", dashboard_file, "\n")
cat("Graphs and CSV files saved in:", output_folder, "\n")

browseURL(normalizePath(dashboard_file))
