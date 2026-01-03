# Student Performance Analytics Dashboard
# animint2 Interactive Visualization

library(animint2)
library(data.table)

# Generate student performance data
set.seed(2024)

n_students <- 50
students <- paste0("S", sprintf("%03d", 1:n_students))
subjects <- c("Mathematics", "Physics", "Chemistry", "Biology", "English", "History")
semesters <- paste0("Sem", 1:8)
years <- c(rep("Year 1", 2), rep("Year 2", 2), rep("Year 3", 2), rep("Year 4", 2))

student_data <- data.table(expand.grid(
  student_id = students,
  subject = subjects,
  semester = semesters,
  stringsAsFactors = FALSE
))

student_data[, year := rep(years, each = 1)[as.numeric(gsub("Sem", "", semester))]]
student_data[, sem_num := as.numeric(gsub("Sem", "", semester))]

student_data[, `:=`(
  base_grade = 60 + sem_num * 2,
  student_ability = rnorm(n_students, mean = 0, sd = 10)[match(student_id, students)],
  subject_difficulty = c(Mathematics = -5, Physics = -3, Chemistry = -2, 
                        Biology = 0, English = 2, History = 3)[subject]
)]

student_data[, grade := base_grade + student_ability + subject_difficulty + rnorm(.N, 0, 5)]
student_data[grade > 100, grade := 100]
student_data[grade < 30, grade := 30 + runif(.N, 0, 10)]
student_data[, grade := round(grade, 1)]

student_data[, `:=`(
  study_hours = round(10 + (grade - 60) / 3 + rnorm(.N, 0, 3)),
  attendance = round(70 + (grade - 60) / 2 + rnorm(.N, 0, 5)),
  assignments_completed = round(runif(.N, 5, 20)),
  quiz_score = round(grade + rnorm(.N, 0, 8))
)]

student_data[study_hours < 2, study_hours := 2]
student_data[study_hours > 40, study_hours := 40]
student_data[attendance < 60, attendance := 60]
student_data[attendance > 100, attendance := 100]
student_data[quiz_score > 100, quiz_score := 100]
student_data[quiz_score < 20, quiz_score := 20]

student_data[, performance_category := cut(
  grade,
  breaks = c(0, 60, 75, 85, 100),
  labels = c("Needs Improvement", "Satisfactory", "Good", "Excellent"),
  include.lowest = TRUE
)]

student_data[, gender := sample(c("Male", "Female"), .N, replace = TRUE)]

# Faceting helper function
PANEL <- function(df, row_var, col_var) {
  data.frame(df,
             row_panel = factor(row_var, c("Grade Trends", "Performance Distribution")),
             col_panel = factor(col_var, c("By Semester", "By Subject")))
}

TRENDS_SEMESTER <- function(df) PANEL(df, "Grade Trends", "By Semester")
TRENDS_SUBJECT <- function(df) PANEL(df, "Grade Trends", "By Subject")
DIST_SEMESTER <- function(df) PANEL(df, "Performance Distribution", "By Semester")
DIST_SUBJECT <- function(df) PANEL(df, "Performance Distribution", "By Subject")

semesters_data <- unique(student_data[, .(semester, sem_num)])
subjects_data <- unique(student_data[, .(subject)])

# Plot 1: Grade progression over semesters
grade_trends <- ggplot() +
  theme_bw() +
  theme(
    panel.margin = grid::unit(0, "lines"),
    legend.position = "right"
  ) +
  theme_animint(width = 1000, height = 600) +
  geom_tallrect(aes(
    xmin = sem_num - 0.5,
    xmax = sem_num + 0.5),
    clickSelects = "semester",
    data = TRENDS_SEMESTER(semesters_data),
    alpha = 0.3) +
  geom_line(aes(
    x = sem_num,
    y = grade,
    group = interaction(student_id, subject),
    color = subject),
    clickSelects = "student_id",
    data = TRENDS_SEMESTER(student_data),
    size = 1,
    alpha = 0.15) +
  geom_point(aes(
    x = sem_num,
    y = grade,
    color = subject,
    size = study_hours,
    key = paste(student_id, subject, semester),
    tooltip = paste0(
      "Student: ", student_id, "\n",
      "Subject: ", subject, "\n",
      "Semester: ", semester, "\n",
      "Grade: ", grade, "%\n",
      "Study Hours: ", study_hours, "\n",
      "Attendance: ", attendance, "%")),
    showSelected = "student_id",
    clickSelects = "subject",
    data = TRENDS_SEMESTER(student_data),
    alpha = 0.8) +
  geom_text(aes(
    x = sem_num,
    y = grade,
    label = round(grade, 0),
    key = paste(student_id, subject, semester)),
    showSelected = "student_id",
    data = TRENDS_SEMESTER(student_data),
    size = 3,
    vjust = -1) +
  scale_size_continuous(range = c(2, 12), name = "Study Hours/Week") +
  facet_grid(row_panel ~ col_panel, scales = "free") +
  ggtitle("Student Performance Dashboard") +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = 1:8, labels = paste0("S", 1:8))

# Plot 2: Study hours vs grade scatter
study_scatter <- ggplot() +
  theme_bw() +
  theme_animint(width = 800, height = 500) +
  geom_point(aes(
    x = study_hours,
    y = grade,
    color = subject),
    data = student_data,
    alpha = 0.1,
    size = 1) +
  geom_point(aes(
    x = study_hours,
    y = grade,
    color = subject,
    size = attendance,
    key = paste(student_id, subject, semester),
    tooltip = paste0(
      "Student: ", student_id, "\n",
      "Subject: ", subject, "\n",
      "Grade: ", grade, "%\n",
      "Study Hours: ", study_hours, "\n",
      "Attendance: ", attendance, "%\n",
      "Performance: ", performance_category)),
    showSelected = "semester",
    clickSelects = c("student_id", "subject"),
    data = student_data,
    alpha = 0.7) +
  geom_smooth(aes(
    x = study_hours,
    y = grade,
    color = subject),
    showSelected = "semester",
    data = student_data,
    method = "lm",
    se = FALSE,
    size = 1.5,
    alpha = 0.5) +
  scale_size_continuous(range = c(2, 15), name = "Attendance %") +
  ggtitle("Study Hours vs Grade Analysis") +
  xlab("Weekly Study Hours") +
  ylab("Grade (%)")

# Plot 3: Subject performance comparison
subject_summary <- student_data[, .(
  median_grade = median(grade),
  mean_grade = mean(grade),
  min_grade = min(grade),
  max_grade = max(grade),
  count = .N
), by = .(semester, subject, sem_num)]

subject_comparison <- ggplot() +
  theme_bw() +
  theme_animint(width = 800, height = 500) +
  geom_bar(aes(
    x = subject,
    y = mean_grade,
    fill = subject,
    key = subject,
    tooltip = paste0(
      "Subject: ", subject, "\n",
      "Semester: ", semester, "\n",
      "Mean Grade: ", round(mean_grade, 1), "%\n",
      "Median Grade: ", round(median_grade, 1), "%\n",
      "Range: ", round(min_grade, 1), "-", round(max_grade, 1), "%\n",
      "Students: ", count)),
    showSelected = "semester",
    clickSelects = "subject",
    data = subject_summary,
    stat = "identity",
    alpha = 0.7) +
  geom_point(aes(
    x = subject,
    y = grade,
    color = subject,
    key = paste(student_id, subject)),
    showSelected = "semester",
    clickSelects = "subject",
    data = student_data,
    alpha = 0.3,
    size = 2,
    position = position_jitter(width = 0.2, height = 0)) +
  ggtitle("Subject Performance Comparison") +
  xlab("Subject") +
  ylab("Grade (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Student rankings
student_avg <- student_data[, .(
  avg_grade = mean(grade),
  total_study_hours = sum(study_hours),
  avg_attendance = mean(attendance)
), by = .(student_id, semester, sem_num)]

top_students <- student_avg[, .SD[order(-avg_grade)][1:20], by = semester]

ranking_plot <- ggplot() +
  theme_bw() +
  theme_animint(width = 800, height = 600) +
  geom_bar(aes(
    x = reorder(student_id, avg_grade),
    y = avg_grade,
    fill = avg_grade,
    key = student_id,
    tooltip = paste0(
      "Student: ", student_id, "\n",
      "Average Grade: ", round(avg_grade, 1), "%\n",
      "Study Hours: ", total_study_hours, "\n",
      "Attendance: ", round(avg_attendance, 1), "%")),
    showSelected = "semester",
    clickSelects = "student_id",
    data = top_students,
    stat = "identity") +
  scale_fill_gradient(low = "orange", high = "darkgreen", name = "Avg Grade") +
  coord_flip() +
  ggtitle("Top 20 Students by Average Grade") +
  xlab("Student ID") +
  ylab("Average Grade (%)")

# Plot 5: Attendance impact analysis
attendance_bins <- student_data[, .(
  avg_grade = mean(grade),
  count = .N
), by = .(attendance_bin = cut(attendance, breaks = seq(60, 100, by = 10)), 
         semester, subject)]

attendance_plot <- ggplot() +
  theme_bw() +
  theme_animint(width = 800, height = 400) +
  geom_bar(aes(
    x = attendance_bin,
    y = avg_grade,
    fill = subject,
    key = paste(attendance_bin, subject),
    tooltip = paste0(
      "Attendance: ", attendance_bin, "\n",
      "Subject: ", subject, "\n",
      "Avg Grade: ", round(avg_grade, 1), "%\n",
      "Students: ", count)),
    showSelected = "semester",
    clickSelects = "subject",
    data = attendance_bins[!is.na(attendance_bin)],
    stat = "identity",
    position = "dodge") +
  ggtitle("Impact of Attendance on Grades") +
  xlab("Attendance Range (%)") +
  ylab("Average Grade (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create interactive dashboard
complete_dashboard <- animint(
  title = "Student Performance Analytics Dashboard",
  source = "https://github.com/AviraL0013/student-performance-viz",
  
  trends = grade_trends,
  studyAnalysis = study_scatter,
  subjectComparison = subject_comparison,
  topPerformers = ranking_plot,
  attendanceImpact = attendance_plot,
  
  time = list(variable = "semester", ms = 3000),
  duration = list(semester = 1000, student_id = 500),
  
  first = list(
    semester = "Sem1",
    student_id = "S001",
    subject = "Mathematics"
  ),
  
  selector.types = list(
    student_id = "single",
    subject = "multiple",
    semester = "single"
  )
)

# Render visualization
output_dir <- "Student-Performance-Dashboard"
if(dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE)
}

animint2dir(complete_dashboard, output_dir)
fwrite(student_data, file.path(output_dir, "student_data.csv"))

cat("Visualization created in:", output_dir, "\n")
cat("To view: servr::httd('", output_dir, "')\n", sep = "")