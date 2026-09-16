
# Homework 1



# Problem 1: Objects, data structures, and subsetting

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# Part A 
section <- factor(section, levels = c("A", "B"))

students <- data.frame(student_id, section, quiz1, quiz2, passed)

score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id

course_record <- list(course = "R Programming", scores = students, cutoffs = c(pass = 70, excellent = 90))

# Inspect
typeof(quiz1)
class(quiz1)
length(quiz1)

typeof(section)
class(section)
levels(section)

typeof(students)
class(students)
dim(students)
str(students)

typeof(score_matrix)
class(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)


#  Part B 
score_matrix["S04", "quiz2"]

score_matrix[1:2, , drop = FALSE]

course_record["course"]
course_record[["course"]]
course_record$course


#  Part C 
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

keep <- students$section == "A" & students$average >= 80
sectionA_high <- students[keep, c("student_id", "section", "average")]
sectionA_high

avg_vec <- setNames(students$average, students$student_id)
avg_vec


# Problem 2: Importing, cleaning, and summarizing data
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#  Part A 
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

measurements$sample_id[!complete.cases(measurements)]


# Part B 
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7

rows <- complete.cases(measurements) & measurements$site %in% c("North", "South") & measurements$status == "ok"
selected <- measurements[rows, c("sample_id", "site", "temp_c", "temp_f", "ph")]
selected

mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

# Part C 
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B
dim(A * B)

A %*% B
dim(A %*% B)



# Problem 3: Functions
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# Part A 
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    NA_character_
  } else if (score >= a_min) {
    "A"
  } else if (score >= b_min) {
    "B"
  } else if (score >= c_min) {
    "C"
  } else if (score >= d_min) {
    "D"
  } else {
    "F"
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#  Part B 
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades


#  Part C-1 
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    total = length(x),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = na.rm), digits),
    sd = round(sd(x, na.rm = na.rm), digits),
    min = round(min(x, na.rm = na.rm), digits),
    max = round(max(x, na.rm = na.rm), digits)
  )
}

summarize_scores(scores)
summarize_scores(x = scores, na.rm = TRUE, digits = 2)

# Part C-2 
plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores")

