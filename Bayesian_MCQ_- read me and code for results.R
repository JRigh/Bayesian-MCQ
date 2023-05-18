#---------------------
# Bayesian MCQ
#---------------------

### example 1

# vector of possible answer and definition of the correct answer
possible_answers <- c("A","B","C","D")
TRUE_answer <- "B"

# vector of weights for an exact true answer
TRUE_answer_weight <- numeric(4)
TRUE_answer_weight[which(possible_answers %in% TRUE_answer)] <- 100
TRUE_answer_weight

# vector of student answers
Q1_answer <- 2
Q2_answer <- 60
Q3_answer <- 28
Q4_answer <- 30
Student_answer_weight <- c(Q1_answer, Q2_answer,
                           Q3_answer, Q4_answer)

# computation of the student score (quadratic loss)
1 - sum(((TRUE_answer_weight - Student_answer_weight)/100)^2)

# studentscore(): a function that return the score for one question
# It takes as argument TRUE_answer_weight (vector) and Student_answer_weight (vector)
studentscore <- function(TRUE_answer_weight,Student_answer_weight) {
  score <- 1 - sum(((TRUE_answer_weight - Student_answer_weight)/100)^2)
  return(score)
}

# scores for different students and different kind of wrong answers
# (1. the safe ignorant, the true probabilist, the overconfident ignorant)

student1_the_safe_ignorant_answer <- rep(25,4) # assigns 25% for each answer
student2_the_true_probabilist_answer <- c(1,45,4,50) # fail to assign the maximum probability to the correct answer
student3_the_overconfident_ignorant_answer <- c(0,0,0,100) # assigns 100% on a wrong answer

studentscore(TRUE_answer_weight,student1_the_safe_ignorant_answer)
# [1] 0.25
studentscore(TRUE_answer_weight,student2_the_true_probabilist_answer)
# [1] 0.4458
studentscore(TRUE_answer_weight,student3_the_overconfident_ignorant_answer)
# [1] -1



### example 2

# vector of possible answer and definition of vector of correct answers (10 questions)
possible_answers <- c("A","B","C","D")
number_of_questions <- 10

# answer_matrix() :
# function that return a matrix with the location and weight for the correct answers
# It takes as argument the number of questions (integer) and a vector of possible answers

answer_matrix <- function(number_of_questions, TRUE_answer_vector) {
  
  TRUE_answer_matrix <- matrix(rep(0, 4*number_of_questions),
                               ncol=number_of_questions)
  all_questions <- rep(possible_answers, number_of_questions)
  all_questions_matrix <- matrix(all_questions, ncol=number_of_questions)
  correct_index <- numeric(number_of_questions)
  
  for(i in 1:number_of_questions) {
  correct_index[i] <- which(all_questions_matrix[,i] %in% TRUE_answer_vector[i])
  TRUE_answer_matrix[,i][correct_index[i]] <- 100
  }
  
return(TRUE_answer_matrix)
}

# for example, if we have 10 questions and for those questions,
# the correct answers are repsectively "B" "A" "C" "B" "C" "C" "A" "C" "C" "D"
set.seed(1986)
example_TRUE_answers <-sample(possible_answers,10,replace = TRUE)
example_TRUE_answers

# we get
answer_matrix(10, example_TRUE_answers)

# suppose that the student answered the following
set.seed(123456)
example <- matrix(sample(1:100, 4*number_of_questions, replace = TRUE),ncol = number_of_questions, byrow = TRUE) 
example <- prop.table(example, margin=2)*100
example
colSums(example)

# computation of the score
final_score <- number_of_questions - (sum(((answer_matrix(10, example_TRUE_answers)- example)/100)^2))
final_score
# 2.940521


### exemple 3

# Bayesian_MCQ():
# function that takes as arguments (1) the number of questions (integer)
# (2) the correct answers (vector) and
# (3) the student solutions (matrix)

Bayesian_MCQ <- function(number_of_questions,TRUE_answer_vector, student_solution) {
  
  # vector of possible answer and definition of vector of correct answers (10 questions)
  possible_answers <- c("A","B","C","D")
  
  answer_matrix <- function(number_of_questions, TRUE_answer_vector) {
    
    TRUE_answer_matrix <- matrix(rep(0, 4*number_of_questions),
                                 ncol=number_of_questions)
    all_questions <- rep(possible_answers, number_of_questions)
    all_questions_matrix <- matrix(all_questions, ncol=number_of_questions)
    correct_index <- numeric(number_of_questions)
    
    for(i in 1:number_of_questions) {
      correct_index[i] <- which(all_questions_matrix[,i] %in% TRUE_answer_vector[i])
      TRUE_answer_matrix[,i][correct_index[i]] <- 100
    }
    
    return(TRUE_answer_matrix)
  }
  
  # computation of the score
  final_score <- number_of_questions - (sum(((answer_matrix(10, example_TRUE_answers) - student_solution)/100)^2))
  return(final_score)
}


# suppose that the student answered the following (random)
set.seed(2021)
student1_solution <- matrix(sample(1:100, 4*number_of_questions, replace = TRUE),ncol = number_of_questions, byrow = TRUE) 
student1_solution <- prop.table(student_solution, margin=2)*100
student1_solution

# the correct answers are repsectively "B" "D" "C" "B" "C" "C" "C" "B" "D" "D"
set.seed(2021)
example_TRUE_answers_1 <-sample(possible_answers,10,replace = TRUE)
example_TRUE_answers_1

# students score and final grade

# student score (the score can vary from -10 (worst) to 10 (best))
Bayesian_MCQ(number_of_questions = 10,
             TRUE_answer_vector = example_TRUE_answers_1,
             student_solution = student1_solution)
# slighty better than the average 0, but not much
# [1] 2.487524

# let us suppose we grade on a scale of 1 (worst) to 100 (best)
# and we get 50 for a score of 0
max_grade = 100

# final grade 
final_grade = (Bayesian_MCQ(number_of_questions = 10,
                            TRUE_answer_vector = example_TRUE_answers_1,
                            student_solution = student1_solution) * (max_grade / (2*number_of_questions))) + (max_grade / 2)


round(final_grade,2)
# [1] 62.44


#--------------------------
# Code to test the students
#--------------------------

#----------------------------------------------------------------------
# whole code for Bayesian Multiple Choices Questionnaires (MCQ)
#----------------------------------------------------------------------
# Bayesian_MCQ():
# function that takes as arguments (1) the number of questions (integer)
# (2) the correct answers (vector) and
# (3) the student solutions (matrix)
Bayesian_MCQ <- function(number_of_questions,TRUE_answer_vector, student_solution) {
  # vector of possible answer and definition of vector of correct answers (10 questions)
  possible_answers <- c("A","B","C","D")
  number_of_questions = number_of_questions
  answer_matrix <- function(number_of_questions, TRUE_answer_vector) {
    TRUE_answer_matrix <- matrix(rep(0, 4*number_of_questions),
                                 ncol=number_of_questions)
    all_questions <- rep(possible_answers, number_of_questions)
    all_questions_matrix <- matrix(all_questions, ncol=number_of_questions)
    correct_index <- numeric(number_of_questions)
    for(i in 1:number_of_questions) {
      correct_index[i] <- which(all_questions_matrix[,i] %in% TRUE_answer_vector[i])
      TRUE_answer_matrix[,i][correct_index[i]] <- 100
    }
    return(TRUE_answer_matrix)
  }
  # computation of the score
  final_score <- number_of_questions - (sum(((answer_matrix(number_of_questions, TRUE_answer_vector) - student_solution)/100)^2))
  return(final_score)
}
# suppose that the student answered the following (random)
set.seed(2021)
student1_solution <- matrix(sample(1:100, 4*number_of_questions, replace = TRUE),ncol = number_of_questions, byrow = TRUE) 
student1_solution <- prop.table(student_solution, margin=2)*100
# the correct answers are repsectively "B" "D" "C" "B" "C" "C" "C" "B" "D" "D"
set.seed(2021)
example_TRUE_answers_1 <-sample(possible_answers,4,replace = TRUE)
# students score and final grade for completely random answers
# student score (the score can vary from -10 (worst) to 10 (best))
Bayesian_MCQ(number_of_questions = 4,
             TRUE_answer_vector = example_TRUE_answers_1,
             student_solution = student1_solution)
# slighty better than the average 0, but not much
# [1] 2.487524
# let us suppose we grade on a scale of 1 (worst) to 100 (best)
# and we get 50 for a score of 0
max_grade = 100
# final grade 
final_grade = (Bayesian_MCQ(number_of_questions = 10,
                            TRUE_answer_vector = example_TRUE_answers_1,
                            student_solution = student1_solution) * (max_grade / (2*number_of_questions))) + (max_grade / 2)
round(final_grade,2)
# [1] 62.44

### example: grade of my test friend :))
number_of_questions = 4
solutions_of_my_test_friend <- as.matrix(cbind(c(0,0,100,0),
                                               c(0,100,0,0),
                                               c(0,100,0,0),
                                               c(100,0,0,0)))
# final grade 
final_grade = (Bayesian_MCQ(number_of_questions = 4,
                            TRUE_answer_vector = c("C","B","B","A"),
                            student_solution = solutions_of_my_test_friend) * (max_grade / (2*number_of_questions))) + (max_grade / 2)
round(final_grade,2)
# [1] 100
