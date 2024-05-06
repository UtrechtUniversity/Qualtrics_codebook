library(tidyverse)
library(qualtRics)
library(sjlabelled)

codebook <- function(survey_id){
  # codebook (value labels) for Qualtrics study. Output contains both the original
  # and the recoded values
   
  description <- fetch_description(survey_id)
  
  questions <- description$questions
  
  codebook_options <- data.frame()

  for (question in names(questions)) {
    
    question_qid <- question
    question_id <- questions[[question]]$DataExportTag

    if ('Choices' %in% names(questions[[question]])){
      
      labels <- data.frame()
      
      # Display element contains the actual label, there are 
      # other labels in some elements which leads to duplicates
      # therefore as_tibble(questions[[question]]$Choices) doesn't work
      for (choice in names(questions[[question]]$Choices)) {
        
        value <- choice
        
        label <- questions[[question]]$Choices[[choice]]$Display
        
        value_label <- data.frame(value = choice,
                                  label = label)

        labels <- rbind(labels, value_label)
        
      }
      
      # label-value combinations are assigned automatically by Qualtrics so
      # some label-values are recoded. Custom combinations always need a recode
      if ('RecodeValues' %in% names(questions[[question]])){
      
        recodes <- data.frame()
      
        for (option in names(questions[[question]]$RecodeValues)) {
          
          original_option <- option
          
          recode <- questions[[question]]$RecodeValues[[original_option]]
          
          value_recode <- data.frame(original_option = original_option,
                                     recode = recode)
          
          recodes <- rbind(recodes, value_recode)
          
        }
        
        labels <- labels %>%
          mutate(question_qid = question_qid,
                 question_id = question_id) %>%
          left_join(recodes, by = join_by('value' == 'original_option'))
        
      } else{
        
        labels <- labels %>%
          mutate(question_qid = question_qid,
                 question_id = question_id)
      
      }
    
      codebook_options <- bind_rows(codebook_options, labels)
    
    }
  }
  
  return(codebook_options)
  
}



variable_labels <- function(survey_id){
  
  description <- fetch_description(survey_id)
  
  questions <- description$questions
  
  variable_labels <- data.frame()

  for (question in names(questions)) {
    
    question_qid <- question
    
    question_id <- questions[[question]]$DataExportTag
    
    question_label <- questions[[question]]$QuestionDescription
    
    question_metadata <- data.frame(question_qid = question_qid,
                                    question_id = question_id,
                                    question_label = question_label)
    
    variable_labels <- bind_rows(variable_labels, question_metadata)

  }

  return(variable_labels)
  
}

# Questions with ChoiceTextEntryValue in the name have yet another structure
# regarding piped text
# find questions with choice entry text. These have TextEntry = true in the choice
# metadata. Not very efficient code but no time/desire to improve now
get_choice_text_vars <- function(survey_id){
  
  description <- fetch_description(survey_id)
  
  questions <- description$questions
  
  choice_text_entry_vars <- c()
  
  for (question in names(questions)){
    
    if ('Choices' %in% names(questions[[question]])){
      
      question_qid <- question
      
      for (choice in questions[[question]]$Choices){
        
        if ('TextEntry' %in% names(choice) && choice$TextEntry == 'true') {
          
          choice_text_entry_vars <- append(choice_text_entry_vars, question_qid)
          
        }
      }
    }
  }
  
  return(unique(choice_text_entry_vars))

}







