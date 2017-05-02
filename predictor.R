####load required lirarie
library(dplyr)
library(tm)
library(SnowballC)
library(cldr)

data_sgt_file <- "sgt_model"

data_model_file <- "data_model"

####ftn to calculate the simple touring abd return the resulting data frame
calculate_touring <- function(model){
    
    #### reate frequency table
    frequecy_table <- table(model$freq)
    
    ########create data frame for sqt
    sqt_df <- data.frame(
        r=as.numeric(names(frequecy_table)),
        n=as.vector(frequecy_table),
        Z=vector("numeric",length(frequecy_table)), 
        logr=vector("numeric",length(frequecy_table)),
        logZ=vector("numeric",length(frequecy_table)),
        r_star=vector("numeric",length(frequecy_table)),
        p=vector("numeric",length(frequecy_table)))
    
    ####create var to store number of rows in sqt dataframe
    number_rows <- nrow(sqt_df)

    for (j in 1:number_rows) {
        if(j == 1) {
          
            r_i <- 0
            
        } else {
          
            r_i <- sqt_df$r[j-1]
        }
        if(j == number_rows) {
          
            r_k <- sqt_df$r[j]
            
        } else {
          
            r_k <- sqt_df$r[j+1]
            
        }
      
        sqt_df$Z[j] <- 2 * sqt_df$n[j] / (r_k - r_i)
        
    }
    
    sqt_df$logr <- log(sqt_df$r)
    
    sqt_df$logZ <- log(sqt_df$Z)
    
    linear_fit <- lm(sqt_df$logZ ~ sqt_df$logr)
    
    coefficient1 <- linear_fit$coefficients[1]
    
    coefficient2 <- linear_fit$coefficients[2]
    
    use_y = FALSE
    
    ####loop through for the number of rows
    for (j in 1:(number_rows-1)) {
      
        r_plus1 <- sqt_df$r[j] + 1
        
        sr_plus1 <- exp(coefficient1 + (coefficient2 * sqt_df$logr[j+1]))
        
        sr <- exp(coefficient1 + (coefficient2 * sqt_df$logr[j]))
        
        y <- r_plus1 * sr_plus1/sr
        
        if(use_y) {
          
            sqt_df$r_star[j] <- y
            
        } else { 
          
            n_r_plus1 <- sqt_df$n[sqt_df$r == r_plus1]
            
            if(length(n_r_plus1) == 0 ) {
              
                sqt_df$r_star[j] <- y
                
                use_y = TRUE
                
            } else {
              
                n_r <- sqt_df$n[j]
                
                x <- (r_plus1) * n_r_plus1/n_r
                
                if (abs(x-y) > 1.96 * sqrt(((r_plus1)^2) * (n_r_plus1/((n_r)^2))*(1+(n_r_plus1/n_r)))) {
                    
                  sqt_df$r_star[j] <- x
                  
                } else {
                    
                  sqt_df$r_star[j] <- y
                  
                  use_y = TRUE
                }
            }
        }
        if(j==(number_rows-1)) {
          
            sqt_df$r_star[j+1] <- y
        }
    }
    N <- sum(sqt_df$n * sqt_df$r)
    
    Nhat <- sum(sqt_df$n * sqt_df$r_star)
    
    Po <- sqt_df$n[1] / N
    
    sqt_df$p <- (1-Po) * sqt_df$r_star/Nhat
    
    return(sqt_df)
}

####function for predicting the next word
predict_next_word <- function(test_sentence, model, sgt, valid_results=NULL) {
    
    ####set options
    options("scipen"=100, "digits"=8)
    
    test_sentence_list <- unlist(strsplit(test_sentence," "))
    
    number_of_words <- length(test_sentence_list)
    
    result_dataframe <- data.frame(word4 = factor(), probAdj = numeric())
    
    predict_ngram(result_dataframe, "w1w2w3", sgt$w1w2w3, valid_results,
                 model$w1w2w3 %>% filter(word1 == test_sentence_list[number_of_words-2], 
                                         word2 == test_sentence_list[number_of_words-1], 
                                         word3 == test_sentence_list[number_of_words]))
    
    predict_ngram(result_dataframe, "w2w3", sgt$w2w3, valid_results, 
                 model$w2w3 %>% filter(word2 == test_sentence_list[number_of_words-1], 
                                       word3 == test_sentence_list[number_of_words]))
    
    predict_ngram(result_dataframe, "w3", sgt$w3, valid_results, 
                 model$w3 %>% filter(word3 == test_sentence_list[number_of_words]))
    
    
    predict_ngram(result_dataframe, "w1w2", sgt$w1w2, valid_results, 
                 model$w1w2 %>% filter(word1 == test_sentence_list[number_of_words-2], 
                                       word2 == test_sentence_list[number_of_words-1]))
    
    predict_ngram(result_dataframe, "w1w3", sgt$w1w3, valid_results, 
                 model$w1w3 %>% filter(word1 == test_sentence_list[number_of_words-2], 
                                       word3 == test_sentence_list[number_of_words]))
    
    predict_ngram(result_dataframe, "w1", sgt$w1, valid_results, 
                 model$w1 %>% filter(word1 == test_sentence_list[number_of_words-2]))
    
    return(result_dataframe %>% arrange(desc(probAdj)))
  
}

####function for predicting the desired ngram
predict_ngram <- function(result_dataframe, label_name, sgt, valid_results, sub_gram) {
    if(nrow(sub_gram) > 0 & !(nrow(result_dataframe) > 0)) {
        
        ####print(label_name)
        sub_gram$probAdj <- sapply(sub_gram$freq, FUN = function(x) sgt$p[sgt$r == x])
        
        sub_gram <- sub_gram %>% select(word4, probAdj)
        
        if(!is.null(valid_results) & nrow(sub_gram) > 0) {
          
            sub_gram <- sub_gram %>% filter(word4 %in% valid_results)
            
        }
        eval.parent(substitute(result_dataframe <- sub_gram))
    }
}

####ftn for creating test sentence
clean_sentence <- function(test_sentence) {
  
    test_sentence <- stripWhitespace(test_sentence)
    
    test_sentence <- tolower(test_sentence)
    
    test_sentence <- removeNumbers(test_sentence)
    
    test_sentence <- removePunctuation(test_sentence, preserve_intra_word_dashes = TRUE)
  
    return(test_sentence)
}

####ftn for predictign word
predict_word <- function(sentence) {
  
    sentence <- clean_sentence(sentence)
    
    sentence_list <- unlist(strsplit(sentence," "))
    
    number_of_words <- length(sentence_list)
    
    if(number_of_words >= 3) {
      
        return(predict_next_word(paste(
            sentence_list[number_of_words-2], 
            sentence_list[number_of_words-1], 
            sentence_list[number_of_words]), predictor.model, predictor.sgt))
   
      } else if(number_of_words == 2) {
        
        return(predict_next_word(paste(
            "-", 
            sentence_list[number_of_words-1], 
            sentence_list[number_of_words]), predictor.model, predictor.sgt))
    } else if(number_of_words == 1) {
        return(predict_next_word(paste(
            "-", 
            "-", 
            sentence_list[number_of_words]), predictor.model, predictor.sgt))
    }
}

####read in data model and data sgt file
model <- readRDS(sprintf("%s.rds", data_model_file))

sgt <- readRDS(sprintf("%s.rds", data_sgt_file))

####add elements to predictor
predictor <- list()

predictor.model <- model

predictor.sgt <- sgt

predictor.predict_word <- predict_word