

generate_pbar <- function(percentage, num_chars = 13) {
    num_filled <- round(percentage*num_chars)
    num_empty <- num_chars-num_filled
    
    display_percentage <- sprintf("%.1f", percentage*100)
    
    msg  <- paste0(paste0(rep('█', num_filled), collapse = ""), 
                   paste0(rep('░', num_empty), collapse = ""), " ",
                   display_percentage, "%",collapse = "")
    
    return(msg)
    
}


