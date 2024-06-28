
plot_percent <- function(all_eval_p, title){
  allTerms <- unique(all_eval_p$term)
  
  # Initialize a list to store plots
  p <- list()
  
  for (i in allTerms) {
    term_data <- subset(all_eval_p, term == i)
    
    # Create ggplot for each term
    plot <- ggplot(term_data, aes(x = bias, y = Method, xmin = lower, xmax = upper, color = Method)) +
      geom_pointrange() +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Add intercept at 0
      labs(title = paste("Term:", i), x = "Bias", y = "") +  # Leave y-axis label blank initially
      theme_bw() +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5))  # Remove legend
    
    # Add the plot to the list
    p[[i]] <- plot
    
    # If not the first plot in the list, hide the y-axis label
    if (i %in% c(allTerms[2], allTerms[3], allTerms[5])) {
      p[[i]] <- plot + theme(axis.text.y = element_blank())
    }
  }
  
  plot_widths <- rep(1, 3)  # Equal width for each plot
  plot_widths[1] <- 1.5  # Increase width for the first plot
  
  # Arrange plots in a grid with ncol = 4 (4 plots per row)
  combined_plot <- arrangeGrob(grobs = p, widths = plot_widths, ncol = 3, top = title)
  
  # Print or view the combined plot
  return(combined_plot)
} 

plot_cov_2 <- function(all_eval_p, title){
  allTerms <- unique(all_eval_p$term)
  p = list()
  
  for(i in allTerms){
    term_data <- subset(all_eval_p, term == i)
    
    plot<- ggplot(term_data, aes(x = cov, y = Method, xmin = cov, xmax = cov), color = "blue") +
      geom_pointrange(shape = 1, size = 1, aes(xmin = cov, xmax = cov), color = "blue", fill = "blue") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  # Add intercept at 0
      labs(title = paste("Term:", i), x = "Coverage", y = "Method") +
      theme_bw() +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5))  # Remove legend
    
    # Add the plot to the list
    p[[i]] <- plot
    
    # If not the first plot in the list, hide the y-axis label
    if (i %in% c(allTerms[2], allTerms[3], allTerms[5])) {
      p[[i]] <- plot + theme(axis.text.y = element_blank())
    }
  }
  
  plot_widths <- rep(1, 3)  # Equal width for each plot
  plot_widths[1] <- 1.5  # Increase width for the first plot
  
  # Arrange plots in a grid with ncol = 4 (4 plots per row)
  combined_plot <- arrangeGrob(grobs = p, widths = plot_widths, ncol = 3, top = title)
  
  # Print or view the combined plot
  return(combined_plot)
}




time_arrange <- function(data,method){
  
  time_df <- map_df(names(data),function(y) { 
    map_df(data[[y]], 
           ~ tibble(Timetaken = .x[3], MissingPercent = y))}) %>%
    mutate(method = method)
  
  return(time_df)
}



plot_cov <- function(all_eval_p){
  allTerms <- unique(all_eval_p$term)
  p = list()
  
  for(i in allTerms){
    term_data <- subset(all_eval_p, term == i)
    
    p[[i]] <- ggplot(term_data) +
      geom_col(aes(x = type, y = cov, fill = Method)) +
      scale_y_continuous(breaks = round(seq(0,1, length.out = 10),1)) +
      labs(title = paste("Term:", i), x = "Type", y = "Coverage") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    
    
  }
  return(p)
}


data_transform <- function(dt, p){
  combined_data <- lapply(names(dt), function(name) {
    data <- dt[[name]]
    data <- mutate(data, term = rownames(data), name = name,  Method = p)
    rownames(data) <- NULL  # Remove rownames
    return(data)
  })
  
  combined_data <- bind_rows(combined_data)
  
}
