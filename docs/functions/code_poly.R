# Build polynomial predictor variables
#
# Takes a data frame, name of predictor variable, and polynomial order. Creates
# polynomial-transformed predictor variables, adds them to the data frame and
# returns the result. Original data frame is unchanged, remember to assign the
# result.
#
# @param df data frame, should not contain any variables called "predictor"
# @param predictor string name of predictor variable
# @param poly.order integer order of polynomial to be created
# @param orthogonal logical value indicating whether polynomial should be
#   orthogonal (default) or natural (aka raw)
# @param draw.poly logical value indicating whether to create a graph showing
#   tranformed polynomial predictor values, defaults to TRUE
# @return Returns a data frame containing the original data and at least two
#   new columns: "predictor".Index, and a column for each order of the
#   polynomial-transformed predictor
# @export
# @examples
# WordLearnEx.gca <- code_poly(df=WordLearnEx, predictor="Block", poly.order=2)
# Az.gca <- code_poly(df=Az, predictor="Time", poly.order=2, orthogonal=FALSE)
code_poly <- function(df=NULL, predictor=NULL, poly.order=NULL, orthogonal=TRUE, draw.poly=TRUE){
  if (draw.poly){
    require(ggplot2)
  }
  
  # convert choice for orthogonal into choice for raw
  raw <- (orthogonal-1)^2
  
  # Make sure that the declared predictor is actually present in the data.frame
  if (!predictor %in% names(df)){
    stop(paste0(predictor, " is not a variable in your data frame. Check spelling and try again"))
  }
  
  # Extract the vector to be used as the predictor
  predictor.vector <- df[[predictor]]
  
  # Create index of predictor (e.g. numbered time bins)
  # The index of the time bin will be used later as an index to call the time sample.
  predictor.indices <- as.numeric(as.factor(predictor.vector))
  
  df$temp.predictor.index <- predictor.indices
  
  # Create n-order order polynomials (orthogonal if not raw)
  predictor.polynomial <- poly(x = unique(sort(predictor.vector)),
                               degree = poly.order, raw=raw)
  
  # Use predictor index as index to align
  #   polynomial-transformed predictor values with original dataset
  #   (as many as called for by the polynomial order).
  df[, paste("poly", 1:poly.order, sep="")] <-
    predictor.polynomial[predictor.indices, 1:poly.order]
  
  # Draw a plot of the polynomial transformations, if desired
  if (draw.poly == TRUE){
    # extract the polynomials from the df
    df.poly <- unique(df[c(predictor, paste("poly", 1:poly.order, sep=""))])
    
    # intuitive level names for plot
    better_names <- c(Linear = "poly1", Quadratic = "poly2", 
                      Cubic = "poly3",  Quartic = "poly4", 
                      Quintic = "poly5", Sextic = "poly6")
    
    # pivot from wide to long format
    df.poly.melt <- df.poly  %>%
      #tidyr::gather(variable, value, -predictor)
      rename(any_of(better_names)) %>% 
      pivot_longer(any_of(c("Linear", "Quadratic", "Cubic",
                            "Quartic", "Quintic", "Sextic")),
                   names_to = "Order")
    
    poly.plot <- ggplot(df.poly.melt, 
                        aes(x = .data[[predictor]], y=value, color=Order))+
      geom_line()+
      xlab(paste0(predictor, "\n (raw predictor)"))+
      ylab("Transformed polynomial value")+
      scale_color_brewer(palette="Set1")+
      theme_bw()
    
    print(poly.plot)
  }
  
  # Restore correct column names
  colnames(df)[colnames(df) == "temp.predictor.index"] <- paste0(predictor,".Index")
  
  # Return the original data.frame with polynomial columns
  # that can be used as model predictors
  return(df)
}