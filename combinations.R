#===This is the code used to generate the combinations of events of the 4 most important co-morbidities for the GDM cohort.  The dataframes generated have a possible combination per row.

#===Diabetic eye disease
generate_combinations <- function(target_sum) {
  valid_combinations <- list()
  for (i1 in 0:0) { 
  for (i2 in 1:4) {
    for (i3 in 1:4) {
      for (i4 in 1:4) {
        for (i5 in 8:8) {
          for (i6 in 12:12) {
            for (i7 in 15:15) {
              for (i8 in 7:7) {
                for (i9 in 10:10) {
                  for (i10 in 7:7) {
                    for (i11 in 8:8) {
                      # Calculate the sum of the numbers
                      total_sum <- i1+ i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11
                      
                      # Check if the combination meets the criteria
                      if (total_sum == target_sum &&
                          (i2 >= 1 && i2 <= 4 &&
                           i3 >= 1 && i3 <= 4 &&
                           i4 >= 1 && i4 <= 4)) {
                        combination <- c(0, i2, i3, i4, 8, 12, 15, 7, 10, 7, 8)
                        valid_combinations <- append(valid_combinations, list(combination))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  }
  
  return(valid_combinations)
}

# Generate combinations
valid_combinations <- generate_combinations(75)

# Print valid combinations
print(valid_combinations)

# Convert list to dataframe
valid_combinations_df <- as.data.frame(do.call(rbind, valid_combinations))

#===HTN
generate_combinations <- function(target_sum) {
  valid_combinations_htn <- list()
  for (i1 in 7:7) { 
    for (i2 in 7:7) {
      for (i3 in 1:4) {
        for (i4 in 1:4) {
          for (i5 in 1:4) {
            for (i6 in 7:7) {
              for (i7 in 5:5) {
                for (i8 in 7:7) {
                  for (i9 in 6:6) {
                    for (i10 in 8:8) {
                      for (i11 in 1:4) {
                        # Calculate the sum of the numbers
                        total_sum <- i1+ i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11
                        
                        # Check if the combination meets the criteria
                        if (total_sum == target_sum &&
                            (i3 >= 1 && i3 <= 4 &&
                             i4 >= 1 && i4 <= 4 &&
                             i5 >= 1 && i5 <= 4 &&
                             i11 >= 1 && i11 <=4)) {
                          combination <- c(7, 7, i3, i4, i5, 7, 5, 7, 6, 8, i11)
                          valid_combinations_htn <- append(valid_combinations_htn, list(combination))
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(valid_combinations_htn)
}

# Generate combinations
valid_combinations_htn <- generate_combinations(55)

# Print valid combinations
print(valid_combinations_htn)

# Convert list to dataframe
valid_combinations_htn <- as.data.frame(do.call(rbind, valid_combinations_htn))

#===IHD

generate_combinations <- function(target_sum) {
  valid_combinations_IHD <- list()
  for (i1 in 2:2) { 
    for (i2 in 1:4) {
      for (i3 in 0:0) {
        for (i4 in 0:0) {
          for (i5 in 0:0) {
            for (i6 in 1:4) {
              for (i7 in 1:4) {
                for (i8 in 1:4) {
                  for (i9 in 5:5) {
                    for (i10 in 0:0) {
                      for (i11 in 1:4) {
                        # Calculate the sum of the numbers
                        total_sum <- i1+ i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11
                        
                        # Check if the combination meets the criteria
                        if (total_sum == target_sum &&
                            (i2 >= 1 && i2 <= 4 &&
                             i6 >= 1 && i6 <= 4 &&
                             i7 >= 1 && i7 <= 4 &&
                             i8 >= 1 && i8 <= 4 &&
                             i11 >= 1 && i11 <=4)) {
                          combination <- c(2, i2, 0, 0, 0, i6, i7, i8, 5, 0, i11)
                          valid_combinations_IHD <- append(valid_combinations_IHD, list(combination))
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(valid_combinations_IHD)
}

# Generate combinations
valid_combinations_IHD <- generate_combinations(14)

# Print valid combinations
print(valid_combinations_IHD)

# Convert list to dataframe
valid_combinations_IHD <- as.data.frame(do.call(rbind, valid_combinations_IHD))


#=====================  FATTY LIVER

generate_combinations <- function(target_sum) {
  valid_combinations_NAFL <- list()
  for (i1 in 1:1) { 
    for (i2 in 1:4) {
      for (i3 in 1:4) {
        for (i4 in 0:0) {
          for (i5 in 1:4) {
            for (i6 in 1:4) {
              for (i7 in 1:4) {
                for (i8 in 8:8) {
                  for (i9 in 1:4) {
                    for (i10 in 0:0) {
                      for (i11 in 1:4) {
                        # Calculate the sum of the numbers
                        total_sum <- i1+ i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11
                        
                        # Check if the combination meets the criteria
                        if (total_sum == target_sum &&
                            (i2 >= 1 && i2 <= 4 &&
                             i3 >= 1 && i3 <= 4 &&
                             i5 >= 1 && i5 <= 4 &&
                             i6 >= 1 && i6 <= 4 &&
                             i7 >= 1 && i7 <= 4 &&
                             i9 >= 1 && i9 <= 4 &&
                             i11 >= 1 && i11 <=4)) {
                          combination <- c(1, i2, i3, 0, i5, i6, i7, 8, i9, 0, i11)
                          valid_combinations_NAFL <- append(valid_combinations_NAFL, list(combination))
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(valid_combinations_NAFL)
}

# Generate combinations
valid_combinations_NAFL <- generate_combinations(28)

# Print valid combinations
print(valid_combinations_NAFL)

# Convert list to dataframe
valid_combinations_NAFL <- as.data.frame(do.call(rbind, valid_combinations_NAFL))

