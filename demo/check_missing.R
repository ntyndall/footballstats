
# Remove greece + ukraine

# Only look at missing values
not.complete <- total.metrics %>%
  subset(total.metrics %>% complete.cases() %>% `!`())


for (i in 1:(not.complete %>% nrow)) {

  write()
  write('text' , file = '', append = TRUE)
  write('text' , file = '', append = TRUE)
}
