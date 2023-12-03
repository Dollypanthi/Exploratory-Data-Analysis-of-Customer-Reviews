#Getting the current working directory
getwd()

#Setting new working directory
setwd('C:/KMPlayer/archive (4)')
># Loading the readr package
library(readr)
# Loading the readr package
library(readr)

#Import the dataset and specifying the column types for "Price" as numeric and "Rate" as integer, treating "NA" as missing values.
flipkart <- read_csv("flipkart_product.csv", col_types = cols(Price = col_number(), Rate = col_integer()), na = "NA")
View(flipkart)

# Removing Rows with missing values
sum(is.na(flipkart))
flipkart <- na.omit(flipkart)

# Checking the structure of the Summary Column
str(flipkart$Summary)

# Now we have to remove punctuation from summary column
library(dplyr)
library(stringi) # for advanced string manipulation 

# The stri_trans_general function from the stringi package is used here to convert non-Latin characters to their closest Latin equivalents.
flipkart <- flipkart %>%
  mutate(
    Summary = stri_trans_general(Summary, "Latin-ASCII"),
    ProductName = stri_trans_general(ProductName, "Latin-ASCII"))

# Removing the punctuation
flipkart <- flipkart %>%
  mutate(Summary = gsub("[[:punct:]]", "", Summary))
View(flipkart)

######## Removing characters with undefined names
str(flipkart)
library(dplyr)
library(stringr)

#This gsub function removes all characters that are not letters, digits, spaces, or forward slashes.
flipkart <- flipkart %>%
  mutate(
    ProductName = gsub("[^a-zA-Z0-9/ ]", "", ProductName))
View(flipkart)

# The gsub function replaces characters with undefined names in the specified columns with a space.
flipkart <- flipkart %>%
  mutate(
    Review = gsub('[^a-zA-Z0-9(/)]', ' ', Review),
    Summary = gsub('[^a-zA-Z0-9(/)]', ' ', Summary))

# The 'str_squish' function collapses multiple adjacent white spaces into a single space.
flipkart <- flipkart %>%
  mutate(
    Summary = str_squish(Summary),
    Review = str_squish(Review))
flipkart <- flipkart %>%
  mutate(
    Summary = tolower(Summary),
    Review = tolower(Review))

View(flipkart)

# Saving the data frame flipkart to a CSV file
write.csv(flipkart, "clean_flipkart_Products.csv", row.names = FALSE)
View(flipkart)



plot(flipkart$Price, flipkart$Rate, col = 'blue', pch = 16, main = 'Ratings vs. Prices', xlab = 'Price', ylab = 'Rating')


summary(flipkart)

# Calculate the counts of each unique value in 'Rate'
star_counts <- table(flipart$Rate)

# Create a bar plot
barplot(star_counts,
        main = 'Count of Reviews by Stars',
        xlab = 'Review Stars',
        ylab = 'Count',
        col = 'skyblue',  # You can customize the color
        ylim = c(0, max(star_counts) + 5),  # Adjust ylim for better visualization
        beside = TRUE  # Display bars beside each other
)

# Set x-axis label
axis(1, at = seq_along(star_counts), labels = names(star_counts))

# Display the plot
dev.off()


# Adjust plot parameters as needed






















View(flipkart)
# Check if columns exist
if ('ReviewSentiment' %in% colnames(flipkart) && 'SummarySentiment' %in% colnames(flipkart)) {
  
  # Check for missing values
  if (any(is.na(flipkart$ReviewSentiment)) || any(is.na(flipkart$SummarySentiment))) {
    # Handle missing values if necessary
    print("Warning: Missing values detected.")
  } else {
    # Combine sentiments
    flipkart$CombinedSentiment <- (flipkart$ReviewSentiment + flipkart$SummarySentiment) / 2
  }
  
} else {
  print("Error: Columns 'ReviewSentiment' and/or 'SummarySentiment' not found.")
}

# Assuming 'df' is your data frame with columns 'ReviewSentiment' and 'SummarySentiment'
flipkart$CombinedSentiment = (flipkart$ReviewSentiment + flipkart$SummarySentiment) / 2
# Assuming 'df' is your data frame
print(head(flipkart))
# Assuming 'df' is your data frame
library(dplyr)

# Copy 'CombinedSentiment' to 'SentimentScore'
flipkart$SentimentScore <- flipkart$CombinedSentiment

# Print the first few rows
print(head(flipkart))

# Drop columns 'ReviewSentiment', 'SummarySentiment', 'CombinedSentiment'
flipkart <- select(flipkart, -c(ReviewSentiment, SummarySentiment, CombinedSentiment))

# Print the first few rows again
print(head(flipkart))
head(flipkart)


# Assuming 'df' is your data frame
# Assuming 'SentimentCategory' is a factor (categorical) column in your DataFrame
# You can convert it to a factor if it's not already
flipkart$SentimentCategory <- as.factor(flipkart$SentimentCategory)

# Get the counts of each sentiment category
sentiment_counts <- table(flipkart$SentimentCategory)

# Plotting the bar chart
barplot(sentiment_counts, col=c('red', 'grey', 'green'),
        main='Distribution of Sentiment Categories',
        xlab='Sentiment Category',
        ylab='Number of Sentiments')

pie(flipkart,
    labels = levels(flipkart$RatingCategory),
    main = 'Distribution of Product Ratings',
    col = c('red', 'orange', 'yellow', 'green', 'blue', 'purple'),
    clockwise = TRUE,
    init.angle = 140,
    border = 'white')
