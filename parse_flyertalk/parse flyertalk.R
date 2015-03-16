library(RCurl)
library(stringr)
# Which URLs are we going after?
url_base = "http://www.flyertalk.com/forum/chase-ultimate-rewards/1330475-ua-mileageplus-explorer-card-up-55k-miles-50-statement-credit"
page_separator = "-"
url_tail = ".html"
# How can we start isolating the messages?
message_separator = "status icon and date -->"

# Let's establish the bounds for the date, which is all I care about right now.
date_prefix = "</a>\r\n\t\t\t"
date_start_shift = str_length(date_prefix)
date_suffix = "\r\n\t\t\t\r\n\t\t\t<!-- / "
date_end_margin = str_length(date_suffix)

# This loop will be for every page
for (i in 1:184) {
  # switch the url structure on i
  if (i == 1) {
    url = str_c(url_base, url_tail)
  }
  else {
    url = str_c(url_base, page_separator, as.character(i), url_tail)
  }
  # now get the URL
  page = getURI(url)
  # split the page source into messages that we can use
  message_blocks = str_split(page, message_separator)
  # how many messages are there?
  section_count = length(message_blocks[[1]])
  # now let's get it to an even number
  if (section_count %% 2 == 1) section_count = section_count - 1
  # and the actual message headers that we want are in ever other message
  message_count = section_count/2

  # if this is the first loop, we'll need to "create" post_dates
  if (!exists("post_dates")) {
    post_dates = NULL
    first_post_on_the_page = 0
  }
  # if it's not, we need to find out how long it is
  else {
    first_post_on_the_page = length(post_dates)
  }
  # extend the string for the post dates
  post_dates = c(post_dates, character(message_count))
  # now to loop through and fill the message headers string correctly.
  for (j in message_count:1) {
    # hold the text in a temp variable for clean code
    temp_message = message_blocks[[1]][[j*2]]
    # where does the date we want start?
    date_start = date_start_shift + str_locate(temp_message, date_prefix)
    # and where does it end?
    date_end = str_length(temp_message) - date_end_margin
    # let's pull it out as it's own variable
    date = substr(temp_message, date_start[[1]][[1]], date_end)
    # Index our current position:
    current_post = first_post_on_the_page + j
    # now add it into the page's vector of dates
    post_dates[[current_post]] = date  
  }
}

post_dates
post_info = data.frame(post_dates)
# first let's get the month, and set up the rest of the date
post_info$month_end = str_locate(post_info$post_dates, " ")
post_info$month = substr(post_info$post_dates, 1, post_info$month_end - 1)
post_info$remainder = substr(post_info$post_dates, post_info$month_end + 1, 100)
# using the same approach, get the date
post_info$date_end = str_locate(post_info$remainder, ",")
post_info$date = as.numeric(substr(post_info$remainder, 1, post_info$date_end - 1))
post_info$remainder = substr(post_info$remainder, post_info$date_end + 2, 100)
# using the same approach, get the year
post_info$year_end = str_locate(post_info$remainder, ",")
post_info$year = as.numeric(substr(post_info$remainder, 1, post_info$year_end - 1))
post_info$remainder = substr(post_info$remainder, post_info$year_end + 2, 100)
# using the same approach, get the hour
post_info$hour_end = str_locate(post_info$remainder, ":")
post_info$hour = as.numeric(substr(post_info$remainder, 1, post_info$hour_end - 1))
post_info$remainder = substr(post_info$remainder, post_info$hour_end + 1, 100)
# using the same approach, get the minutes
post_info$minutes_end = str_locate(post_info$remainder, " ")
post_info$minutes = as.numeric(substr(post_info$remainder, 1, post_info$minutes_end - 1))
post_info$remainder = substr(post_info$remainder, post_info$minutes_end + 1, 100)
# the daypart is now what's left in the remainder

posts = subset(post_info, select = c("month", "date", "year", "hour", "minutes", "remainder"))
write.csv(posts, "flyertalk posts.csv")
