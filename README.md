# myproject_airbnb
This project aims at predicting Airbnb's customer's destination for the first time. 
By finding out new user's behaviors, Airbnb can share more personalized content with their community, decrease the average time to first booking, and
better forecast demand.

Data Description
id: user id
date_account_created: the date of account creation
timestamp_first_active: timestamp of the first activity, note that it can be earlier than date_account_created or date_first_booking because a user can search before signing up
date_first_booking: date of first booking gender
age
signup_method
signup_flow: the page a user came to signup up from
language: international language preference
affiliate_channel: what kind of paid marketing
affiliate_provider: where the marketing is e.g. google, craigslist, other
first_affiliate_tracked: whats the first marketing the user interacted with before the signing up
signup_app
first_device_type
first_browser
country_destination: this is the target variable you are to predict
