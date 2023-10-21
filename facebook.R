library(tidyr)
library(dplyr)
library(readxl)
library(readr)

data <- read_csv("C:/Users/ibeha/OneDrive/Desktop/MY R DATASET/Facebook ad dataset/KAG_conversion_data.csv")
View(data)
glimpse(data)

unique(data$age)
### "30-34" "35-39" "40-44" "45-49", they are in character data type, we can take a mean of the ages
new_data <- data
new_data$age[new_data$age == "30-34"] <- 32
new_data$age[new_data$age == "35-39"] <- 37
new_data$age[new_data$age == "40-44"] <- 42
new_data$age[new_data$age == "45-49"] <- 47

# convert variable to integer
new_data$age <- as.integer(new_data$age)
str(new_data$age)
###chnage gender to 1 and 0
new_data$gender[new_data$gender == "M"] <- 0
new_data$gender[new_data$gender == "F"] <- 1

new_data$gender <- as.integer(new_data$gender)
glimpse(new_data)

# abbreviate some variable names
new_data <- new_data %>%
 rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
         conv = Total_Conversion, appConv = Approved_Conversion)

library(heatmaply)
dataMatNorm <- as.matrix(normalize(new_data, method = "standardize"))
heatmap(dataMatNorm)
#Click-through rate (CTR) is a metric used in online advertising and digital marketing to 
#measure the effectiveness of an online advertising campaign or a specific advertisement.
#CTR is expressed as a percentage and represents the proportion of people who click on an ad 
#or a specific link within a webpage, email, or other digital content, relative to the 
#total number of people who were exposed to the ad (impressions) or had the opportunity to click (clicks).
#The formula to calculate CTR is:
#CTR=CTR=( Number of Clicks/ Number of Impressions or Opportunities)×100

#Key points about CTR:
  
#1. *Measurement of Engagement:* CTR is a fundamental metric used to measure user engagement with digital content, 
#such as banner ads, text ads, email campaigns, and search engine results.

#2. *Indication of Relevance:* A higher CTR typically indicates that the ad or content is more relevant and 
#appealing to the audience. It suggests that a larger percentage of viewers found the content interesting enough to click on.

#3. *Benchmarking:* CTR serves as a benchmark for evaluating the performance of different ads or campaigns. 
#Marketers can compare the CTR of various advertisements to determine which ones are more effective in driving user engagement.

#4. *Optimization:* Marketers and advertisers use CTR data to optimize their campaigns. They may adjust ad copy, design, 
#placement, targeting, or other factors to increase CTR and improve campaign performance.

#5. *Quality Score:* In pay-per-click (PPC) advertising platforms like Google Ads, CTR is an important component 
#of the Quality Score, which affects ad rankings and costs. Higher CTR can lead to lower costs per click (CPC) and better ad positions.

#6. *Limitations:* While CTR is a valuable metric, it doesn't provide a complete picture of the success of a campaign. 
#A high CTR doesn't guarantee conversions or sales. It's essential to consider other metrics such as conversion rate, return on investment (ROI), 
#and overall campaign goals to assess the full impact of a marketing campaign.

#Conversion Rate
#Conversion Rate (CR) is a key performance indicator (KPI) used in marketing and e-commerce to measure the 
#effectiveness of a marketing campaign, website, or online sales funnel. It represents the percentage of 
#visitors or users who take a desired action, referred to as a "conversion," out of the total number of 
#visitors or users who had the opportunity to take that action. Conversion Rate is expressed as a percentage.

#The formula to calculate Conversion Rate is:
  
#CR=( Number of Conversions//Total Number of Visitors or Users)×100 
#Key points about Conversion Rate:
  
# 1. *Measurement of Success:* Conversion Rate is a crucial metric for evaluating the success of marketing 
#efforts or specific web pages. It indicates how effective a website or campaign is at persuading visitors to complete a desired action, 
#such as making a purchase, signing up for a newsletter, filling out a form, or any other predefined goal.

#2. *Types of Conversions:* Conversions can vary depending on the specific goal of a campaign or website. For e-commerce, a conversion might be a 
#completed purchase. For a lead generation website, it might be the submission of a contact form. Different organizations and websites define conversions
#based on their objectives.

#3. *Benchmarking and Optimization:* Marketers use Conversion Rate to benchmark the performance of different 
#marketing channels, campaigns, or website elements. By analyzing Conversion Rate data, they can identify areas for improvement 
#and optimize their strategies.

#4. *A/B Testing:* Conversion Rate optimization often involves conducting A/B tests or split tests, where different versions of a 
#webpage or advertisement are compared to determine which one generates a higher Conversion Rate.

#5. *Funnel Analysis:* Conversion Rate is particularly valuable in analyzing sales or conversion funnels. 
#Marketers can identify drop-off points in the funnel where users are not converting as expected and take steps to improve those areas.

#6. *Segmentation:* Marketers may segment their audience to analyze Conversion Rate for different groups, such as new vs.
#returning visitors or visitors from different geographic regions. This can provide insights into audience behavior and preferences.

#7. *Landing Pages:* Conversion Rate is commonly used to assess the effectiveness of landing pages, 
#which are designed to convert visitors into customers or leads. High-performing landing pages often have higher Conversion Rates.

#8. *Multi-Channel Attribution:* In digital marketing, Conversion Rate can be used as part of multi-channel attribution models to understand the 
#contributions of different marketing channels to conversions.

#Cost Per Click (CPC) 

#Cost Per Click (CPC) is a common digital advertising metric used by advertisers and marketers to measure the cost-effectiveness of
#online advertising campaigns, particularly in pay-per-click (PPC) advertising models. CPC represents the average cost that an advertiser 
#pays each time a user clicks on their ad. It is a crucial metric for assessing the efficiency and return on investment (ROI) of online advertising efforts.

#Here are key points to understand about Cost Per Click (CPC):
  
#1. *Definition:* CPC is calculated by dividing the total cost of an advertising campaign by the total number of clicks generated by the campaign. The formula is: 
  
#CPC= Total Cost of Campaign/ Total Number of Click
#2. *Payment Model:* In a PPC advertising model, advertisers only pay when a user clicks on their ad. Advertisers set a maximum bid (the maximum CPC they are willing to pay) for specific keywords or ad placements. The actual CPC paid can be lower than the maximum bid, depending on competition and ad quality.

#3. *Keyword and Ad Auctions:* In platforms like Google Ads, advertisers participate in keyword auctions, where the highest bidder for a keyword wins ad placement. However, other factors, such as ad quality and relevance, also influence ad placement and CPC.

#4. *Budget Control:* CPC allows advertisers to control their advertising budget effectively. Advertisers can set daily or campaign budgets to limit spending and prevent overspending.

#5. *Performance Measurement:* CPC is a key performance indicator (KPI) used to evaluate the performance of specific keywords, ads, campaigns, or advertising channels. Advertisers can assess which keywords or ads are driving clicks at a reasonable cost and adjust their strategies accordingly.

#6. *Ad Quality:* Advertisers who create high-quality, relevant ads can often achieve a lower CPC and better ad placements. Search engines and advertising platforms reward ads that provide a good user experience.

#7. *ROI Calculation:* Advertisers use CPC data in conjunction with conversion data to calculate the return on investment (ROI) of their advertising campaigns. By comparing the cost per click to the revenue generated from conversions, they can determine the profitability of their campaigns.

#8. *Competitive Landscape:* The CPC for specific keywords or industries can vary widely based on competition. High-demand keywords often have higher CPCs, while less competitive keywords may have lower CPCs.

#9. *Bid Strategies:* Advertisers may employ various bid strategies, such as manual bidding or automated bidding, to optimize CPC and campaign performance.

#10. *Display Advertising:* CPC is not limited to search engine advertising. It is also used in display advertising, social media advertising, and other online advertising channels.


#CLICK PER CONVERTION

#Cost Per Conversion (CPC) is a digital marketing metric that measures the cost incurred by a business or 
#advertiser for each successful conversion in an advertising campaign. A conversion is a predefined action that a user takes on a website or digital platform, such as making a purchase, filling out a contact form, signing up for a newsletter, or any other action that aligns with the campaign's goals. CPC provides insights into how efficiently an advertising campaign or marketing strategy is at driving desired actions from users.

#Here are key points to understand about Cost Per Conversion (CPC):

#1. **Definition:** CPC is calculated by dividing the total cost of an advertising campaign by the total number of conversions generated by that campaign. The formula is:
#CPC=Total Cost of Campaign// Total Number of Conversions
#2. **Measurement of Efficiency:** CPC is a crucial metric for evaluating the cost-effectiveness and return on investment (ROI) of marketing campaigns. It quantifies how much an advertiser pays on average for each successful conversion.

#3. **Conversion Types:** Conversions can vary depending on the campaign's objectives. They can include actions such as online sales, lead generation, form submissions, app downloads, or other actions relevant to the business's goals.

#4. **ROI Analysis:** Advertisers use CPC data in conjunction with revenue data (e.g., sales revenue, revenue generated from leads) to calculate the ROI of their campaigns. By comparing the CPC to the revenue generated from conversions, they can assess campaign profitability.

#5. **Optimization:** Marketers and advertisers often seek to optimize their campaigns to achieve a lower CPC while maintaining or increasing the conversion rate. Strategies may involve adjusting ad copy, targeting, landing pages, or bidding strategies.

#6. **Budget Management:** CPC allows advertisers to manage their advertising budgets effectively. By knowing the cost associated with each conversion, they can allocate resources to campaigns that deliver the best results.

#7. **Keyword and Ad Performance:** In pay-per-click (PPC) advertising, CPC data helps advertisers assess the performance of specific keywords, ads, and ad groups. It guides decisions on keyword bids and ad quality improvements.

#8. **A/B Testing:** Advertisers may conduct A/B tests to compare the performance of different ad variations. CPC data is used to determine which ad variant is more cost-effective at driving conversions.

#9. **Quality and Relevance:** Advertisers who provide high-quality, relevant content and landing pages often achieve a lower CPC because search engines and advertising platforms reward ads that enhance user experience.

#10. **Conversion Tracking:** Effective conversion tracking is essential for calculating CPC accurately. Businesses need to set up tracking mechanisms to capture and attribute conversions to specific campaigns or sources.


#Create the additional KPIs with dplyr create the CTR and CPC figures

new_data <- new_data %>%
  mutate(CTR = ((Clicks/impr)* 100), CPC = Spent/Clicks)
##round to 4 decemail place

new_data$CTR <- round(new_data$CTR, 4)
new_data$CPC <- round(new_data$CPC, 2)

# create trimmed dataset
dataTfTrim <- new_data %>%
  select(CTR, CPC, appConv, conv, impr, Spent, Clicks)

# omit missing values, normalise data, calculate correlations and plot heatmap
heatmap(cor(normalize(na.omit(dataTfTrim))))

# Install and load reshape2 package
library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(dataTfTrim),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
#remove NA
melted_corr_mat <- na.omit(melted_corr_mat)
# head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4)
##From our broad overview of the data, we can see that the more we spend, the more clicks and conversions we seem to get. That's quite reassuring to know, 
#but doesn't really give us the 'actionable insight' we were looking for.

##For our next stage in the analysis, we'll look at a specific campaign in more detail and see what we can pull out of it. First of all, let's
#choose a campaign, the one on which we regularly spend the most money and regularly get the most conversions
# Group by Campaign Name and calculate total spending and conversions
campaign_summary <- new_data %>%
  group_by(xyzCampId) %>%
  summarise(TotalSpending = sum(Spent, na.rm = TRUE),
            TotalConversions = sum(appConv, na.rm = TRUE))

# Find the campaign with the highest spending and the most conversions
top_campaign <- campaign_summary %>%
  filter(TotalSpending == max(TotalSpending) & TotalConversions == max(TotalConversions))

# Print the top campaign
print(top_campaign)

##we can see that the 1178 has the most spending and impression

data1178 <- data %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
         conv = Total_Conversion, appConv = Approved_Conversion) %>%
  filter(xyzCampId == 1178)

library(DataExplorer)
plot_missing(data1178)

#Nothing missing, good to know. Let's look at the distributions of our data, variable by variable:
options(repr.plot.width=4, repr.plot.height=4)
plot_bar(data1178)

options(repr.plot.width=8, repr.plot.height=4)
plot_histogram(data1178)

#feature engineering
# Return on Advertising Spend (ROAS)
#let's assume that an enquiry (Total conversion, conv) is worth £5, and a sale (Approved conversion, appConv),
#is worth £100. We can now create our conversion value-based variables sing mutate:

data1178 <- data1178 %>%
  mutate(totConv = conv + appConv,
         conVal = conv * 5,
         appConVal = appConv * 100) %>%
  mutate(totConVal = conVal + appConVal) %>%
  mutate(costPerCon = round(Spent / totConv, 2),
         ROAS = round(totConVal / Spent, 2))

#Cost Per Mille (CPM). This number is the cost of one thousand impressions. 
#If your objective is ad exposure to increase brand awareness, this might be an important KPI for you to measure.
data1178 <- data1178 %>%
  mutate(CPM = round((Spent / impr) * 1000, 2))

options(repr.plot.width=6, repr.plot.height=3)
ggplot(data1178, aes(Spent, totConv)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total number of conersions")
ggplot(data1178, aes(Spent, totConVal)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total value of conversions")

options(repr.plot.width=4, repr.plot.height=3)
ggplot(data1178, aes(gender, ROAS)) + geom_boxplot() + scale_y_log10()

#The data look quite symmetrical with a log-transformed axis, but without the log-transformation, it doesn't fit the normal distribution, 
#so we'll look to see if this difference is significant using a non-parametric test:

wilcox.test(ROAS ~ gender, data=data1178)

data1178 %>%
  select(gender, ROAS) %>%
  group_by(gender) %>%
  filter(ROAS != 'Inf') %>%
  summarise(medianROAS = median(ROAS), meanROAS = mean(ROAS))

#However, we have the data to go a lot more granular than this, 
#so let's see how else we can break the dataset apart. We'll look at interests next:

options(repr.plot.width=8, repr.plot.height=3)
ggplot(data1178, aes(as.factor(interest), Clicks)) + geom_boxplot() +
  labs(x = "Interest Identifier", y = "Number of Clicks")

options(repr.plot.width=8, repr.plot.height=3)
data1178 %>%
  ggplot(aes(as.factor(interest), ROAS)) + geom_boxplot() + scale_y_log10() +
  labs(x = "Interest Identifier", y = "ROAS")

data1178 %>%
  select(interest, ROAS, Clicks) %>%
  group_by(interest) %>%
  filter(ROAS != 'Inf') %>%
  summarise(medianROAS = round(median(ROAS) ,2), 
            meanROAS = round(mean(ROAS), 2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS)) %>%
  head(n = 10)

options(repr.plot.width=8, repr.plot.height=3)
data1178 %>%
  filter(interest == 101 | interest == 15 | interest == 21) %>%
  ggplot(aes(x = as.factor(interest), y = ROAS, fill = gender)) + geom_boxplot() + scale_y_log10() +
  labs(x = 'Interest ID', y = 'ROAS')

data1178 %>%
  select(interest, gender, ROAS, Clicks) %>%
  group_by(interest, gender) %>%
  filter(ROAS != 'Inf', interest == 101 | interest == 15 | interest == 21) %>%
  summarise(medianROAS = round(median(ROAS), 2),
            meanROAS = round(mean(ROAS) ,2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS))

#Analysis by age¶
#We've been able to break apart this campaign by interest and gender, with each split allowing us to pull out groups with the highest ROAS. What about the age variable though, 
#we haven't used that yet, so let's see if we can be even more targeted:

options(repr.plot.width=8, repr.plot.height=4)
data1178 %>%
  filter(interest == 21 | interest == 15 & gender == 'M') %>%
  group_by(age, interest) %>% 
  ggplot(aes(x = as.factor(age), y = ROAS, fill = as.factor(interest))) + geom_boxplot() + scale_y_log10() +
  labs(x = 'Age group', y = 'ROAS') + scale_fill_discrete(name="Interest\nID")

data1178 %>%
  select(age, interest, gender, ROAS, Clicks) %>%
  group_by(age, interest) %>%
  filter(ROAS != 'Inf', interest == 21 | interest == 15, gender == 'M') %>%
  summarise(medianROAS = round(median(ROAS), 2),
            meanROAS = round(mean(ROAS) ,2), clicks = sum(Clicks)) %>%
  arrange(desc(meanROAS))
