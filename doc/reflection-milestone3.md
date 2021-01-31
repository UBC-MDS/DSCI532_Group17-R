## Reflection for Milestone 3

Marco Ma, Guanshu Tao, Yuan Xiong, Marc Sun

#### Things Implemented

# R version:

For this milestone, our group constructed a dashboard with R and followed the structure from what we had developed from milestone 1 and 2. We displayed a dashboard about FIFA Stars from Kaggle's "FIFA 19 complete player dataset" and users are able to explore different aspects of football stars. On our dashboard, there are four dropdown boxes on the left, a 15-row-table in the middle with another dropbox on top of it, and two histograms on the right. With the left dropdown boxes, users can select an attribute from the list: 'Value', 'Overall', 'Potential', 'Height', 'Weight'. Then they can sort them with descending or ascending orders and filtering by continent and clubs. With the top dropbox, users can decide which attribute to display except for "Ranking" and "Name" columns. The histograms are about the sum of "Overall" score from FIFA stars VS "Club" and "Nationality", respectively and their y-axis will change along with filtering from users' clicking.

# Python version:

We changed our table from 10 rows to 15 rows and added a histogram of selected attribute VS ranking at the bottom. For the two histograms on the right, we merged them together and let users select which one to display.

# Things not yet Implemented

In the coming week, we are going to improve our R version project in the same way as python version. In addition, we will add colours to our table and charts as well as re-organize them to make it more attractive. We will work on making our dashboard more flexible so that the users will be able to choose what range of rankings they want through dragging dots on a bar under the table. We would also optimize the y-scale of the histogram and add tooltips to the bars in order to give the audience a clear sense about this information. 

# Thoughts on Experience with R in Milestone3

We are more familiar to the logic of dash and we get more practice this time. While R works in a different way from Python, we need to take care of some syntax issues and overall we really enjoyed implementing with R. 

# Impacts Based on Feedbacks

According to Joel's and Afshin's feedback, we improved our table and charts in our python version and due to time limit we did not do more improvements. We will be working on giving audience a high level sense about the dataset as well as make our dashboard more complicated in the coming week.