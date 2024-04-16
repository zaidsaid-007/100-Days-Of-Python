# -------------------------------------------------------------------------------
# Final Project
# Student Name: 
# Submission Date: 99/99/9999
# -------------------------------------------------------------------------------
# Honor Code Statement: I received no assistance on this assignment that
# violates the ethical guidelines as set forth by the
# instructor and the class syllabus.
# -------------------------------------------------------------------------------
# References: 
# -------------------------------------------------------------------------------
# Notes to grader: 
# -------------------------------------------------------------------------------
# Your source code below
# -------------------------------------------------------------------------------
import csv
from collections import defaultdict
import csv
import random

class App:
    def __init__(self, id, name, developer, description, price, rating, review_count, category):
        self.id = id
        self.name = name
        self.developer = developer
        self.description = description
        self.price = price
        self.rating = rating
        self.review_count = review_count
        self.category = category

        # Save apps to category dictionary
        if category in App.category_dict:
            App.category_dict[category].append(self)
        else:
            App.category_dict[category] = [self]

        # Save apps to ratings dictionary
        rating_key = round(float(rating), 1)
        if rating_key in App.ratings_dict:
            App.ratings_dict[rating_key].append(self)
        else:
            App.ratings_dict[rating_key] = [self]

        # Save apps to price dictionary
        price_key = f"${price:.2f}"
        if price_key in App.price_dict:
            App.price_dict[price_key].append(self)
        else:
            App.price_dict[price_key] = [self]

# Initialize dictionaries
App.category_dict = {}
App.ratings_dict = {}
App.price_dict = {}

# Process CSV file
with open("Top50ShopifyApps.csv") as csvfile:
    csv_reader = csv.reader(csvfile, delimiter=",")
    next(csv_reader)  # Skip header
    for row in csv_reader:
        id, name, developer, description, price, rating, review_count, category = row
        App(id, name, developer, description, price, rating, review_count, category)


# process file
filename = 'Top50ShopifyApps.csv'
with open(filename) as fin:
    reader = csv.reader(fin)
    next(reader)  # skip header
    for row in reader:
        ID, name, developer, description, price, rating, review_count, category = row
        App(ID, name, developer, description, price, rating, review_count, category)


# define a Cart class
class Cart:
    def __init__(self):
        self.items = []

    def subtotal(self):
        return sum(item.get_price() for item in self.items)

    def add_item(self, app):
        if app not in self.items:
            self.items.append(app)

    def append(self, app1):
        pass



'''Testing code to check object creating Items
Uncomment to test and then
Comment out when done'''
'''
for k, v in App.category_dict.items():  # v is a list of all objects
    print(k, [(obj.get_name(), obj.get_rating()) for obj in v])
print('++++++++++')

for k, v in App.rating_dict.items():  # v is a list of all objects
    print(k, [obj.get_rating() for obj in v])
print('++++++++++')

for k, v in App.price_dict.items():  # v is a list of all objects
    print(k, [obj.get_price() for obj in v])
print('++++++++++')
'''
# Testing code
# for key, value in App.category_dict.items():
#     print(f"Category: {key}")
#     for app in value:
#         print(f"{app.name}, {app.developer}, {app.description}, {app.price}, {app.rating}, {app.review_count}, {app.category}")
#
# for key, value in App.ratings_dict.items():
#     print(f"Rating: {key}")
#     for app in value:
#         print(f"{app.name}, {app.developer}, {app.description}, {app.price}, {app.rating}, {app.review_count}, {app.category}")
#
# for key, value in App.price_dict.items():
#     print(f"Price: {key}")
#     for app in value:
#         print(f"{app.name}, {app.developer}, {app.description}, {app.price}, {app.rating}, {app.review_count}, {app.category}")