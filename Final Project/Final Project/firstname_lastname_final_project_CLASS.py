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
        self._price = price
        self._rating = rating
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
        price = row[4]
        if price == 'Free':
            price_key = "$0.00"
        else:
            price_key = f"${float(price):.2f}"
        if price_key in App.price_dict:
            App.price_dict[price_key].append(self)
        else:
            App.price_dict[price_key] = [self]

    @property
    def price(self):
        return self._price

    @price.setter
    def price(self, value):
        self._price = value

        # Update price_dict
        price_key = f"${value:.2f}"
        if price_key in App.price_dict:
            App.price_dict[price_key].remove(self)
            App.price_dict[price_key].append(self)
        else:
            App.price_dict[price_key] = [self]

    @property
    def rating(self):
        return self._rating

    @rating.setter
    def rating(self, value):
        self._rating = value

        # Update ratings_dict
        rating_key = round(float(value), 1)
        if rating_key in App.ratings_dict:
            App.ratings_dict[rating_key].remove(self)
            App.ratings_dict[rating_key].append(self)
        else:
            App.ratings_dict[rating_key] = [self]

    def get_categories(self):
        return {category: self for category in self.category.split("|")}

# Initialize dictionaries
App.category_dict = defaultdict(list)
App.ratings_dict = defaultdict(list)
App.price_dict = defaultdict(list)

# Process CSV file
with open("Top50ShopifyApps.csv") as csvfile:
    csv_reader = csv.reader(csvfile, delimiter=",")
    next(csv_reader)  # Skip header
    for row in csv_reader:
        id, name, developer, description, price, rating, review_count, category = row
        App(id, name, developer, description, price, rating, review_count, category)


class Cart:
    def __init__(self):
        self.items = []

    def subtotal(self):
        return sum(item.price for item in self.items)

    def add_item(self, app):
        if app not in self.items:
            self.items.append(app)

    def remove_item(self, app):
       if app in self.items:
            self.items.remove(app)

    def display_items(self):
        for item in self.items:
            print(f"{item.name} ({item.developer}) - ${item.price} - Rating: {item.rating}")


# Testing code to check object creating Items
# Uncomment to test and then
# Comment out when done

# cart = Cart()
# for app in App.category_dict["Marketing"]:
#     cart.add_item(app)
# cart.display_items()
# print(f"Subtotal: ${cart.subtotal()}")

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