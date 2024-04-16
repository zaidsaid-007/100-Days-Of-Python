import csv
from collections import defaultdict

class App:
    # Initialize the class and dictionaries
    def __init__(self, id, name, developer, description, price, rating, reviews_count ,category):
        self.id = id
        self.name = name
        self.developer = developer
        self.description = description
        self.price = price
        self.rating = rating
        self.reviews_count = reviews_count
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
App.category_dict = defaultdict(list)
App.ratings_dict = defaultdict(list)
App.price_dict = defaultdict(list)

# Process CSV file
with open("Top50ShopifyApps.csv") as csvfile:
    csv_reader = csv.reader(csvfile, delimiter=",")
    next(csv_reader)  # Skip header
    for row in csv_reader:
        id, name, developer, description, price, rating, reviews_count, category = row
        if price == "Free":
            price = 0.0
        else:
            price = float(price)
        App(id, name, developer, description, price, rating, int(reviews_count), category)
class Cart(list):
    def subtotal(self):
        return sum(app.price for app in self)

# Testing code to check object creating Items
# Uncomment to test and then
# Comment out when done
'''
for k, v in App.category_dict.items():  # v is a list of all objects
    print(k, [(obj.name, obj.rating) for obj in v])
print('++++++++++')

for k, v in App.rating_dict.items():  # v is a list of all objects
    print(k, [obj.rating for obj in v])
print('++++++++++')

for k, v in App.price_dict.items():  # v is a list of all objects
    print(k, [obj.price for obj in v])
print('++++++++++')
'''