import random  # used in random receipt no function
import string
from functools import partial
from logging import root
import tkinter as tk
from firstname_lastname_final_project_CLASS import Cart, App

# define a MyFrame class
class MyFrame(tk.Frame):
    def __init__(self, root):
        tk.Frame.__init__(self, root)
        self.shop_by_apps_category = None
        self.shop_by_apps_price = None
        self.shop_by_apps_ratings = None
        self.init_container()
        self.cart = Cart()
        self.welcome()
        self.data = tk.StringVar(self, 'Subtotal: 0.0')

    def init_container(self):
        self.states = []

    def clear_frame(self):
        for widget in self.winfo_children():
            widget.destroy()

    @staticmethod
    def exit_application():
        root.destroy()

    def welcome(self):
        self.clear_frame()
        tk.Label(self, text='****Welcome to AppsCart!****', background="gray70").grid(row=0, column=0, columnspan=4)

        tk.Button(self, text="Select by category", command=self.shop_by_apps_category).grid(row=1, column=0)
        tk.Button(self, text="Select by ratings", command=self.shop_by_apps_ratings).grid(row=1, column=1)
        tk.Button(self, text="Select by Price", command=self.shop_by_apps_price).grid(row=1, column=2)
        tk.Button(self, text="Exit Application", command=self.exit_application).grid(row=1, column=3)

    def shop_by_category(self, category=None):
        self.clear_frame()
        self.init_container()

        if category is None:
            categories = list(App.category_dict.keys())
            Choose_Category_Label = tk.Label(self, text="Choose Category: ")
            Choose_Category_Label.grid(row=0, column=0)

            for category in categories:
                tk.Button(self, text=category, command=partial(self.shop_by_category, category)).grid(row=categories.index(category) + 1, column=0)

            tk.Button(self, text="Go Back", command=self.welcome).grid(row=len(categories) + 1, column=0)
        else:
            current_items = App.category_dict[category]

            row = 0
            for item in current_items:
                self.states.append(tk.IntVar())
                chk = tk.Checkbutton(self, text=item.name, variable=self.states[row], command=partial(self.add_to_cart, item))
                chk.grid(row=row, column=0)

                tk.Label(self, text="$" + str(item.price)).grid(row=row, column=1)
                tk.Label(self, text=str(item.rating)).grid(row=row, column=2)
                tk.Label(self, text=item.category).grid(row=row, column=3)

                row += 1

            tk.Label(self, textvariable=self.data).grid(row=row + 1, column=0, columnspan=4)
            tk.Button(self, text="Add to Cart", command=self.add_to_cart_multiple).grid(row=row + 2, column=0)
            tk.Button(self, text="Go Back", command=self.welcome).grid(row=row + 3, column=0)

    def shop_by_ratings(self, rating=None):
        self.clear_frame()
        self.init_container()

        if rating is None:
            ratings = [i / 2 for i in range(1, 10)]
            Choose_Rating_Label= tk.Label(self, text="Choose Rating: ")
            Choose_Rating_Label.grid(row=0, column=0)

            for rating in ratings:
                tk.Button(self, text=f"{rating} Stars", command=partial(self.shop_by_ratings, rating)).grid(row=ratings.index(rating) + 1, column=0)

            tk.Button(self, text="Go Back", command=self.welcome).grid(row=len(ratings) + 1, column=0)
        else:
            lower_bound = rating - 0.5
            upper_bound = rating + 0.5
            current_items = [item for item in App.all_apps if lower_bound <= item.rating < upper_bound]

            row = 0
            for item in current_items:
                self.states.append(tk.IntVar())
                chk = tk.Checkbutton(self, text=item.name, variable=self.states[row], command=partial(self.add_to_cart, item))
                chk.grid(row=row, column=0)

                tk.Label(self, text="$" + str(item.price)).grid(row=row, column=1)
                tk.Label(self, text=str(item.rating)).grid(row=row, column=2)
                tk.Label(self, text=item.category).grid(row=row, column=3)

                row += 1

            tk.Label(self, textvariable=self.data).grid(row=row + 1, column=0, columnspan=4)
            tk.Button(self, text="Add to Cart", command=self.add_to_cart_multiple).grid(row=row + 2, column=0)
            tk.Button(self, text="Go Back", command=self.welcome).grid(row=row + 3, column=0)

    def shop_by_price(self, price_range=None):
        self.clear_frame()
        self.init_container()

        if price_range is None:
            price_ranges = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, float("inf"))]
            Choose_Price_Label = tk.Label(self, text="Choose Pricing: ")
            Choose_Price_Label.grid(row=0, column=0)

            for price_range in price_ranges:
                tk.Button(self, text=f"${price_range[0]:.2f} - ${price_range[1]:.2f}", command=partial(self.shop_by_price, price_range)).grid(row=price_ranges.index(price_range) + 1, column=0)

            tk.Button(self, text="Go Back", command=self.welcome).grid(row=len(price_ranges) + 1, column=0)
        else:
            current_items = [item for item in App.all_apps if price_range[0] <= item.price < price_range[1]]

            row = 0
            for item in current_items:
                self.states.append(tk.IntVar())
                chk = tk.Checkbutton(self, text=item.name, variable=self.states[row], command=partial(self.add_to_cart, item))
                chk.grid(row=row, column=0)

                tk.Label(self, text="$" + str(item.price)).grid(row=row, column=1)
                tk.Label(self, text=str(item.rating)).grid(row=row, column=2)
                tk.Label(self, text=item.category).grid(row=row, column=3)

                row += 1

            tk.Label(self, textvariable=self.data).grid(row=row + 1, column=0, columnspan=4)
            tk.Button(self, text="Add to Cart", command=self.add_to_cart_multiple).grid(row=row + 2, column=0)
            tk.Button(self, text="Go Back", command=self.welcome).grid(row=row + 3, column=0)

    def add_to_cart(self, item):
        self.cart.add_item(item)
        self.data.set(f"Subtotal: ${self.cart.subtotal():.2f}")

    def add_to_cart_multiple(self):
        for i in range(len(self.states)):
            if self.states[i].get() == 1:
                self.cart.add_item(App.all_apps[i])

        self.data.set(f"Subtotal: ${self.cart.subtotal():.2f}")

    def generate_receipt(self):
        self.clear_frame()

        e_order_label = tk.Label(self, text="Your e-order:")
        e_order_label.grid(row=0, column=0, columnspan=4)

        receipt_number = self.get_receipt_number()
        receipt_label = tk.Label(self, text=f"e-Order Number: {receipt_number}")
        receipt_label.grid(row=1, column=0, columnspan=4)

        header_label = tk.Label(self, text="Name\tPrice\tRating\tCategory")
        header_label.grid(row=2, column=0, columnspan=4)

        for item in self.cart.items:
            name_label = tk.Label(self, text=item.name)
            name_label.grid(row=3 + self.cart.items.index(item), column=0)

            price_label = tk.Label(self, text="$" + str(item.price))
            price_label.grid(row=3 + self.cart.items.index(item), column=1)

            rating_label = tk.Label(self, text=str(item.rating))
            rating_label.grid(row=3 + self.cart.items.index(item), column=2)

            category_label = tk.Label(self, text=item.category)
            category_label.grid(row=3 + self.cart.items.index(item), column=3)

        subtotal_label = tk.Label(self, text=f"Subtotal: ${self.cart.subtotal():.2f}")
        subtotal_label.grid(row=len(self.cart.items) + 4, column=0, columnspan=2)

        tax_label = tk.Label(self, text="Tax: 4.3%")
        tax_label.grid(row=len(self.cart.items) + 4, column=2)


        # Total: Label - subtotal + tax
        total_label = tk.Label(self, text=f"Total: ${self.cart.subtotal() * 1.043:.2f}")
        total_label.grid(row=len(self.cart.items)+ 5, column=0, columnspan=4)

        # ‘Thank you’ message: Label
        thank_you_label = tk.Label(self, text="Thank you for your purchase!")
        thank_you_label.grid(row=len(self.cart.items) + 6, column=0, columnspan=4)

        # Exit application: Button – exit the program-command = exit_application
        exit_button = tk.Button(self, text="Exit Application", command=self.exit_application)
        exit_button.grid(row=len(self.cart.items) + 7, column=0, columnspan=4)

    @staticmethod
    def get_receipt_number():
        """Generate random receipt number"""
        return ''.join(random.choices(string.ascii_letters.upper() + string.digits, k=4))
    # main driver code


root = tk.Tk()
root.title("Apps Cart")  # set window title
my_frame = MyFrame(root)  # create a MyFrame object
my_frame.pack()
root.mainloop()