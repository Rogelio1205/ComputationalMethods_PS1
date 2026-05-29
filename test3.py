# ============================================================
# Inventory Management System
# Demonstrates: keywords, identifiers, integers, floats,
# strings, comments, operators, delimiters, whitespace
# ============================================================

import json
import os
from datetime import datetime


# --- Constants ---

MAX_STOCK    = 1000
LOW_STOCK    = 10
TAX_RATE     = 0.16
DISCOUNT_VIP = 0.20
STORE_NAME   = "PyMart"
VERSION      = "1.0.0"


# --- Data model ---

class Product:
    """Represents a product in the inventory."""

    def __init__(self, product_id, name, price, stock, category):
        self.product_id = product_id
        self.name       = name
        self.price      = float(price)
        self.stock      = int(stock)
        self.category   = category
        self.sold       = 0

    def is_available(self):
        return self.stock > 0

    def is_low_stock(self):
        return self.stock <= LOW_STOCK

    def apply_discount(self, percent):
        # Reduce price by a given percentage
        if percent < 0 or percent > 1:
            raise ValueError("Discount must be between 0.0 and 1.0")
        self.price = round(self.price * (1 - percent), 2)

    def restock(self, amount):
        if amount <= 0:
            raise ValueError("Restock amount must be positive")
        if self.stock + amount > MAX_STOCK:
            raise OverflowError(f"Cannot exceed max stock of {MAX_STOCK}")
        self.stock += amount

    def sell(self, quantity):
        if quantity <= 0:
            raise ValueError("Quantity must be positive")
        if quantity > self.stock:
            raise ValueError(f"Only {self.stock} units available")
        self.stock -= quantity
        self.sold  += quantity
        return self.price * quantity

    def total_value(self):
        return round(self.price * self.stock, 2)

    def __repr__(self):
        return (
            f"Product(id={self.product_id!r}, name={self.name!r}, "
            f"price={self.price}, stock={self.stock})"
        )


class Inventory:
    """Manages a collection of products."""

    def __init__(self, store_name):
        self.store_name = store_name
        self.products   = {}
        self.log        = []

    def add_product(self, product):
        if product.product_id in self.products:
            raise KeyError(f"Product '{product.product_id}' already exists")
        self.products[product.product_id] = product
        self._record(f"Added product: {product.name}")

    def remove_product(self, product_id):
        if product_id not in self.products:
            raise KeyError(f"Product '{product_id}' not found")
        removed = self.products.pop(product_id)
        self._record(f"Removed product: {removed.name}")
        return removed

    def get(self, product_id):
        return self.products.get(product_id)

    def search(self, keyword):
        # Case-insensitive search by name or category
        kw = keyword.lower()
        return [
            p for p in self.products.values()
            if kw in p.name.lower() or kw in p.category.lower()
        ]

    def low_stock_alerts(self):
        return [p for p in self.products.values() if p.is_low_stock()]

    def total_inventory_value(self):
        return round(sum(p.total_value() for p in self.products.values()), 2)

    def category_summary(self):
        summary = {}
        for p in self.products.values():
            if p.category not in summary:
                summary[p.category] = {"count": 0, "value": 0.0}
            summary[p.category]["count"] += 1
            summary[p.category]["value"] += p.total_value()
        return summary

    def _record(self, message):
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        entry     = f"[{timestamp}] {message}"
        self.log.append(entry)

    def export_json(self, path):
        data = {
            "store":    self.store_name,
            "exported": datetime.now().isoformat(),
            "products": [
                {
                    "id":       p.product_id,
                    "name":     p.name,
                    "price":    p.price,
                    "stock":    p.stock,
                    "category": p.category,
                    "sold":     p.sold,
                }
                for p in self.products.values()
            ]
        }
        with open(path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
        self._record(f"Exported inventory to {path}")


# --- Checkout ---

def calculate_total(cart, is_vip=False):
    """
    Given a list of (product, quantity) tuples,
    returns subtotal, tax, discount, and grand total.
    """
    subtotal = 0.0
    for product, qty in cart:
        subtotal += product.price * qty

    discount = subtotal * DISCOUNT_VIP if is_vip else 0.0
    taxable  = subtotal - discount
    tax      = round(taxable * TAX_RATE, 2)
    total    = round(taxable + tax, 2)

    return {
        "subtotal": round(subtotal, 2),
        "discount": round(discount, 2),
        "tax":      tax,
        "total":    total,
    }


def process_sale(inventory, cart, is_vip=False):
    """Process a sale: deduct stock and return receipt."""
    receipt_lines = []
    actual_cart   = []

    for product_id, qty in cart:
        product = inventory.get(product_id)
        if product is None:
            print(f"  Warning: product '{product_id}' not found, skipping")
            continue
        if not product.is_available():
            print(f"  Warning: '{product.name}' is out of stock, skipping")
            continue
        amount = product.sell(qty)
        actual_cart.append((product, qty))
        receipt_lines.append(
            f"  {product.name:<25} x{qty:>3}  ${amount:>8.2f}"
        )
        inventory._record(f"Sold {qty}x {product.name}")

    totals = calculate_total(actual_cart, is_vip=is_vip)

    receipt = [
        "=" * 45,
        f"  {STORE_NAME} — Receipt".center(45),
        "=" * 45,
    ] + receipt_lines + [
        "-" * 45,
        f"  {'Subtotal':<30} ${totals['subtotal']:>8.2f}",
        f"  {'VIP Discount':<30} -${totals['discount']:>7.2f}",
        f"  {'Tax (16%)':<30} ${totals['tax']:>8.2f}",
        f"  {'TOTAL':<30} ${totals['total']:>8.2f}",
        "=" * 45,
    ]
    return "\n".join(receipt)


# --- Reports ---

def print_inventory_report(inventory):
    print(f"\n{'=' * 50}")
    print(f"  {inventory.store_name} — Inventory Report")
    print(f"{'=' * 50}")

    for p in sorted(inventory.products.values(), key=lambda x: x.category):
        flag = " ⚠ LOW" if p.is_low_stock() else ""
        print(
            f"  [{p.product_id}] {p.name:<22} "
            f"${p.price:>7.2f}  stock: {p.stock:>4}{flag}"
        )

    alerts = inventory.low_stock_alerts()
    if alerts:
        print(f"\n  Low stock alerts ({len(alerts)} items):")
        for p in alerts:
            print(f"    - {p.name} ({p.stock} left)")

    print(f"\n  Total inventory value: ${inventory.total_inventory_value():,.2f}")

    print("\n  Category breakdown:")
    for cat, data in inventory.category_summary().items():
        print(f"    {cat:<15} {data['count']} products  ${data['value']:,.2f}")

    print(f"{'=' * 50}\n")


# --- Main ---

def main():
    inv = Inventory(STORE_NAME)

    # Add products: (id, name, price, stock, category)
    products = [
        Product("A001", "Wireless Mouse",      29.99,  45, "Electronics"),
        Product("A002", "USB-C Hub",            49.95,   8, "Electronics"),
        Product("A003", "Mechanical Keyboard", 119.00,  22, "Electronics"),
        Product("B001", "Desk Lamp",            24.50,  60, "Office"),
        Product("B002", "Notebook (A4)",         4.75, 200, "Office"),
        Product("B003", "Ballpoint Pens x10",    3.99,   7, "Office"),
        Product("C001", "Coffee Mug",            9.99,  35, "Kitchen"),
        Product("C002", "Stainless Bottle",     18.50,  12, "Kitchen"),
    ]

    for p in products:
        inv.add_product(p)

    # Apply a discount to electronics
    for p in inv.search("electronics"):
        p.apply_discount(0.10)

    print_inventory_report(inv)

    # Simulate a VIP sale
    cart = [
        ("A001", 2),
        ("B002", 5),
        ("C001", 1),
        ("A002", 3),
    ]
    print(process_sale(inv, cart, is_vip=True))

    # Restock a low-stock item
    pen_pack = inv.get("B003")
    if pen_pack and pen_pack.is_low_stock():
        pen_pack.restock(50)
        print(f"\n  Restocked '{pen_pack.name}' — new stock: {pen_pack.stock}")

    # Export
    inv.export_json("inventory_export.json")
    print(f"\n  Inventory exported. Log entries: {len(inv.log)}\n")


if __name__ == "__main__":
    main()