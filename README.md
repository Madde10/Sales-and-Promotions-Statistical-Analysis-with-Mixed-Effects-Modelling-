# Sales-and-Promotions-Statistical-Analysis-with-Mixed-Effects-Modelling-
 Examining the effects of pricing and promotion strategies on total spend for that product, number of households who purchased that product, and the number of store visits.

This file "SnackChain.xlsx" contains real sales and promotions data from a large retail chain (of 79 stores) on 58 products belong to four product categories (bagged snacks, cold cereal, frozen pizza, and oral hygiene products) from multiple manufacturers like Frito Lay, Kellogg, and General Mills, over a period of 156 weeks. The data comes from three database tables: stores, products, and transactions, as shown in different tabs in the spreadsheet; the different tables and fields are shown in the Glossary tab. It has over 500,000 transactions.

We will first join data from the different tables (using foreign keys) to run the analytics. We are not interested in oral hygiene products; so we can drop this product category and all associated transactions.

The transaction table has weekly information on price and promotions of products (e.g., whether a product was assigned a special store display, or was in an in-store circular, or had a temporary price reduction). We want to examine the effects of these pricing and promotion strategies on total spend for that product, number of households who purchased that product, and the number of store visits. 

At the end of your analysis, we are interested in answers to the following questions:

What are the effects of product display, being featured on in-store circular, and temporary price reduction on product sales (spend), unit sales, and number of household purchasers?
How do the effects of display, feature, and TPR on SPEND vary by product categories (cold cereals, frozen pizza, bag snacks) and store segments (mainstream, upscale, value)?
What are the five most price elastic and five least price elastic products? Price elasticity is the change in units sold for change in product price.
As the retailer, which products would you lower the price to maximize (a) product sales and (b) unit sales, and why? 

Refer to the documentation for full analysis and answers.
