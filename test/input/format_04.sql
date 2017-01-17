SELECT product_id,SUM(nitems) sum_nitems,SUM(price) sum_price
FROM sales
GROUP BY product_id, 3
ORDER BY product_id, 3;
