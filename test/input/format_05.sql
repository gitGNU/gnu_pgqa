SELECT p.category_id, SUM(sum_nitems), SUM(sum_price) FROM (SELECT product_id,SUM(nitems) sum_nitems,SUM(price) sum_price
FROM sales GROUP BY product_id) p JOIN (SELECT product_id,SUM(nitems) sum_nitems,SUM(price) sum_price
FROM sales GROUP BY product_id) s ON p.product_id = s.product_id GROUP BY p.category_id;
