SELECT p.category_id, SUM(sum_nitems),
SUM(sum_price) FROM (SELECT product_id,
SUM(nitems) AS sum_nitems, SUM(price) AS
sum_price FROM sales GROUP BY product_id)
AS p JOIN (SELECT product_id, SUM(nitems)
AS sum_nitems, SUM(price) AS sum_price
FROM sales GROUP BY product_id) AS s ON
p.product_id = s.product_id GROUP BY
p.category_id

SELECT          p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM            (SELECT         product_id, SUM(nitems) AS sum_nitems,
                                SUM(price) AS sum_price
                FROM            sales
                GROUP BY        product_id) AS p JOIN
                (SELECT         product_id, SUM(nitems) AS sum_nitems,
                                SUM(price) AS sum_price
                FROM            sales
                GROUP BY        product_id) AS s ON p.product_id =
                s.product_id
GROUP BY        p.category_id

SELECT          p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM            (SELECT         product_id, SUM(nitems) AS sum_nitems,
                                SUM(price) AS sum_price
                FROM            sales
                GROUP BY        product_id) AS p
                JOIN
                (SELECT         product_id, SUM(nitems) AS sum_nitems,
                                SUM(price) AS sum_price
                FROM            sales
                GROUP BY        product_id) AS s ON p.product_id =
                        s.product_id
GROUP BY        p.category_id

SELECT          p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM            (SELECT         product_id, SUM(nitems) AS sum_nitems,
                                SUM(price) AS sum_price
                FROM            sales
                GROUP BY        product_id) AS p
                JOIN
                (SELECT         product_id, SUM(nitems) AS sum_nitems,
                                SUM(price) AS sum_price
                FROM            sales
                GROUP BY        product_id) AS s ON p.product_id =
                        s.product_id
GROUP BY        p.category_id

SELECT          p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM            (SELECT         product_id, SUM(nitems) AS sum_nitems, SUM(price)
                                AS sum_price
                FROM            sales
                GROUP BY        product_id) AS p
                JOIN
                (SELECT         product_id, SUM(nitems) AS sum_nitems, SUM(price)
                                AS sum_price
                FROM            sales
                GROUP BY        product_id) AS s ON
                                p.product_id
                        =
                                s.product_id
GROUP BY        p.category_id

SELECT
        p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM
        (SELECT
                product_id, SUM(nitems) AS sum_nitems, SUM(price) AS
                sum_price
        FROM
                sales
        GROUP BY
                product_id) AS p
        JOIN
        (SELECT
                product_id, SUM(nitems) AS sum_nitems, SUM(price) AS
                sum_price
        FROM
                sales
        GROUP BY
                product_id) AS s ON p.product_id = s.product_id
GROUP BY
        p.category_id

SELECT
        p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM
        (SELECT
                product_id, SUM(nitems) AS sum_nitems, SUM(price) AS sum_price
        FROM
                sales
        GROUP BY
                product_id) AS p
        JOIN
        (SELECT
                product_id, SUM(nitems) AS sum_nitems, SUM(price) AS sum_price
        FROM
                sales
        GROUP BY
                product_id) AS s ON
                        p.product_id
                =
                        s.product_id
GROUP BY
        p.category_id

SELECT
    p.category_id, SUM(sum_nitems), SUM(sum_price)
FROM
    (SELECT
        product_id, SUM(nitems) AS sum_nitems, SUM(price) AS sum_price
    FROM
        sales
    GROUP BY
        product_id) AS p
    JOIN
    (SELECT
        product_id, SUM(nitems) AS sum_nitems, SUM(price) AS sum_price
    FROM
        sales
    GROUP BY
        product_id) AS s ON
            p.product_id
        =
            s.product_id
GROUP BY
    p.category_id

SELECT
                p.category_id,
                SUM(sum_nitems),
                SUM(sum_price)
FROM            (SELECT
                                product_id,
                                SUM(nitems)
                                AS
                                sum_nitems,
                                SUM(price)
                                AS
                                sum_price
                FROM
                                sales
                GROUP BY
                                product_id)
                AS p
                JOIN
                (SELECT
                                product_id,
                                SUM(nitems)
                                AS
                                sum_nitems,
                                SUM(price)
                                AS
                                sum_price
                FROM
                                sales
                GROUP BY
                                product_id)
                AS s
                ON
                        p.product_id
                        =
                        s.product_id
GROUP BY
                p.category_id

SELECT
        p.category_id,
        SUM(sum_nitems),
        SUM(sum_price)
FROM
        (SELECT
                product_id,
                SUM(nitems) AS sum_nitems,
                SUM(price) AS sum_price
        FROM
                sales
        GROUP BY
                product_id) AS p
        JOIN
        (SELECT
                product_id,
                SUM(nitems) AS sum_nitems,
                SUM(price) AS sum_price
        FROM
                sales
        GROUP BY
                product_id) AS s ON p.product_id = s.product_id
GROUP BY
        p.category_id

SELECT
        p.category_id,
        SUM(sum_nitems),
        SUM(sum_price)
FROM
        (SELECT
                product_id,
                SUM(nitems) AS sum_nitems,
                SUM(price) AS sum_price
        FROM
                sales
        GROUP BY
                product_id) AS p
        JOIN
        (SELECT
                product_id,
                SUM(nitems) AS sum_nitems,
                SUM(price) AS sum_price
        FROM
                sales
        GROUP BY
                product_id) AS s ON
                        p.product_id
                =
                        s.product_id
GROUP BY
        p.category_id

                SELECT p.category_id, SUM(sum_nitems), SUM(sum_price) FROM (SELECT
                product_id, SUM(nitems) AS sum_nitems, SUM(price) AS sum_price
                FROM sales GROUP BY product_id) AS p JOIN (SELECT product_id,
                SUM(nitems) AS sum_nitems, SUM(price) AS sum_price FROM sales
                GROUP BY product_id) AS s ON p.product_id = s.product_id
                GROUP BY p.category_id

                SELECT          p.category_id, SUM(sum_nitems), SUM(sum_price)
                FROM            (SELECT         product_id, SUM(nitems) AS
                                                sum_nitems, SUM(price) AS
                                                sum_price
                                FROM            sales
                                GROUP BY        product_id) AS p JOIN
                                (SELECT         product_id, SUM(nitems) AS
                                                sum_nitems, SUM(price) AS
                                                sum_price
                                FROM            sales
                                GROUP BY        product_id) AS s ON
                                p.product_id = s.product_id
                GROUP BY        p.category_id

                SELECT
                        p.category_id, SUM(sum_nitems), SUM(sum_price)
                FROM
                        (SELECT
                                product_id, SUM(nitems) AS sum_nitems, SUM(price)
                                AS sum_price
                        FROM
                                sales
                        GROUP BY
                                product_id) AS p
                        JOIN
                        (SELECT
                                product_id, SUM(nitems) AS sum_nitems, SUM(price)
                                AS sum_price
                        FROM
                                sales
                        GROUP BY
                                product_id) AS s ON
                                        p.product_id
                                =
                                        s.product_id
                GROUP BY
                        p.category_id