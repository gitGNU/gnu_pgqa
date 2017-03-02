SELECT * FROM a AS a, b, ((SELECT * FROM
b) AS s JOIN c ON i = k) AS j

SELECT  *
FROM    a AS a, b,
        ((SELECT *
        FROM    b) AS s JOIN c ON i = k) AS j

SELECT  *
FROM    a AS a, b,
        ((SELECT *
        FROM    b) AS s
        JOIN c ON i = k) AS j

SELECT  *
FROM    a AS a, b,
        ((SELECT *
        FROM    b) AS s
        JOIN
        c ON i = k) AS j

SELECT  *
FROM    a AS a, b,
        ((SELECT *
        FROM    b) AS s
        JOIN
        c ON
                        i
                =
                        k) AS j

SELECT
        *
FROM
        a AS a, b,
        ((SELECT
                *
        FROM
                b) AS s
        JOIN
        c ON i = k) AS j

SELECT
        *
FROM
        a AS a, b,
        ((SELECT
                *
        FROM
                b) AS s
        JOIN
        c ON
                        i
                =
                        k) AS j

SELECT
    *
FROM
    a AS a, b,
    ((SELECT
        *
    FROM
        b) AS s
    JOIN
    c ON
            i
        =
            k) AS j

SELECT  *
FROM    a AS a, b,
        ((SELECT *
        FROM    b)
        AS s
        JOIN
        c ON i = k)
        AS j

SELECT
        *
FROM
        a AS a,
        b,
        ((SELECT
                *
        FROM
                b) AS s
        JOIN
        c ON i = k) AS j

SELECT
        *
FROM
        a AS a,
        b,
        ((SELECT
                *
        FROM
                b) AS s
        JOIN
        c ON
                        i
                =
                        k) AS j

                SELECT * FROM a AS a, b, ((SELECT * FROM b) AS s JOIN c ON i =
                k) AS j

                SELECT  *
                FROM    a AS a, b,
                        ((SELECT *
                        FROM    b) AS s JOIN c ON i = k) AS j

                SELECT
                        *
                FROM
                        a AS a, b,
                        ((SELECT
                                *
                        FROM
                                b) AS s
                        JOIN c ON
                                        i
                                =
                                        k) AS j