SELECT i, i, j + k, l, m FROM x, a LEFT
JOIN b ON true, y JOIN c ON j <> x WHERE
x = y OR j = (k - 1) * - (2 * x) / .5

SELECT  i, i, j + k, l, m
FROM    x, a LEFT JOIN b ON true, y JOIN c ON j <> x
WHERE   x = y OR j = (k - 1) * - (2 * x) / .5

SELECT  i, i, j + k, l, m
FROM    x, a
        LEFT JOIN b ON true, y
        JOIN c ON j <> x
WHERE   x = y OR j = (k - 1) * - (2 * x) / .5

SELECT  i, i, j + k, l, m
FROM    x,
        a
        LEFT JOIN b ON true,
        y
        JOIN c ON j <> x
WHERE   x = y OR j = (k - 1) * - (2 * x) / .5

SELECT  i, i,
                j
        +
                k,
        l, m
FROM    x,
        a
        LEFT JOIN b ON true,
        y
        JOIN c ON
                        j
                <>
                        x
WHERE
                        x
                =
                        y
        OR
                        j
                =
                                        (
                                                        k
                                                -
                                                        1
                                        )
                                *
                                        -
                                                (
                                                                2
                                                        *
                                                                x
                                                )
                        /
                                .5