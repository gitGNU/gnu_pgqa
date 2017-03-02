SELECT  *
FROM
        a as a, b, ((select * from b) AS s join c on i = k) as j;
