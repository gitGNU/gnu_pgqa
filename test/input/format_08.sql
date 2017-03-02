SELECT  *
FROM
        x, (a join c on i=k) j1 join (select * from b where j=10) AS s on i=j;
