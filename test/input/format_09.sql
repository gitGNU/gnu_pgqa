SELECT  *
FROM
        (select * from b where i=10) AS s
	join
	(select * from c where j=1) as s2
	on i=j;
