-- This is primarily to test unary postfix operators at different positions of
-- the tree when pgqa-multiline-operator is set.
SELECT *
FROM	pg_catalog.pg_stat_user_tables s,
	squeeze.tables t
WHERE
	i.class_id NOTNULL AND
	i.table_id = t.id AND
	(t.tabschema, t.tabname) = (s.schemaname, s.relname) AND
	(
		(s.last_vacuum >= now() - t.vacuum_max_age)
		OR
		(s.last_autovacuum >= now() - t.vacuum_max_age)
	)
	AND
	(
		i.last_task_finished ISNULL
		OR
		i.last_task_finished < s.last_vacuum
		OR
		i.last_task_finished < s.last_autovacuum
	);
