SELECT id FROM tasks_new AS t,
pg_catalog.pg_stat_user_tables AS s
WHERE (t.tabschema, t.tabname) = (s.schemaname,
s.relname) AND (s.n_dead_tup / (s.n_live_tup
+ s.n_dead_tup)) > t.max_dead_frac AND ((s.last_analyze
>= now() - t.stats_max_age) OR (s.last_autoanalyze
>= now() - t.stats_max_age))

SELECT  id
FROM    tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE   (t.tabschema, t.tabname) = (s.schemaname, s.relname) AND (s.n_dead_tup
        / (s.n_live_tup + s.n_dead_tup)) > t.max_dead_frac AND ((s.last_analyze
        >= now() - t.stats_max_age) OR (s.last_autoanalyze >= now() -
        t.stats_max_age))

SELECT  id
FROM    tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE   (t.tabschema, t.tabname) = (s.schemaname, s.relname) AND (s.n_dead_tup
        / (s.n_live_tup + s.n_dead_tup)) > t.max_dead_frac AND ((s.last_analyze
        >= now() - t.stats_max_age) OR (s.last_autoanalyze >= now() -
        t.stats_max_age))

SELECT  id
FROM    tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE   (t.tabschema, t.tabname) = (s.schemaname, s.relname) AND (s.n_dead_tup
        / (s.n_live_tup + s.n_dead_tup)) > t.max_dead_frac AND ((s.last_analyze
        >= now() - t.stats_max_age) OR (s.last_autoanalyze >= now() -
        t.stats_max_age))

SELECT  id
FROM    tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE
                                (t.tabschema, t.tabname)
                        =
                                (s.schemaname, s.relname)
                AND
                                (
                                                s.n_dead_tup
                                        /
                                                (
                                                                s.n_live_tup
                                                        +
                                                                s.n_dead_tup
                                                )
                                )
                        >
                                t.max_dead_frac
        AND
                (
                                (
                                                s.last_analyze
                                        >=
                                                        now()
                                                -
                                                        t.stats_max_age
                                )
                        OR
                                (
                                                s.last_autoanalyze
                                        >=
                                                        now()
                                                -
                                                        t.stats_max_age
                                )
                )

SELECT
        id
FROM
        tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE
        (t.tabschema, t.tabname) = (s.schemaname, s.relname) AND (s.n_dead_tup
        / (s.n_live_tup + s.n_dead_tup)) > t.max_dead_frac AND ((s.last_analyze
        >= now() - t.stats_max_age) OR (s.last_autoanalyze >= now() -
        t.stats_max_age))

SELECT
        id
FROM
        tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE
                                (t.tabschema, t.tabname)
                        =
                                (s.schemaname, s.relname)
                AND
                                (
                                                s.n_dead_tup
                                        /
                                                (
                                                                s.n_live_tup
                                                        +
                                                                s.n_dead_tup
                                                )
                                )
                        >
                                t.max_dead_frac
        AND
                (
                                (
                                                s.last_analyze
                                        >=
                                                        now()
                                                -
                                                        t.stats_max_age
                                )
                        OR
                                (
                                                s.last_autoanalyze
                                        >=
                                                        now()
                                                -
                                                        t.stats_max_age
                                )
                )

SELECT
    id
FROM
    tasks_new AS t, pg_catalog.pg_stat_user_tables AS s
WHERE
                (t.tabschema, t.tabname)
            =
                (s.schemaname, s.relname)
        AND
                (
                        s.n_dead_tup
                    /
                        (
                                s.n_live_tup
                            +
                                s.n_dead_tup
                        )
                )
            >
                t.max_dead_frac
    AND
        (
                (
                        s.last_analyze
                    >=
                            now()
                        -
                            t.stats_max_age
                )
            OR
                (
                        s.last_autoanalyze
                    >=
                            now()
                        -
                            t.stats_max_age
                )
        )