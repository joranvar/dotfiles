(ert-deftest sql-reformat-test/empty-string ()
  "Should return empty string when given empty string."
  (should (equal (sql-reformat-string "") "")))

(ert-deftest sql-reformat-test/minimal-select ()
  "Should return clean statement when given minimal select statement."
  (should (equal (sql-reformat-string "select 1") "SELECT 1;"))
  (should (equal (sql-reformat-string "select 2") "SELECT 2;"))
  (should (equal (sql-reformat-string "select 2;") "SELECT 2;")))

(ert-deftest sql-reformat-test/minimal-select-id ()
  "Should return clean statement when given minimal select statement with ids."
  (should (equal (sql-reformat-string "select this") "SELECT [this];"))
  (should (equal (sql-reformat-string "select [me]") "SELECT [me];"))
  (should (equal (sql-reformat-string "select [2nd_time];") "SELECT [2nd_time];"))
  (should (equal (sql-reformat-string "select 2, [2nd_time], 5;" 18)
                 "SELECT 2
                       , [2nd_time]
                       , 5;")))

(ert-deftest sql-reformat-test/minimal-select-alias-and-id ()
  "Should return clean statement when given minimal select statement with ids with an alias."
  (should (equal (sql-reformat-string "select b.this") "SELECT [this] = [b].[this];"))
  (should (equal (sql-reformat-string "select this = 2") "SELECT [this] = 2;"))
  (should (equal (sql-reformat-string "select 2 as this") "SELECT [this] = 2;")))

(ert-deftest sql-reformat-test/multi-expression-select ()
  "Should return clean statement when given multi-expression select statement."
  (should (equal (sql-reformat-string "select 1, 2" 18)
                 "SELECT 1
                       , 2;"))
  (should (equal (sql-reformat-string "select 1, 2, 3" 18)
                 "SELECT 1
                       , 2
                       , 3;")))

(ert-deftest sql-reformat-test/trailing-unparsed ()
  "Should return mark the spot between parsed/reformatted and unparsed text."
  (should (equal (sql-reformat-string "select 1, 2; bla" 18)
                 "SELECT 1
                       , 2;
                  -- PARSE ERROR\n bla")))

(ert-deftest sql-reformat-test/select-with-from ()
  "Should return clean statement when given select statement with from clause."
  (should (equal (sql-reformat-string "select 1 from quetzlquatl" 18)
                 "SELECT 1
                    FROM [quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1 from [quetzlquatl]" 18)
                 "SELECT 1
                    FROM [quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from quetzlquatl" 18)
                 "SELECT 1
                       , 2
                    FROM [quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 From Quetzlquatl;" 18)
                 "SELECT 1
                       , 2
                    FROM [Quetzlquatl];")))

(ert-deftest sql-reformat-test/select-with-subquery ()
  "Should return clean statement when given select statement with a subquery."
  (should (equal (sql-reformat-string "select (select 1 from quetzlquatl)" 18)
                 "SELECT ( SELECT 1
                             FROM [quetzlquatl]
                         );")))

(ert-deftest sql-reformat-test/select-with-subquery-and-aliases ()
  "Should return clean statement when given select statement with a subquery with aliases."
  (should (equal (sql-reformat-string "select (select 1 as one from quetzlquatl) as sub" 18)
                 "SELECT [sub] = ( SELECT [one] = 1
                                     FROM [quetzlquatl]
                                 );")))

(ert-deftest sql-reformat-test/select-with-from-specified-table ()
  "Should return clean statement when given select statement with from clause from a specified table."
  (should (equal (sql-reformat-string "select 1 from dbo.quetzlquatl" 18)
                 "SELECT 1
                    FROM [dbo].[quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from thisdb..quetzlquatl" 18)
                 "SELECT 1
                       , 2
                    FROM [thisdb]..[quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from thisdb.dbo.quetzlquatl" 18)
                 "SELECT 1
                       , 2
                    FROM [thisdb].[dbo].[quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from myserver.thisdb.dbo.quetzlquatl" 18)
                 "SELECT 1
                       , 2
                    FROM [myserver].[thisdb].[dbo].[quetzlquatl];")))

(ert-deftest sql-reformat-test/select-with-from-and-where ()
  "Should return clean statement when given select statement with from and where clause."
  (should (equal (sql-reformat-string "select 1 from quetzlquatl where x = 1" 18)
                 "SELECT 1
                    FROM [quetzlquatl]
                   WHERE [x] = 1;"))
  (should (equal (sql-reformat-string "select 1 from quetzlquatl where x = 1 or x = 2;" 18)
                 "SELECT 1
                    FROM [quetzlquatl]
                   WHERE [x] = 1
                      OR [x] = 2;"))
  (should (equal (sql-reformat-string "select 1 from quetzlquatl where 2 = 1 and y = 2 or x = 3;" 18)
                 "SELECT 1
                    FROM [quetzlquatl]
                   WHERE 2 = 1
                         AND [y] = 2
                      OR [x] = 3;")))

;; (rdp-parse-string "select a, amore, somuch from dbo.a join myDb..this join second on morebla on bla;" sql-tokens)
;; (sql-ast-to-string (rdp-parse-string "select a as b" sql-tokens))
