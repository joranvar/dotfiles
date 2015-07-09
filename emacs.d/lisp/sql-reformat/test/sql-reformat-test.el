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
  (should (equal (sql-reformat-string "select 2, [2nd_time], 5;") "SELECT 2\n     , [2nd_time]\n     , 5;")))

(ert-deftest sql-reformat-test/minimal-select-alias-and-id ()
  "Should return clean statement when given minimal select statement with ids with an alias."
  (should (equal (sql-reformat-string "select b.this") "SELECT [this] = [b].[this];"))
  (should (equal (sql-reformat-string "select this = 2") "SELECT [this] = 2;"))
  (should (equal (sql-reformat-string "select 2 as this") "SELECT [this] = 2;")))

(ert-deftest sql-reformat-test/multi-expression-select ()
  "Should return clean statement when given multi-expression select statement."
  (should (equal (sql-reformat-string "select 1, 2") "SELECT 1\n     , 2;"))
  (should (equal (sql-reformat-string "select 1, 2, 3") "SELECT 1\n     , 2\n     , 3;")))

(ert-deftest sql-reformat-test/multi-expression-select ()
  "Should return clean statement when given multi-expression select statement."
  (should (equal (sql-reformat-string "select 1, 2") "SELECT 1\n     , 2;"))
  (should (equal (sql-reformat-string "select 1, 2, 3") "SELECT 1\n     , 2\n     , 3;")))

(ert-deftest sql-reformat-test/select-with-from ()
  "Should return clean statement when given select statement with from clause."
  (should (equal (sql-reformat-string "select 1 from quetzlquatl") "SELECT 1\n  FROM [quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1 from [quetzlquatl]") "SELECT 1\n  FROM [quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from quetzlquatl") "SELECT 1\n     , 2\n  FROM [quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 From Quetzlquatl;") "SELECT 1\n     , 2\n  FROM [Quetzlquatl];")))

(ert-deftest sql-reformat-test/select-with-from-specified-table ()
  "Should return clean statement when given select statement with from clause from a specified table."
  (should (equal (sql-reformat-string "select 1 from dbo.quetzlquatl") "SELECT 1\n  FROM [dbo].[quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from thisdb..quetzlquatl") "SELECT 1\n     , 2\n  FROM [thisdb]..[quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from thisdb.dbo.quetzlquatl") "SELECT 1\n     , 2\n  FROM [thisdb].[dbo].[quetzlquatl];"))
  (should (equal (sql-reformat-string "select 1, 2 from myserver.thisdb.dbo.quetzlquatl") "SELECT 1\n     , 2\n  FROM [myserver].[thisdb].[dbo].[quetzlquatl];")))

;; (rdp-parse-string "select a, amore, somuch from dbo.a join myDb..this join second on morebla on bla;" sql-tokens)
;; (sql-ast-to-string (rdp-parse-string "select a as b" sql-tokens))
