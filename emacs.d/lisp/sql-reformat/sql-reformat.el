;;; sql-reformat.el --- Reformat an SQL region
;;; Commentary:
;; Package-Requires: ((dash "2.10.0") (rdp))
;;; Code:

(require 'dash)
(require 'rdp)

(defconst sql-keywords
  '("ADD" "ALL" "ALTER" "AND" "ANY" "AS" "ASC" "AUTHORIZATION"
    "BACKUP" "BEGIN" "BETWEEN" "BREAK" "BROWSE" "BULK" "BY"
    "CASCADE" "CASE" "CHECK" "CHECKPOINT" "CLOSE" "CLUSTERED" "COALESCE" "COLLATE" "COLUMN" "COMMIT" "COMPUTE"
    "CONSTRAINT" "CONTAINS" "CONTAINSTABLE" "CONTINUE" "CONVERT" "CREATE" "CROSS" "CURRENT" "CURRENT_DATE"
    "CURRENT_TIME" "CURRENT_TIMESTAMP" "CURRENT_USER" "CURSOR"
    "DATABASE" "DBCC" "DEALLOCATE" "DECLARE" "DEFAULT" "DELETE" "DENY" "DESC" "DISK" "DISTINCT" "DISTRIBUTED"
    "DOUBLE" "DROP" "DUMP"
    "ELSE" "END" "ERRLVL" "ESCAPE" "EXCEPT" "EXEC" "EXECUTE" "EXISTS" "EXIT" "EXTERNAL"
    "FETCH" "FILE" "FILLFACTOR" "FOR" "FOREIGN" "FREETEXT" "FREETEXTTABLE" "FROM" "FULL" "FUNCTION"
    "GOTO" "GRANT" "GROUP"
    "HAVING" "HOLDLOCK"
    "IDENTITY" "IDENTITYCOL" "IDENTITY_INSERT" "IF" "IN" "INDEX" "INNER" "INSERT" "INTERSECT" "INTO" "IS"
    "JOIN"
    "KEY" "KILL"
    "LEFT" "LIKE" "LINENO" "LOAD"
    "MERGE"
    "NATIONAL" "NOCHECK" "NONCLUSTERED" "NOT" "NULL" "NULLIF"
    "OF" "OFF" "OFFSETS" "ON" "OPEN" "OPENDATASOURCE" "OPENQUERY" "OPENROWSET" "OPENXML" "OPTION" "OR" "ORDER" "OUTER"
    "OVER"
    "PERCENT" "PIVOT" "PLAN" "PRECISION" "PRIMARY" "PRINT" "PROC" "PROCEDURE" "PUBLIC"
    "RAISERROR" "READ" "READTEXT" "RECONFIGURE" "REFERENCES" "REPLICATION" "RESTORE" "RESTRICT" "RETURN" "REVERT"
    "REVOKE" "RIGHT" "ROLLBACK" "ROWCOUNT" "ROWGUIDCOL" "RULE"
    "SAVE" "SCHEMA" "SECURITYAUDIT" "SELECT" "SEMANTICKEYPHRASETABLE" "SEMANTICSIMILARITYDETAILSTABLE"
    "SEMANTICSIMILARITYTABLE" "SESSION_USER" "SET" "SETUSER" "SHUTDOWN" "SOME" "STATISTICS" "SYSTEM_USER"
    "TABLE" "TABLESAMPLE" "TEXTSIZE" "THEN" "TO" "TOP" "TRAN" "TRANSACTION" "TRIGGER" "TRUNCATE" "TRY_CONVERT"
    "TSEQUAL"
    "UNION" "UNIQUE" "UNPIVOT" "UPDATE" "UPDATETEXT" "USE" "USER"
    "VALUES" "VARYING" "VIEW"
    "WAITFOR" "WHEN" "WHERE" "WHILE" "WITH" "WITHIN GROUP" "WRITETEXT")
  "MSSQL keywords as found on https://msdn.microsoft.com/en-us/library/ms189822.aspx.")

(defun sql-reformat-string (s)
  "Parse and reformat string S to prettify it.

This code is not yet complete, and *will* eat your SQL."
  (sql-ast-to-string (rdp-parse-string s sql-tokens)))

(defun sql-reformat (p1 p2)
  "Format SQL in region P1 through P2."
  (interactive "r")
  (save-restriction
    (narrow-to-region p1 p2)
    (let ((case-fold-search t))
      (--each sql-keywords
        (progn
          (goto-char (point-min))
          (while (re-search-forward (s-concat "\\[?\\<" it "\\>\\]?") nil t)
            (when (string= (s-upcase (match-string-no-properties 0)) it)
              (replace-match (s-upcase it)))))))))

(defvar sql-tokens
  '((query        select from)
    (empty        . "")
    (select       "SELECT" exprs)
    (from         . [("FROM" table) empty])
    (table        . [srv-table db-table sch-table id])
    (srv-table    id "\\." id "\\." [id empty] "\\." id)
    (db-table     id "\\." [id empty] "\\." id)
    (sch-table    id "\\." id)
    (exprs        . [(expr "," exprs) expr])
    (expr         . [id literal])
    (literal      . [num string])
    (num          . "[0-9.]*")
    (string       . "'\\([^']\\|''\\)*'")
    (id           . "\\([a-zA-Z][^,;. ]*\\|\\[[^,; ]*\\]\\)")))

;; The token set below is the direction I want to grow this to.
;; (defvar sql-tokens
;;   '((ctequery                  ctes query [";" empty])
;;     (query                     select sources); filter ordering grouping)
;;     (ctes                    . [cte (cte "," ctes) empty])
;;     (cte                       "WITH" "(" query ")" "AS" ident)
;;     (select                    "SELECT" select-exprs)
;;     (select-exprs            . [(select-expr "," select-exprs) select-expr])
;;     (select-expr             . [ident-select-expr select-expr-ident expr])
;;     (ident-select-expr         ident "=" expr)
;;     (select-expr-ident         expr "as" ident)
;;     (ident                   . [("\\[" "[a-zA-Z]+" "\\]") ("[a-zA-Z]+")])
;;     (expr                    . "[a-zA-Z0-9]+" )
;;     (sources                 . [from empty])
;;     (from                      "FROM" [table subquery] post-alias joins)
;;     (joins                   . [(join joins) join])
;;     (join                      "\\(INNER\\|LEFT\\|RIGHT\\|FULL\\|\\)" "JOIN" [table subquery] post-alias [joins empty] "ON" expr)
;;     (subquery                  "(" query ")")
;;     (post-alias              . [("AS" ident) empty ident])
;;     (table                   . [server-qualified-object database-qualified-object schema-qualified-object ident])
;;     (server-qualified-object   ident "\\." ident "\\." [ident empty] "\\." ident)
;;     (database-qualified-object ident "\\." [ident empty] "\\." ident)
;;     (schema-qualified-object   ident "\\." ident)
;;     (empty                   . "")))

(defun sql-quote (s)
  "Add sql-quotes to S if not yet there and if S not empty."
  (s-replace "[[" "["
             (s-replace "]]" "]"
                        (s-replace "[]" ""
                                   (s-concat "[" s "]")))))

(defalias 'sql-astts 'sql-ast-to-string)

(defun sql-ast-to-string (ast)
  "Translate the rdp-parsed AST back to sql."
  (pcase ast
    (`(empty . "")              "")
    (`(query ,select ,from)     (s-concat (sql-astts select) (sql-astts from) ";"))
    (`(select ,_ ,expr)         (s-concat "SELECT " (sql-astts expr)))
    (`(from empty . ,_)         "")
    (`(from ,_  (table . ,table))(s-concat "\n  FROM " (sql-astts table)))
    (`(sch-table . ,table)      (s-concat (sql-astts (nth 0 table)) "."
                                          (sql-astts (nth 2 table))))
    (`(db-table . ,table)       (s-concat (sql-astts (nth 0 table)) "."
                                          (sql-astts (nth 2 table)) "."
                                          (sql-astts (nth 4 table))))
    (`(srv-table . ,table)      (s-concat (sql-astts (nth 0 table)) "."
                                          (sql-astts (nth 2 table)) "."
                                          (sql-astts (nth 4 table)) "."
                                          (sql-astts (nth 6 table))))
    (`(exprs ,expr "," ,exprs)  (s-concat (sql-astts expr) "\n     , " (sql-astts exprs)))
    (`(exprs . ,expr)           (sql-astts expr))
    (`(expr . ,expr)            (sql-astts expr))
    (`(id . ,id)                (sql-quote id))
    (`(literal . ,lit)          (sql-astts lit))
    (`(num . ,num)              num)
    (`(string . ,str)           str)
    (_                          (print ast))
    ))

(provide 'sql-reformat)
;;; sql-reformat.el ends here
