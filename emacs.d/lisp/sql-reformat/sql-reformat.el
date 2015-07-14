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

(defun sql-reformat-string (s &optional indent)
  "Parse and reformat string S to prettify it.

This code is not yet complete, and *will* eat your SQL.  INDENT
is the number of spaces to indent everything after the first
line."
  (s-concat
   (sql-ast-to-string (rdp-parse-string s sql-tokens) (or indent 0))
   (when (> (length s) rdp-best)
     (s-concat
      (sql-newline)
      "-- PARSE ERROR\n"
      (substring s rdp-best)))))

(defun sql-reformat (p1 p2)
  "Format SQL in region P1 through P2."
  (interactive "r")
  (let ((indent (save-excursion (goto-char p1) (current-column))))
    (save-restriction
      (narrow-to-region p1 p2)
      (let ((s (buffer-substring-no-properties p1 p2)))
        (delete-region p1 p2)
        (insert (sql-reformat-string s indent))
        (let ((case-fold-search t))
          (--each sql-keywords
            (progn
              (goto-char (point-min))
              (while (re-search-forward (s-concat "\\[?\\<" it "\\>\\]?") nil t)
                (when (string= (s-upcase (match-string-no-properties 0)) it)
                  (replace-match (s-upcase it)))))))))))

(defvar sql-tokens
  '((statement           query [";" empty])
    (query               select from where)
    (empty             . "")
    (select              "SELECT" [directives empty] aliasable-exprs)
    (directives        . [(directive directives) directive])
    (directive         . [distinct top])
    (distinct          . "DISTINCT")
    (top                 "TOP" num)
    (from              . [("FROM" table) empty])
    (where             . [("WHERE" pred) empty])
    (table             . [subquery-as-table srv-table db-table sch-table id])
    (subquery-as-table   pexpr "\\(AS\\)?" id)
    (srv-table           id "\\." id "\\." [id empty] "\\." id)
    (db-table            id "\\." [id empty] "\\." id)
    (sch-table           id "\\." id)
    (aliasable-exprs   . [(aliasable-expr "," aliasable-exprs) aliasable-expr])
    (exprs             . [(expr "," exprs) expr])
    (expr              . [cast pexpr column id literal])
    (pexpr               "(" [query expr] ")")
    (cast                "CAST" "(" expr "AS" type ")")
    (type              . ["INT\\(EGER\\)?" "N?VARCHAR\\(([0-9+])\\|MAX\\)?" "BIT" "FLOAT"])
    (literal           . [num string])
    (num               . "[0-9][0-9\\.]*")
    (string            . "'\\([^']\\|''\\)*'")
    (id                . "\\([a-zA-Z][^,;.() ]*\\|\\[[^,; ]+\\]\\)")
    (aliasable-expr    . [aliasable-column post-alias pre-alias expr star])
    (aliasable-column  . column)
    (post-alias          expr "AS" id)
    (pre-alias           id "=" expr)
    (column              id "\\." id)
    (star              . "*")
    (pred              . [(predexpr predop pred) predexpr])
    (predexpr            "\\(NOT\\)?" expr cmp expr)
    (predop            . [and or])
    (and               . "AND")
    (or                . "OR")
    (cmp               . "\\(=\\|<>\\)")))

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

(defvar sql-cur-indent 0)

(defmacro sql-adjusting-indent-for (s &rest body)
  "Adjust the sql-indent with the length of string S during execution of BODY."
  `(let ((sql-cur-indent (+ (length ,s) sql-cur-indent)))
     ,@body))

(defun sql-newline ()
  (s-concat "\n" (make-string sql-cur-indent ?\s)))

(defun sql-ast-to-string (ast &optional indent)
  "Translate the rdp-parsed AST back to sql.

INDENT is the number of spaces to indent everything after the
first line."
  (when indent
    (setq sql-cur-indent indent))
  (pcase ast
    (`(empty . "")                               "")
    (`(statement ,query ,_)                      (s-concat (sql-astts query) ";"))
    (`(query ,select ,from ,where)               (s-concat (sql-astts select) (sql-astts from) (sql-astts where)))
    (`(select ,_ ,directive ,expr)               (s-concat "SELECT " (sql-astts directive) (sql-astts expr)))
    (`(from empty . ,_)                          "")
    (`(from ,_ ,table)                           (s-concat (sql-newline) "  FROM " (sql-astts table)))
    (`(where empty . ,_)                         "")
    (`(where ,_ ,pred)                           (s-concat (sql-newline) " WHERE " (sql-astts pred)))
    (`(table id . ,table)                        (sql-quote table))
    (`(table subquery-as-table ,pexpr ,_ ,alias) (s-concat (sql-astts pexpr) " AS " (sql-astts alias)))
    (`(table sch-table . ,table)                 (s-concat (sql-astts (nth 0 table)) "."
                                                           (sql-astts (nth 2 table))))
    (`(table db-table . ,table)                  (s-concat (sql-astts (nth 0 table)) "."
                                                           (sql-astts (nth 2 table)) "."
                                                           (sql-astts (nth 4 table))))
    (`(table srv-table . ,table)                 (s-concat (sql-astts (nth 0 table)) "."
                                                           (sql-astts (nth 2 table)) "."
                                                           (sql-astts (nth 4 table)) "."
                                                           (sql-astts (nth 6 table))))
    (`(directives ,directive ,directives)        (s-concat (sql-astts directive) " " (sql-astts directives)
                                                           (sql-newline) "       "))
    (`(directives . ,directive)                  (s-concat (sql-astts directive) (sql-newline) "       "))
    (`(directive top ,_ ,num)                    (s-concat "TOP " (sql-astts num)))
    (`(directive distinct . ,_ )                 "DISTINCT")
    (`(aliasable-exprs ,expr "," ,exprs)         (s-concat (sql-astts expr) (sql-newline) "     , " (sql-astts exprs)))
    (`(aliasable-exprs . ,expr)                  (sql-astts expr))
    (`(exprs ,expr "," ,exprs)                   (s-concat (sql-astts expr) (sql-newline) "     , " (sql-astts exprs)))
    (`(exprs . ,expr)                            (sql-astts expr))
    (`(expr . ,expr)                             (sql-astts expr))
    (`(pexpr "(" ,contained-expr ")")            (s-concat "( "
                                                           (sql-adjusting-indent-for "SELECT ( "
                                                                                     (sql-astts contained-expr))
                                                           (sql-newline)
                                                           "       )"))
    (`(cast ,_ ,_ ,expr ,_ ,type ,_)             (s-concat "cast (" (sql-adjusting-indent-for " ( " (sql-astts expr)) " AS " (sql-astts type) ")"))
    (`(type . ,type)                             (s-downcase type))
    (`(aliasable-expr . ,expr)                   (sql-astts expr))
    (`(pred ,predexpr ,predop, pred)             (s-concat (sql-astts predexpr)
                                                           (sql-newline) (sql-astts predop) (sql-astts pred)))
    (`(pred . ,predexpr)                         (sql-astts predexpr))
    (`(predexpr ,not ,expr1 ,cmp ,expr2)         (s-concat (unless (string= "" not) "NOT ")
                                                           (sql-astts expr1) " " (sql-astts cmp) " " (sql-astts expr2)))
    (`(predop and . ,_)                          "       AND ")
    (`(predop or . ,_)                           "    OR ")
    (`(cmp . ,cmp)                               cmp)
    (`(aliasable-column column ,alias ,_ ,fld)   (sql-adjusting-indent-for (s-concat (sql-astts fld) " = ")
                                                                           (s-concat (sql-astts fld) " = "
                                                                                     (sql-astts alias) "."
                                                                                     (sql-astts fld))))
    (`(pre-alias ,alias ,_ ,expr)                (sql-adjusting-indent-for (s-concat (sql-astts alias) " = ")
                                                                           (s-concat (sql-astts alias) " = "
                                                                                     (sql-astts expr))))
    (`(post-alias ,expr ,_ ,alias)               (sql-adjusting-indent-for (s-concat (sql-astts alias) " = ")
                                                                           (s-concat (sql-astts alias) " = "
                                                                                     (sql-astts expr))))
    (`(column ,alias ,_ ,fld)                    (s-concat (sql-astts alias) "."
                                                           (sql-astts fld)))
    (`(star . ,_)                                "*")
    (`(id . ,id)                                 (sql-quote id))
    (`(literal . ,lit)                           (sql-astts lit))
    (`(num . ,num)                               num)
    (`(string . ,str)                            str)
    (_                                           (print ast))
    ))

(provide 'sql-reformat)
;;; sql-reformat.el ends here
