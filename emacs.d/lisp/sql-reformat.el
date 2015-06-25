;;; sql-reformat.el --- Reformat an SQL region
;;; Commentary:
;; Package-Requires: ((dash "2.10.0"))
;;; Code:

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

(provide 'sql-reformat)
;;; sql-reformat.el ends here
