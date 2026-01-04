(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table search-table)

(defmacro define-search-command (name-and-options (&rest extra-parameters)
                                 &body body)
  (destructuring-bind (name &rest options)
      (alexandria:ensure-list name-and-options)
    `(define-buffer-command (,name search-table ,@options) ,extra-parameters
       ,@body)))

;;; Ordinary search

(define-search-command com-search-forward ((string 'string))
  (unless (edit:perform buffer 'edit.search:search string :forward) ; TODO case mode etc
    (mini:display-message "No match")))

(define-search-command com-search-backward ((string 'string))
  (unless (edit:perform buffer 'edit.search:search string :backward)
    (mini:display-message "No match")))

;;; Incremental search

(define-search-command com-incremental-search-forward ()
  (edit:perform buffer 'edit.search:incremental-search :forward)
  (update-for-search buffer))
(bind-key 'search-table '(#\s :control) 'com-incremental-search-forward)

(define-search-command com-incremental-search-backward ()
  (edit:perform buffer 'edit.search:incremental-search :backward)
  (update-for-search buffer))
(bind-key 'search-table '(#\r :control) 'com-incremental-search-backward)

;;; Incremental search mode

(clim:define-command-table incremental-search-table
  :inherit-from (global-table))

(defmacro define-incremental-search-command (name-and-options
                                             (&rest extra-parameters)
                                             &body body)
  (destructuring-bind (name &rest options)
      (alexandria:ensure-list name-and-options)
    `(define-buffer-command (,name incremental-search-table ,@options) ,extra-parameters
       ,@body)))

(define-incremental-search-command com-finish-incremental-search ()
  (edit:perform buffer 'edit.search:finish-incremental-search))
(bind-key 'incremental-search-table '(#\Return) 'com-finish-incremental-search)

(define-incremental-search-command com-abort-incremental-search ()
  (edit:perform buffer 'edit.search:abort-incremental-search))
(bind-key 'incremental-search-table '(#\c :control) 'com-abort-incremental-search) ; TODO C-g doesn't work

(defun update-for-search (buffer &key comment)
  (mini:display-message (edit.search:description
                         buffer :comment comment)))

(define-incremental-search-command (com-incremental-search-set-case-mode
                                    :name "Set Case Mode")
    ((mode '(member :ignore :match)))
  (edit:perform buffer '(setf edit.search:case-mode) mode)
  (update-for-search buffer))
(bind-key 'incremental-search-table '(#\m :meta) 'com-incremental-search-set-case-mode :match)
(bind-key 'incremental-search-table '(#\i :meta) 'com-incremental-search-set-case-mode :ignore)

(define-incremental-search-command com-incremental-search-extend-query ((item 't))
  (edit:perform buffer 'edit.search:extend-query item)
  (update-for-search buffer))
(loop for i from 32 to 126
      for char = (code-char i)
      do (bind-key 'incremental-search-table `(,char) 'com-incremental-search-extend-query char))
(bind-key 'incremental-search-table '(#\j :control) 'com-incremental-search-extend-query #\Newline)

(define-incremental-search-command com-incremental-search-truncate-query ()
  (edit:perform buffer 'edit.search:truncate-query)
  (update-for-search buffer))
(bind-key 'incremental-search-table '(#\Backspace) 'com-incremental-search-truncate-query)

(define-incremental-search-command com-incremental-search-next-match ()
  (edit:perform buffer 'edit.search:next-match)
  (update-for-search buffer :comment "Next match"))
(bind-key 'incremental-search-table '(#\s :control) 'com-incremental-search-next-match)

(define-incremental-search-command com-incremental-search-previous-match ()
  (edit:perform buffer 'edit.search:previous-match)
  (update-for-search buffer :comment "Previous match"))
(bind-key 'incremental-search-table '(#\r :control) 'com-incremental-search-previous-match)

(define-incremental-search-command com-convert-matches-to-sites ()
  (edit.search:convert-matches-to-sites buffer))
