;;; mu4e-tagging-test.el --- tests for minor mode for quick tagging in mu4e -*- lexical-binding: t -*-

(require 'package)
(package-initialize)

(push "." load-path)
(require 'mu4e-tagging-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test harness

(setq m4t-test-state-variables
      '(creichen/mu4e-tagging-tags
	creichen/mu4e-tagging-minor-mode-keymap
	creichen/mu4e-tagging-minor-mode-auto-keymap
	creichen/mu4e-tagging-known-tags
	creichen/mu4e-tagging-categories-var
	creichen/mu4e-tagging-flags-var
	creichen/mu4e-tagging-reverse-key-table-tag
	creichen/mu4e-tagging-reverse-key-table-untag
	creichen/mu4e-tagging-untag-prefix
	creichen/mu4e-tagging-tag-info-window
	creichen/mu4e-tagging-mail-info-window
	creichen/mu4e-tagging-tag-info-window-buf
	creichen/mu4e-tagging-mail-info-window-buf
	creichen/mu4e-tagging-query-flags
	creichen/mu4e-tagging-query-category
	creichen/mu4e-tagging-query-rewrite-function-backup
	creichen/mu4e-tagging-query-separator
	creichen/mu4e-tagging-query-rewrite-last-original-query
	creichen/mu4e-tagging-query-rewrite-last-rewritten-query
	))


(defun m4t-test-state-backup ()
  "Backs up all relevant state into an alist."
  (mapcar (lambda (sym)
	    (cons sym (eval sym)))
	  m4t-test-state-variables)
  )

(defun m4t-test-state-restore (backups)
  "Restores all relevant state from an alist passed as argument."
  (dolist (item backups)
	  (-let (((sym . old-binding) item))
	    (setq sym old-binding))
	  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for test harness

(ert-deftest test-state-backup ()
  "Check that the test harness' backup functionality records all protected variables."
  (should (equal m4t-test-state-variables
		 (mapcar 'car (m4t-test-state-backup))))
  (dolist (binding (m4t-test-state-backup))
    (cdr binding))
  )

(ert-deftest test-state-backup-restore ()
  "Check that restoring all protected variables in the test harness does not change the result of backing up."
  (let ((backup1 (m4t-test-state-backup)))
    (let ((backup2 (m4t-test-state-backup)))
      (should (equal backup1 backup2))
      (m4t-test-state-restore backup2)
      (should (equal backup1 (m4t-test-state-backup)))
      (m4t-test-state-restore backup1)
      (should (equal backup2 (m4t-test-state-backup)))
      )))

(defmacro protecting-state (&rest BODY)
  "Run the test in BODY and restore the creichen/mu4e-tagging state afterwards."
  (let ((backup (m4t-test-state-backup)))
    (condition-case result
	`(progn
	   ,@BODY
	   )
      (:success (progn
		  (m4t-test-state-restore backup)
		  result)
		)
      (t (progn
	   (m4t-test-state-restore backup)
	   throw result))))
  )

(defmacro should-preserve-all-state (&rest BODY)
  "Run the test in BODY and assert that the state of creichen/mu4e-tagging is not altered."
  (let ((backup (m4t-test-state-backup)))
    (condition-case result
	`(progn
	   ,@BODY
	   )
      (:success (progn
		  (should (equal backup (m4t-test-state-backup)))
		  (m4t-test-state-restore backup)
		  result)
		)
      (t (progn
	   (m4t-test-state-restore backup)
	   throw result))))
  )

(ert-deftest test-re-customise-tagging-tags ()
  "Check that re-updating to the default tags does not change the state."
  (should-preserve-all-state
   (customize-set-variable 'creichen/mu4e-tagging-tags
			   creichen/mu4e-tagging-tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun sorted-strings (l)
  "Returns l sorted according to string-lessp"
  (let ((l2 (cl-copy-list l)))
    (sort l2 'string-lessp)
    l2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customisation works as intended

(defun setup-test-tags ()
  "Sets a particualr tags configuration that we use for testing here."
  (customize-set-variable 'creichen/mu4e-tagging-tags
			  '(("a" :key "a" :short "A" :flag t :foreground "yellow" :background "black")
			    ("bassoon" :key "b" :short "bas")
			    ("ba")
			    ("chips")
			    ("particularly-lengthy" :short "M" :flag t :box t :weight "ultra-thin")
			  )))


(ert-deftest test-customise--category-tags ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (should (equal '("bassoon" "ba" "chips")
		  (creichen/mu4e-tagging-known-category-tags)))
  ))

(ert-deftest test-customise--known-tags ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (should (equal '("a" "ba" "bassoon" "chips" "particularly-lengthy")
		  (sorted-strings (hash-table-keys creichen/mu4e-tagging-known-tags))))
   ))

(ert-deftest test-customise--auto-keymap ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (should (not (null
		  (keymap-lookup creichen/mu4e-tagging-minor-mode-auto-keymap
				 (kbd "a")))))
   (should (equal "a"
		  (car (gethash [?a] creichen/mu4e-tagging-reverse-key-table-tag
				))))
   (should (not (null
		  (keymap-lookup creichen/mu4e-tagging-minor-mode-auto-keymap
				 (kbd "c")))))
   (should (equal "chips"
		  (car (gethash [?c] creichen/mu4e-tagging-reverse-key-table-tag
				))))
   (should (not (null
		  (keymap-lookup creichen/mu4e-tagging-minor-mode-auto-keymap
				 (kbd "1")))))
   (should (equal "ba"
		  (car (gethash [?1] creichen/mu4e-tagging-reverse-key-table-tag
				))))
   (should (not (null
		  (keymap-lookup creichen/mu4e-tagging-minor-mode-auto-keymap
				 (kbd "b")))))
   (should (equal "bassoon"
		  (car (gethash [?b] creichen/mu4e-tagging-reverse-key-table-tag
				))))
   (should (not (null
		  (keymap-lookup creichen/mu4e-tagging-minor-mode-auto-keymap
				 (kbd "p")))))
   (should (equal "particularly-lengthy"
		  (car (gethash [?p] creichen/mu4e-tagging-reverse-key-table-tag
				))))
  ))

(ert-deftest test-customise--dotags ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (let ((results nil))
     (creichen/mu4e-tagging-dotags (taginfo tagty)
				   (if (null tagty)
				       (push '(separator) results)
				     (let* ((tag-name (car taginfo))
					    (tag-plist (cdr taginfo)))
				       (push (list tagty tag-name (plist-get tag-plist :short) (plist-get tag-plist :key))
					     results)
				       ))
				   )
     ;; results will be in reverse order
     (should (equal '(
		      (flag "particularly-lengthy" "M" [?p])
		      (flag "a" "A" [?a])
		      (separator)
		      (category "chips" "chips" [?c])
		      (category "ba" "ba" [?1])
		      (category "bassoon" "bas" [?b])
		      )
		    results)
   ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query rewriting works as intended

(ert-deftest test-query--mode-preservation ()
  "Entering and exiting query mode alters and restores mu4e-query-rewrite-function."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (should (equal #'creichen/mu4e-tagging--query-rewrite-mu4e mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-disable)
     (should (equal old mu4e-query-rewrite-function))
     )
   )
  )

(ert-deftest test-query--filter-empty ()
  "Query mode does not alter queries by default."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (should (equal "(foo)" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-category ()
  "Query mode filtering for one category."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-category "ba")
     (should (equal "(foo) AND x:ba" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-uncategorized ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-category 'uncategorized)
     (should (equal "(foo) AND (NOT x:bassoon) AND (NOT x:ba) AND (NOT x:chips)" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-positive-flag ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-category nil)
     (setq creichen/mu4e-tagging-query-flags '(("a" . +)))
     (should (equal "(foo) AND x:a" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-negative-flag ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-flags '(("a" . -)))
     (should (equal "(foo) AND (NOT x:a)" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-blended-flags ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-flags '(("particularly-lengthy" . -) ("a" . +)))
     (should (equal "(foo) AND (NOT x:particularly-lengthy) AND x:a" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-flags-and-category ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-category "ba")
     (setq creichen/mu4e-tagging-query-flags '(("particularly-lengthy" . -) ("a" . +)))
     (should (equal "(foo) AND x:ba AND (NOT x:particularly-lengthy) AND x:a" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

(ert-deftest test-query--filter-flags-and-uncategorized ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (creichen/mu4e-tagging-query-submode-enable)
     (setq creichen/mu4e-tagging-query-category 'uncategorized)
     (setq creichen/mu4e-tagging-query-flags '(("particularly-lengthy" . +) ("a" . -)))
     (should (equal "(foo) AND (NOT x:bassoon) AND (NOT x:ba) AND (NOT x:chips) AND x:particularly-lengthy AND (NOT x:a)" (creichen/mu4e-tagging--query-rewrite "foo")))
     (creichen/mu4e-tagging-query-submode-disable)
     )
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query setter helper functions work as intended

(ert-deftest test-query-support--parse-category ()
  "Category strings are parsed correctly."
  (should-preserve-all-state
   (should (equal nil
		  (creichen/mu4e-tagging--query-category-parse "")))
   (should (equal 'uncategorized
		  (creichen/mu4e-tagging--query-category-parse "-")))
   (should (equal "bassoon"
		  (creichen/mu4e-tagging--query-category-parse "bassoon")))
   )
  )

(ert-deftest test-query-support--parse-flag ()
  "Singel flag specifications are parsed correctly."
  (should-preserve-all-state
   (should (equal nil
		  (creichen/mu4e-tagging--query-flag-parse "")))
   (should (equal nil
		  (creichen/mu4e-tagging--query-flag-parse "=foo")))
   (should (equal nil
		  (creichen/mu4e-tagging--query-flag-parse "+")))
   (should (equal nil
		  (creichen/mu4e-tagging--query-flag-parse "-")))
   (should (equal nil
		  (creichen/mu4e-tagging--query-flag-parse "-")))
   (should (equal '("foo" . -)
		  (creichen/mu4e-tagging--query-flag-parse "-foo")))
   (should (equal '("bar" . +)
		  (creichen/mu4e-tagging--query-flag-parse "+bar")))
   )
  )

(ert-deftest test-query-support--parse-flags ()
  "Multi-flag specifications are parsed correctly."
  (should-preserve-all-state
   (should (equal nil
		  (creichen/mu4e-tagging--query-flags-parse "")))
   (should (equal nil
		  (creichen/mu4e-tagging--query-flags-parse ",")))
   (should (equal nil
		  (creichen/mu4e-tagging--query-flags-parse "foo,bar")))
   (should (equal '(("foo" . -))
		  (creichen/mu4e-tagging--query-flags-parse "-foo")))
   (should (equal '(("bar" . +))
		  (creichen/mu4e-tagging--query-flags-parse "+bar")))
   (should (equal '(("bar" . +) ("foo" . -))
		  (creichen/mu4e-tagging--query-flags-parse "+bar,-foo")))
   (should (equal '(("bar" . +) ("foo" . -))
		  (creichen/mu4e-tagging--query-flags-parse "+bar,error,-foo")))
   )
  )

(ert-deftest test-query-support--set-read-category ()
  "After setting the category, the category query state stringification looks as expected."
  (should-preserve-all-state
   (should (equal ""
		  (creichen/mu4e-tagging--query-current-category-string)))
   (creichen/mu4e-tagging-query-category "")
   (should (equal ""
		  (creichen/mu4e-tagging--query-current-category-string)))
   (creichen/mu4e-tagging-query-category "-")
   (should (equal 'uncategorized creichen/mu4e-tagging-query-category))
   (should (equal "-"
		  (creichen/mu4e-tagging--query-current-category-string)))
   (creichen/mu4e-tagging-query-category "bassoon")
   (should (equal "bassoon"
		  (creichen/mu4e-tagging--query-current-category-string)))
   (creichen/mu4e-tagging-query-submode-disable)
   )
  )

(ert-deftest test-query-support--set-read-flags ()
  "After setting the query flags, the flag query state stringification looks as expected."
  (should-preserve-all-state
   (should (equal ""
		  (creichen/mu4e-tagging--query-current-flags-string)))
   (creichen/mu4e-tagging-query-flags "")
   (should (equal nil
		  creichen/mu4e-tagging-query-flags))
   (creichen/mu4e-tagging-query-flags "+a")
   (should (equal '(("a" . +))
		  creichen/mu4e-tagging-query-flags))
   (creichen/mu4e-tagging-query-flags "-particularly-lengthy,+a")
   (should (equal '(("particularly-lengthy" . -) ("a" . +))
		  creichen/mu4e-tagging-query-flags))
   (creichen/mu4e-tagging-query-flags "-a")
   (should (equal '(("a" . -))
		  creichen/mu4e-tagging-query-flags))
   (creichen/mu4e-tagging-query-submode-disable)
   )
  )
