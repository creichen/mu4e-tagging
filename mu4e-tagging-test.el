;;; mu4e-tagging-test.el --- Tests for mu4e-tagging -*- lexical-binding: t -*-

;; Copyright (C) 2023 Christoph Reichenbach

;; Author: Christoph Reichenbach <creichen@creichen.net>
;; Maintainer: Christoph Reichenbach <creichen@creichen.net>
;; Created: 12 Oct 2023

;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.0") (compat "29.1"))

;; Keywords: mail
;; URL: https://github.com/creichen/mu4e-tagging

;; This file is not part of GNU Emacs.

 ;; mu4e-tagging is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

(require 'package)
(package-initialize)

(load-file "mu4e-tagging.el")
(require 'el-mock)

;; Set up default key bindings

(mu4e-tagging-setup-default-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test harness

(setq m4t-test-state-variables
      '(mu4e-tagging-tags
        mu4e-tagging-mode-map
        mu4e-tagging--dyn-keymap
        mu4e-tagging--known-tags
        mu4e-tagging--categories
        mu4e-tagging--flags
        mu4e-tagging--key-action-table
        mu4e-tagging-untag-prefix
        mu4e-tagging-tag-info-window
        mu4e-tagging-mail-info-window
        mu4e-tagging-tag-info-window-buf
        mu4e-tagging-mail-info-window-buf
        mu4e-tagging-query-flags
        mu4e-tagging-query-category
        mu4e-tagging-query-rewrite-function-backup
        mu4e-tagging-query-separator
        mu4e-tagging-query-rewrite-last-original-query
        mu4e-tagging-query-rewrite-last-rewritten-query))


(defun m4t-test-state-backup ()
  "Backs up all relevant state into an alist."
  (mapcar (lambda (sym)
            (cons sym (eval sym)))
          m4t-test-state-variables))

(defun m4t-test-state-restore (backups)
  "Restore all relevant state from an alist passed in BACKUPS."
  (dolist (item backups)
          (-let (((sym . old-binding) item))
            (setq sym old-binding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for test harness

(ert-deftest test-state-backup ()
  "Test harness' backups record all protected variables."
  (should (equal m4t-test-state-variables
                 (mapcar 'car (m4t-test-state-backup))))
  (dolist (binding (m4t-test-state-backup))
    (cdr binding))
  )

(ert-deftest test-state-backup-restore ()
  "Restoring protected variables in test harness does not affect backup."
  (let ((backup1 (m4t-test-state-backup)))
    (let ((backup2 (m4t-test-state-backup)))
      (should (equal backup1 backup2))
      (m4t-test-state-restore backup2)
      (should (equal backup1 (m4t-test-state-backup)))
      (m4t-test-state-restore backup1)
      (should (equal backup2 (m4t-test-state-backup))))))

(defmacro protecting-state (&rest BODY)
  "Run the test in BODY and restore the mu4e-tagging state afterwards."
  (let ((backup (m4t-test-state-backup)))
    (condition-case result
        `(progn
           ,@BODY)
      (:success (progn
                  (m4t-test-state-restore backup)
                  result))
      (t (progn
           (m4t-test-state-restore backup)
           throw result)))))

(defmacro should-preserve-all-state (&rest BODY)
  "Run test in BODY, assert that state of mu4e-tagging is not altered."
  (let ((backup (m4t-test-state-backup)))
    (condition-case result
        `(progn
           ,@BODY)
      (:success (progn
                  (should (equal backup (m4t-test-state-backup)))
                  (m4t-test-state-restore backup)
                  result))
      (t (progn
           (m4t-test-state-restore backup)
           throw result)))))

(ert-deftest test-re-customise-tagging-tags ()
  "Check that re-updating to default tags does not change the state."
  (should-preserve-all-state
   (customize-set-variable 'mu4e-tagging-tags
                           mu4e-tagging-tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun sorted-strings (l)
  "Returns l sorted according to string-lessp"
  (let ((l2 (cl-copy-list l)))
    (sort l2 'string-lessp)
    l2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customisation works as intended

(defun setup-test-tags ()
  "Sets a particualr tags configuration that we use for testing here."
  (customize-set-variable 'mu4e-tagging-tags
                          '(("a"
                             :key "a"
                             :short "A"
                             :flag t
                             :foreground "yellow"
                             :background "black")
                            ("bassoon"
                             :key "b"
                             :short "bas")
                            ("ba")
                            ("chips")
                            ("particularly-lengthy"
                             :short "M"
                             :flag t
                             :box t
                             :weight "ultra-thin"))))


(ert-deftest test-customise--category-tags ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (should (equal '("bassoon" "ba" "chips")
                  (mu4e-tagging--category-tags)))))

(ert-deftest test-customise--known-tags ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (should (equal '("a" "ba" "bassoon" "chips" "particularly-lengthy")
                  (sorted-strings (hash-table-keys
                                   mu4e-tagging--known-tags))))))

(ert-deftest test-customise--dyn-keymap ()
  "Customising the tags generates the correct set of category tags."
  (protecting-state
   (setup-test-tags)
   (should (not (null
                  (keymap-lookup mu4e-tagging--dyn-keymap
                                 (kbd "a")))))
   (should (equal "a"
                  (car (gethash [?a] mu4e-tagging--key-action-table
                                ))))
   (should (not (null
                  (keymap-lookup mu4e-tagging--dyn-keymap
                                 (kbd "c")))))
   (should (equal "chips"
                  (car (gethash [?c] mu4e-tagging--key-action-table
                                ))))
   (should (not (null
                  (keymap-lookup mu4e-tagging--dyn-keymap
                                 (kbd "1")))))
   (should (equal "ba"
                  (car (gethash [?1] mu4e-tagging--key-action-table
                                ))))
   (should (not (null
                  (keymap-lookup mu4e-tagging--dyn-keymap
                                 (kbd "b")))))
   (should (equal "bassoon"
                  (car (gethash [?b] mu4e-tagging--key-action-table
                                ))))
   (should (not (null
                  (keymap-lookup mu4e-tagging--dyn-keymap
                                 (kbd "p")))))
   (should (equal "particularly-lengthy"
                  (car (gethash [?p] mu4e-tagging--key-action-table
                                ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query rewriting works as intended

(ert-deftest test-query--mode-preservation ()
  "Enter/exit query mode alters/restores `mu4e-query-rewrite-function'."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (should (equal #'mu4e-tagging--query-rewrite-mu4e
                    mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-disable)
     (should (equal old mu4e-query-rewrite-function)))))

(ert-deftest test-query--filter-empty ()
  "Query mode does not alter queries by default."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (should (equal "(foo)" (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-category ()
  "Query mode filters one category."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (setq mu4e-tagging-query-category "ba")
     (should (equal "(foo) AND x:ba"
                    (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-uncategorized ()
  "Query mode filters uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (setq mu4e-tagging-query-category 'uncategorized)
     (should
      (equal
       "(foo) AND (NOT x:bassoon) AND (NOT x:ba) AND (NOT x:chips)"
       (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-positive-flag-handler ()
  "Query mode filters by required flags."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (setq mu4e-tagging-query-category nil)
     (mu4e-tagging-query-flags '("+a"))
     (should (equal "(foo) AND x:a"
                    (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-negative-flag-handler ()
  "Query mode filters by blocked flags."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (mu4e-tagging-query-flags "-a")
     (should (equal "(foo) AND (NOT x:a)"
                    (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-blended-flags-handler ()
  "Query mode filters by mixed flags (blocked + required)."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (mu4e-tagging-query-flags '("-particularly-lengthy"
                                 "+a"))
     (should (equal
              "(foo) AND x:a AND (NOT x:particularly-lengthy)"
              (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-flags-and-category ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (setq mu4e-tagging-query-category "ba")
     (mu4e-tagging-query-flags '("-particularly-lengthy"
                                 "+a"))
     (should (equal
              "(foo) AND x:ba AND x:a AND (NOT x:particularly-lengthy)"
              (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

(ert-deftest test-query--filter-flags-and-uncategorized ()
  "Query mode filtering for uncategorised messages."
  (should-preserve-all-state
   (let ((old mu4e-query-rewrite-function))
     (mu4e-tagging-query-submode-enable)
     (setq mu4e-tagging-query-category 'uncategorized)
     (mu4e-tagging-query-flags '("+particularly-lengthy"
                                 "-a"))
     (should
      (equal
         "(foo) AND (NOT x:bassoon) AND (NOT x:ba) AND (NOT x:chips) AND (NOT x:a) AND x:particularly-lengthy"
         (mu4e-tagging--query-rewrite "foo")))
     (mu4e-tagging-query-submode-disable))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query setter helper functions work as intended

(ert-deftest test-query-support--parse-category ()
  "Category strings are parsed correctly."
  (should-preserve-all-state
   (should (equal nil
                  (mu4e-tagging--query-category-parse "")))
   (should (equal 'uncategorized
                  (mu4e-tagging--query-category-parse "-")))
   (should (equal "bassoon"
                  (mu4e-tagging--query-category-parse "bassoon")))))

(ert-deftest test-query-support--parse-flag-handler ()
  "Single flag specifications are parsed correctly."
  (should-preserve-all-state
   (should (equal nil
                  (mu4e-tagging--query-flag-parse "")))
   (should (equal nil
                  (mu4e-tagging--query-flag-parse "=foo")))
   (should (equal nil
                  (mu4e-tagging--query-flag-parse "+")))
   (should (equal nil
                  (mu4e-tagging--query-flag-parse "-")))
   (should (equal nil
                  (mu4e-tagging--query-flag-parse "-")))
   (should (equal '("foo" . -)
                  (mu4e-tagging--query-flag-parse "-foo")))
   (should (equal '("bar" . +)
                  (mu4e-tagging--query-flag-parse "+bar")))))

(ert-deftest test-query-support--parse-flags-handler ()
  "Multi-flag specifications are parsed correctly."
  (should-preserve-all-state
   (should (equal nil
                  (mu4e-tagging--query-flags-parse "")))
   (should (equal nil
                  (mu4e-tagging--query-flags-parse ",")))
   (should (equal nil
                  (mu4e-tagging--query-flags-parse "foo,bar")))
   (should (equal '(("foo" . -))
                  (mu4e-tagging--query-flags-parse "-foo")))
   (should (equal '(("bar" . +))
                  (mu4e-tagging--query-flags-parse "+bar")))
   (should (equal '(("bar" . +) ("foo" . -))
                  (mu4e-tagging--query-flags-parse "+bar,-foo")))
   (should (equal '(("bar" . +) ("foo" . -))
                  (mu4e-tagging--query-flags-parse "+bar,error,-foo")))
   (should (equal '(("a" . +))
                  (mu4e-tagging--query-flags-parse '("+a"))))
   (should (equal '(("a" . +))
                  (mu4e-tagging--query-flags-parse '(nil "+a" nil))))
   (should (equal '(("a" . +) ("b" . -))
                  (mu4e-tagging--query-flags-parse '(nil "+a" "-b"))))
   (should (equal nil
                  (mu4e-tagging--query-flags-parse nil)))))

(ert-deftest test-query-support--set-read-category ()
  "Query state stringification after setting category."
  (should-preserve-all-state
   (should (equal ""
                  (mu4e-tagging--query-current-category-string)))
   (mu4e-tagging-query-category "")
   (should (equal ""
                  (mu4e-tagging--query-current-category-string)))
   (mu4e-tagging-query-category "-")
   (should (equal 'uncategorized mu4e-tagging-query-category))
   (should (equal "-"
                  (mu4e-tagging--query-current-category-string)))
   (mu4e-tagging-query-category "bassoon")
   (should (equal "bassoon"
                  (mu4e-tagging--query-current-category-string)))
   (mu4e-tagging-query-submode-disable)))

(ert-deftest test-query-support--set-read-flags ()
  "Query state stringification after setting query flags."
  (should-preserve-all-state
   (should (equal ""
                  (mu4e-tagging--query-current-flags-string)))
   (mu4e-tagging-query-flags "")
   (should (equal nil
                  (mu4e-tagging--query-flags-alist)))
   (mu4e-tagging-query-flags "+a")
   (should (equal '(("a" . +))
                  (mu4e-tagging--query-flags-alist)))
   (mu4e-tagging-query-flags "-particularly-lengthy,+a")
   (should (equal '( ("a" . +) ("particularly-lengthy" . -))
                  (mu4e-tagging--query-flags-alist)))
   (mu4e-tagging-query-flags "-a")
   (should (equal '(("a" . -))
                  (mu4e-tagging--query-flags-alist)))
   (mu4e-tagging-query-submode-disable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key conversions work as intended

(ert-deftest test-key-convert--from-vec ()
  "Key binding conversion around `key-valid-p'-style strings."
  (should-preserve-all-state
   (dolist (equivalent-keyspecs '(([?a] "a" ?a)
                   ([?b] "b" ?b)
                   ([25] "C-y" 25)
                   ([26] "C-z" 26)
                   ([?A ?b] "A b")
                   ([?C ?d] "C d")))
     (-let (((key-vec key-string &rest _) equivalent-keyspecs))
       ;; sanity check
       (should (key-valid-p key-string))
       (dolist (key-encoding equivalent-keyspecs)
         (should (equal key-string
                        (mu4e-tagging-keystring key-encoding)))
         (should (equal key-vec
                        (mu4e-tagging-keyvec key-encoding))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filter updates

(ert-deftest test-filter-update--require-category-handler ()
  "The interceptor handler for require-filters works for categories."
  (should-preserve-all-state
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?b])
     (mock (mu4e-tagging-query-category "bassoon"))
     (stub mu4e-tagging-query-rerun)
     (mu4e-tagging--interceptor-query-require))
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?c])
     (mock (mu4e-tagging-query-category "chips"))
     (stub mu4e-tagging-query-rerun)
     (mu4e-tagging--interceptor-query-require))))

(ert-deftest test-filter-update--require-flag-handler ()
  "The interceptor handler for require-filters works for flags."
  (should-preserve-all-state
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?p])
     (stub mu4e-tagging-query-rerun)
     (mock (mu4e-tagging-query-submode-enable))
     (mu4e-tagging--interceptor-query-require)
     (should (equal '(("particularly-lengthy" . +))
                    (mu4e-tagging--query-flags-alist))))
   ;; idempotent
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?p])
     (stub mu4e-tagging-query-rerun)
     (mock (mu4e-tagging-query-submode-enable))
     (mu4e-tagging--interceptor-query-require)
     (should (equal '(("particularly-lengthy" . +))
                    (mu4e-tagging--query-flags-alist))))
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?a])
     (stub mu4e-tagging-query-rerun)
     (mock (mu4e-tagging-query-submode-enable))
     (mu4e-tagging--interceptor-query-require)
     (should (equal '(("a" . +)
                      ("particularly-lengthy" . +))
                    (mu4e-tagging--query-flags-alist))))
   (mu4e-tagging-query-flags '("-a"
                               "-particularly-lengthy"))
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?p])
     (stub mu4e-tagging-query-rerun)
     (mock (mu4e-tagging-query-submode-enable))
     (mu4e-tagging--interceptor-query-require)
     (should (equal '(("a" . -))
                    (mu4e-tagging--query-flags-alist))))
   (with-mock
     ;; C-w = 23
     (stub this-command-keys-vector => [23 ?a])
     (stub mu4e-tagging-query-rerun)
     (mock (mu4e-tagging-query-submode-enable))
     (mu4e-tagging--interceptor-query-require)
     (should (equal nil
                    (mu4e-tagging--query-flags-alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap handling

(ert-deftest test-keymap--flag-tag ()
  "Key sequence for tagging flags"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-message-at-point =>  'my-test-message)
      (mock (mu4e-action-retag-message 'my-test-message "+a"))
      (execute-kbd-macro "a")
      ))))

(ert-deftest test-keymap--flag-untag ()
  "Key sequence for untagging flags"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-message-at-point =>  'my-test-message)
      (mock (mu4e-action-retag-message 'my-test-message "-a"))
      (execute-kbd-macro "-a")
      ))))

(ert-deftest test-keymap--category-tag ()
  "Key sequence for setting categories"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-message-at-point =>  'my-test-message)
      (mock (mu4e-action-retag-message 'my-test-message "+bassoon,-ba,-chips"))
      (execute-kbd-macro "b")
      ))))

(ert-deftest test-keymap--category-untag ()
  "Key sequence for removing categories"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-message-at-point =>  'my-test-message)
      (mock (mu4e-action-retag-message 'my-test-message "-bassoon"))
      (execute-kbd-macro "-b")))))

(ert-deftest test-keymap--flag-require-query ()
  "Key sequence for querying for presence of a flag"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wa"))
     (should (equal '(("a" . +))
                    (mu4e-tagging--query-flags-alist)))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wp")
      (should (equal '(("a" . +)
                       ("particularly-lengthy" . +))
                     (mu4e-tagging--query-flags-alist)))))
   ;; Clean up
   (clrhash mu4e-tagging-query-flags)))

(ert-deftest test-keymap--flag-block-query ()
  "Key sequence for querying for absence of a flag"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-w-a"))
     (should (equal '(("a" . -))
                    (mu4e-tagging--query-flags-alist)))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wp"))
     (should (equal '(("a" . -)
                      ("particularly-lengthy" . +))
                    (mu4e-tagging--query-flags-alist))))
   ;; Clean up
   (clrhash mu4e-tagging-query-flags)))

(ert-deftest test-keymap--flag-block-permit-require-query ()
  "Key sequence for first blocking, then iterating to requiring a flag"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-w-a"))
     (should (equal '(("a" . -))
                    (mu4e-tagging--query-flags-alist)))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wa"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wa"))
     (should (equal '(("a" . +))
                    (mu4e-tagging--query-flags-alist))))
   ;; Clean up
   (clrhash mu4e-tagging-query-flags)))

(ert-deftest test-keymap--flag-require-permit-block-query ()
  "Key sequence for first requiring a flag, then iterating to blocking a flag"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wa"))
     (should (equal '(("a" . +))
                    (mu4e-tagging--query-flags-alist)))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-w-a"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-w-a"))
     (should (equal '(("a" . -))
                    (mu4e-tagging--query-flags-alist))))
   ;; Clean up
   (clrhash mu4e-tagging-query-flags)))

(ert-deftest test-keymap--category-require-unrequire ()
  "First require a category, then remove that requirement again"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wb"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (should (equal "bassoon"
		    mu4e-tagging-query-category))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-w-b"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (should (equal nil
		    mu4e-tagging-query-category)))
   ;; Clean up
   (setq mu4e-tagging-query-category nil)))

(ert-deftest test-keymap--category-switch ()
  "First require a category, then switch categories"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wb"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (should (equal "bassoon"
		    mu4e-tagging-query-category))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-wc"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (should (equal "chips"
		    mu4e-tagging-query-category)))
   ;; Clean up
   (setq mu4e-tagging-query-category nil)))

(ert-deftest test-keymap--uncategorised ()
  "Require uncategorised mails only"
  (should-preserve-all-state
   ;; Override all keybindings, for testing
   (let ((overriding-terminal-local-map mu4e-tagging-mode-map))
     (with-mock
      (stub mu4e-tagging-query-submode-enable)
      (stub mu4e-tagging-query-rerun)
      (execute-kbd-macro "\C-w\C-d"))
     (should (equal nil
                    (mu4e-tagging--query-flags-alist)))
     (should (equal 'uncategorized
		    mu4e-tagging-query-category)))
   ;; Clean up
   (setq mu4e-tagging-query-category nil)))

;;; mu4e-tagging-test.el ends here
