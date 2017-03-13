;;; elpass.el --- LastPass command wrapper -*- lexical-binding: t -*-

;; Copyright © 2017

;; Author: Petter Storvik
;; URL: https://github.com/storvik/elpass
;; Version: 0.1.0
;; Created: 2017-02-17
;; Package-Requires: ((emacs "24.4"))
;; Keywords: extensions processes lpass lastpass elpass

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains a wrapper for the LastPass command line utility
;; lpass.
;;
;; Several functions used for interacting with lpass in Emacs are
;; made available to the user through the "M-x" interface.
;; Such functions are:
;; - `elpass-login'
;; - `elpass-logout'
;; - `elpass-status'
;; - `elpass-getpass'
;; - `elpass-addpass'
;; These functions can also used in elisp when configuring Emacs.
;;
;; A lpass manager is available by running `elpass-list-all'.
;; This function will list all passwords and a major mode takes care of
;; setting som neat keybindings to some neat functions.  All these functions
;; are shown in the lpass buffer, and is self-explanatory.
;;
;; For more information, see the readme at https://github.com/storvik/elpass

;;; Code:

(require 'tree-widget)

(defgroup elpass nil
  "LastPass functions and settings."
  :group 'external
  :tag "elpass"
  :prefix "elpass-")

(defcustom elpass-user ""
  "LastPass user e-mail."
  :type 'string
  :group 'elpass)

(defcustom elpass-location "/usr/local/bin/"
  "Lastpass command line location."
  :type 'string
  :group 'elpass)

(defcustom elpass-shell "/bin/bash"
  "Shell to be used when running LastPass commands."
  :type 'string
  :group 'elpass)

(defcustom elpass-pass-length 12
  "Default password length when generating passwords."
  :type 'integer
  :group 'elpass)

(defcustom elpass-pass-no-symbols nil
  "Use symbols when generating passwords."
  :type 'boolean
  :group 'elpass)

(defvar elpass-group-completion '()
  "List containing groups.  Gets updated on `elpass-list-all'.")

(defun elpass-runcmd (cmd &rest args)
  "Run lpass command CMD with ARGS."
  (with-temp-buffer
    (list (apply 'call-process (concat elpass-location "lpass") nil (current-buffer) nil (cons cmd args))
          (replace-regexp-in-string "\n$" ""
                                    (buffer-string)))))

(defun elpass-pipe-to-cmd (cmd prepend &rest args)
  "Run lpass command CMD, piping PREPEND and appending ARGS.
Can for example be used with lpass add and the following prepended string:
Username: testuser\nPassword: testpassword.  Returns a list with status code
and returned string from lpass command."
  (with-temp-buffer
    (let ((command (concat "printf \"" prepend "\"" " | " elpass-location "lpass " (mapconcat 'identity (cons cmd args) " ") " --non-interactive")))
      (list (apply 'call-process-shell-command command nil (current-buffer) nil)
            (replace-regexp-in-string "\n$" "" (buffer-string))))))

(defun elpass-list-all-reload ()
  "Reload `elpass-list-all' by killing *elpass-list* and reopening."
  (interactive)
  (when (string-match (buffer-name) "*elpass-list*")
    (kill-buffer "*elpass-list*")
    (elpass-list-all)))

;;;###autoload
(defun elpass-login ()
  "Prompts user for password if not logged in."
  (interactive)
  (unless (equal (nth 0 (elpass-runcmd "status")) 1)
    (error "LastPass: Already logged in"))
  (when (get-process "elpass")
    (delete-process "elpass"))
  (let ((process (start-process-shell-command
                  "elpass"
                  nil
                  (concat "LPASS_DISABLE_PINENTRY=1 "
                          elpass-shell
                          " -c '"
                          elpass-location
                          "lpass login "
                          elpass-user
                          "'"))))
    (set-process-filter
     process
     (lambda (proc string)
       (when (string-match "password" string)
         (process-send-string
          proc
          (if (string-match "invalid" string)
              (concat (read-passwd "Wrong password. LastPass master password: ") "\n")
            (concat (read-passwd "LastPass master password: ") "\n"))))
       (when (string-match "success" string)
         (message (concat "LastPass: Successfully logged in as " elpass-user)))))))

;;;###autoload
(defun elpass-status ()
  "Check LastPass status, if user is logged in or not."
  (interactive)
  (let ((ret (elpass-runcmd "status")))
    (message "LastPass status: %s" (nth 1 ret))))

(defun elpass-logged-in ()
  "Check if `elpass-user' is logged in to LastPass.
Returns nil if not logged in."
  (let ((ret (elpass-runcmd "status")))
    (and (equal (nth 0 ret) 0)
         (string-match elpass-user (nth 1 ret)))))

;;;###autoload
(defun elpass-logout ()
  "Log out of lpass.  Does not ask for confirmation."
  (interactive)
  (unless (equal (nth 0 (elpass-runcmd "status")) 0)
    (error "LastPass: Not logged in, no need to log out"))
  (unless  (equal (nth 0 (elpass-runcmd "logout" "--force")) 0)
    (error "LastPass: Something went wrong, could not log out"))
  (message "LastPass: Successfully logged out."))

;;;###autoload
(defun elpass-getpass (account &optional print-message)
  "Get password associated with ACCOUNT.
If run interactively PRINT-MESSAGE gets set and password is printed to minibuffer."
  (interactive "MLastPass account name: \np")
  (unless (equal (nth 0 (elpass-runcmd "status")) 0)
    (error "LastPass: Not logged in"))
  (let ((ret (elpass-runcmd "show" "--password" account)))
    (if (equal (nth 0 ret) 0)
        (progn
          (when print-message
            (message "LastPass: Password for account %s is: %s" account (nth 1 ret)))
          (nth 1 ret))
      (message "LastPass: Something went wrong, could not get password."))))

;;;###autoload
(defun elpass-addpass (account user password url group)
  "Add account ACCOUNT with USER and PASSWORD to LastPass.
Optionally URL and GROUP can be set to nil."
  (interactive
   (list
    (read-string "Account name:")
    (read-string "User:")
    (read-string "Password(leave blank to generate):")
    (read-string "URL:")
    (completing-read "Group:" elpass-group-completion nil nil)))
  (unless (equal (nth 0 (elpass-runcmd "status")) 0)
    (error "LastPass: Not logged in"))
  (if (and password
           (> (length password) 0))
      ;; Add account without generating password
      (let ((inputstr (concat "Username: " user
                              "\nPassword: " password)))
        (when (and url
                   (> (length url) 0))
          (setq inputstr (concat inputstr "\nURL: " url)))
        (when (and group
                   (> (length group) 0))
          (setq account (concat group "/" account))
          (setq account (replace-regexp-in-string " " "\\ " account t t)))
        (unless (equal (nth 0 (elpass-pipe-to-cmd "add" inputstr account)) 0)
          (error "LastPass: Could not add account")))
    ;; Add account and generate password
    (let ((arguments (list (number-to-string elpass-pass-length))))
      (when (and group
                 (> (length group) 0))
        (setq account (concat group "/" account)))
      (when (and url
                 (push account arguments)
                 (> (length url) 0))
        (push (concat "--url=" url) arguments))
      (push (concat "--username=" user) arguments)
      (when elpass-pass-no-symbols
        (push "--no-symbols" arguments))
      (push "generate" arguments)
      (apply 'elpass-runcmd arguments)))
  (message "LastPass: Account \"%s\" added" account)
  (elpass-list-all-reload))

;; lpass-list-dialog-mode
(defvar elpass-list-dialog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "r" 'elpass-list-all-reload)
    (define-key map "a" 'elpass-addpass)
    (define-key map "s" 'elpass-list-all-getpass)
    (define-key map "w" 'elpass-list-all-kill-ring-save)
    (define-key map "m" 'elpass-list-all-movepass)
    (define-key map "d" 'elpass-list-all-deletepass)
    (define-key map "q" 'elpass-list-cancel-dialog)
    map)
  "Keymap used in recentf dialogs.")

(define-derived-mode elpass-list-dialog-mode nil "elpass-list-dialog"
  "Major mode of recentf dialogs.

\\{elpass-list-dialog-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defmacro elpass-list-dialog (name &rest forms)
  "Show a dialog buffer with NAME, setup with FORMS."
  (declare (indent 1) (debug t))
  `(with-current-buffer (get-buffer-create ,name)
     ;; Cleanup buffer
     (let ((inhibit-read-only t)
           (ol (overlay-lists)))
       (mapc 'delete-overlay (car ol))
       (mapc 'delete-overlay (cdr ol))
       (erase-buffer))
     (elpass-list-dialog-mode)
     ,@forms
     (widget-setup)
     (switch-to-buffer (current-buffer))))

;; Dialog settings and actions
(defun elpass-list-cancel-dialog (&rest _ignore)
  "Cancel the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Dialog canceled"))

(defun elpass-list-all-item-action (widget &rest _ignore)
  "Do action to element associated with WIDGET's value.
IGNORE other arguments."
  ;;(kill-buffer (current-buffer))
  (funcall 'message "Item pressed: %s" (widget-value widget)))

(defsubst elpass-list-all-get-element-id ()
  "Get id from line in dialog widget."
  (let ((line (thing-at-point 'line t)))
    (with-temp-buffer
      (insert line)
      (goto-char (point-max))
      (backward-word)
      (re-search-forward "\\([0-9]+\\)")
      (match-string 0))))

(defun elpass-list-all-getpass ()
  "Display current items password in minibuffer.
As it uses message to print the password, it will be visible in the *Messages* buffer."
  (interactive)
  (message "Password: %s" (elpass-getpass (elpass-list-all-get-element-id))))

(defun elpass-list-all-kill-ring-save ()
  "LastPass `kill-ring-save', insert password to kill ring."
  (interactive)
  (kill-new (elpass-getpass (elpass-list-all-get-element-id)))
  (message "Password added to kill ring"))

(defun elpass-list-all-deletepass ()
  "Delete account from LastPass."
  (interactive)
  (let ((id (elpass-list-all-get-element-id)))
    (when (y-or-n-p (concat "LastPass: Delete "
                            id
                            "? "))
      (unless  (equal (nth 0 (elpass-runcmd "rm" id)) 0)
        (error "LastPass: Something went wrong, could not delete account"))
      (message "LastPass: Successfully deleted account, updating list."))))

(defun elpass-list-all-movepass ()
  "Move password to group."
  (interactive)
  (let ((id (elpass-list-all-get-element-id)))
    (let ((group (completing-read (concat "Move item " id " to group: ") elpass-group-completion nil nil)))
      (unless  (equal (nth 0 (elpass-runcmd "mv" id group)) 0)
        (error "LastPass: Something went wrong, could not move account to group"))
      (message "LastPass: Successfully moved account, updating list.")))
  (elpass-list-all-reload))

(defsubst elpass-list-all-make-spaces (spaces)
  "Create a string with SPACES number of whitespaces."
  (mapconcat 'identity (make-list spaces " ") ""))

(defsubst elpass-list-all-make-element (item)
  "Create a new widget element from ITEM.
Also update the `elpass-group-completion' variable by adding groups to list."
  (cons (concat
         ;;(with-temp-buffer
         ;;  (insert item)
         ;;  (goto-char (point-min))
         ;;  (re-search-forward "\\([0-9-]+\\s-[0-9:]+\\)")
         ;;  (match-string 0))
         ;;(elpass-list-all-make-spaces 4)
         (let ((str (replace-regexp-in-string "/" ""
                                              (with-temp-buffer
                                                (insert item)
                                                (goto-char (point-min))
                                                (re-search-forward "\\([a-z()]+\\s-\\)*\\([a-z()]+\\)?/")
                                                (match-string 0)))))
           (add-to-list 'elpass-group-completion str)
           (concat str (elpass-list-all-make-spaces (- 24 (length str)))))
         (let ((str (replace-regexp-in-string "\\(/\\|\\s-\\[id\\)" ""
                                              (with-temp-buffer
                                                (insert item)
                                                (goto-char (point-min))
                                                (re-search-forward "/\\(.+\\)\\(\\s-\\[id\\)")
                                                (match-string 0)))))
           (concat str (elpass-list-all-make-spaces (- 30 (length str)))))
         (let ((str (replace-regexp-in-string "\\(username:\\s-\\|]\\)" ""
                                              (with-temp-buffer
                                                (insert item)
                                                (goto-char (point-min))
                                                (re-search-forward "\\(username:\\s-\\)\\(.+\\|\\)\\(]\\)")
                                                (match-string 0)))))
           (concat str (elpass-list-all-make-spaces (- 30 (length str)))))
         (let ((str (replace-regexp-in-string "\\(id:\\s-\\|]\\)" ""
                                              (with-temp-buffer
                                                (insert item)
                                                (goto-char (point-min))
                                                (re-search-forward "\\(id:\\s-\\)\\([0-9]+\\|\\)\\(]\\)")
                                                (match-string 0)))))
           (concat str))) ;; (lpass-list-all-make-spaces (- 24 (length str))))))
        (replace-regexp-in-string ".+id: " "" (replace-regexp-in-string "].+" "" item))))

(defun elpass-list-all-item (pass-element)
  "Return a widget to display PASS-ELEMENT in a dialog buffer."
  (if (consp (cdr pass-element))
      ;; Represent a sub-menu with a tree widget
      `(tree-widget
        :open t
        :match ignore
        :node (item :tag ,(car pass-element)
                    :sample-face bold
                    :format "%{%t%}:\n")
        ,@(mapcar 'lpass-list-all-item
                  (cdr pass-element)))
    ;; Represent a single file with a link widget
    `(link :tag ,(car pass-element)
           :button-prefix ""
           :button-suffix ""
           :button-face default
           :format "%[%t\n%]"
           :help-echo ,(concat "Viewing item " (cdr pass-element))
           :action elpass-list-all-item-action
           ;; Override the (problematic) follow-link property of the
           ;; `link' widget (bug#22434).
           :follow-link nil
           ,(cdr pass-element))))

(defun elpass-list-all-items (items)
  "Return a list of widgets to display ITEMS in a dialog buffer."
  (mapcar 'elpass-list-all-item
          ;;TODO: Add headers over list. Think append and concat should be used for this.
          (mapcar 'elpass-list-all-make-element
                  items)))

;;;###autoload
(defun elpass-list-all (&optional group)
  "Show a dialog, listing all entries associated with `elpass-user'.
If optional argument GROUP is given, only entries in GROUP will be listed."
  (interactive)
  (unless (equal (nth 0 (elpass-runcmd "status")) 0)
    (error "LastPass: Not logged in.  Log in with lpass-login to continue"))
  (mapc
   (lambda (x)
     (delete x elpass-group-completion))
   elpass-group-completion)
  (elpass-list-dialog "*elpass-list*"
    (widget-insert (concat "LastPass list mode.\n"
                           "Usage:\n"
                           "\tn next line\n"
                           "\tp previous line\n"
                           "\tr reload accounts\n"
                           "\ta add password\n"
                           "\ts show password\n"
                           "\tw add password to kill ring\n"
                           "\tm move account to group\n"
                           "\td delete account\n"
                           "\tq quit\n"))
    ;; Use a L&F that looks like the recentf menu.
    (tree-widget-set-theme "folder")
    (apply 'widget-create
           `(group
             :indent 0
             :format "\n%v\n"
             ,@(elpass-list-all-items (split-string (message (nth 1 (if (not group)
                                                                        (elpass-runcmd "ls" "--long")
                                                                      (elpass-runcmd "ls" "--long" group))))
                                                    "\\(\r\n\\|[\n\r]\\)"))))
    (widget-create
     'push-button
     :notify 'elpass-list-cancel-dialog
     "Cancel")
    (goto-char (point-min))))

(provide 'elpass)
;;; elpass.el ends here
