;;; lastpass-test.el --- Tests for lastpass

;; Copyright © 2017

;; Author: Petter Storvik

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
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'lastpass)
(message "Emacs version: %s" emacs-version)

(ert-deftest spaces()
  (should (equal (lastpass-list-all-make-spaces 3) "   "))
  (should (equal (lastpass-list-all-make-spaces 6) "      "))
  (should (equal (lastpass-list-all-make-spaces 9) "         ")))

(ert-deftest list-all-parse-element()
  (should (equal (nth 0 (lastpass-list-all-make-element "123456789123456789,Account Name,Group Name,User Name"))
                 "123456789123456789      Account Name            Group Name              User Name"))
  (should (equal (nth 0 (lastpass-list-all-make-element "123456789123456789,!#$%&/(),Group,mail@foobar.no"))
                 "123456789123456789      !#$%&/()                Group                   mail@foobar.no"))
  (should (equal (nth 0 (lastpass-list-all-make-element "123456789123456789,,,Usertest123456"))
                 "123456789123456789                                                      Usertest123456")))


(provide 'lastpass-test)
;;; lastpass-test.el ends here
