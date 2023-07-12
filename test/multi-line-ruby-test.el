;;; multi-line-ruby-test.el --- multi-line ruby tests -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2023 Ivan Malison

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test the behavior of multi-line in ruby-mode

;;; Code:

(require 'multi-line)
(require 'multi-line-test)

(multi-line-deftest-for-mode ruby test-ruby-hash-literal
"{:avalue => \"cool\", :greater => \"fun\", :avalue3 => \"cool\", :greaterg => \"fun\", :more => \"coool real long\",}"
"{
  :avalue => \"cool\", :greater => \"fun\", :avalue3 => \"cool\", :greaterg => \"fun\",
  :more => \"coool real long\",
}"
:setup ((search-forward "a")))

(provide 'multi-line-ruby-test)
;;; multi-line-ruby-test.el ends here
