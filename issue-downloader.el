;;; issue-downloader.el --- download github issue

;; Copyright (C) 2004-2016 DarkSun <lujun9972@gmail.com>

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-03-24
;; Version: 0.1
;; Keywords: convenience, github, org
;; Package-Requires: ((org "8.0") (emacs "24.4") (ox-gfm "0.1") (gh "0.1"))
;; URL: https://github.com/lujun9972/issue-downloader

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; issue-downloader's code can be found here:
;;   http://github.com/lujun9972/issue-downloader

;;; Commentary:

;; issue-downloader is a little tool that used to download github issues

;; Quick start:

;; 1. specify ~issue-downloader-repos~ which specifies which repos to download issues and where to store those issues
;; 2. execute =M-x issue-downloader=

;;; Code:
(require 'gh)
(defgroup issue-downloader nil
  "download posts in github issue based blog")

(defcustom issue-downloader-base-dir "~/issue-blog"
  ""
  :group 'issue-downloader)

(defcustom issue-downloader-repos nil
  "specify blog repos. It should be a list of (\"USER/REPO\") or (\"USER/REPO\" download-directory)"
  :group 'issue-downloader)

(defun issue--count-file-in-dir (dir)
  "Count file in DIR,Will create DIR if not exist"
  (unless (file-exists-p dir)
    (make-directory dir t))
  (length (directory-files dir nil "\.md$")))

(defun issue-downloader-download (user/repo &optional dir since)
  "Download issues of USER/REPO into DIR. If SINCE is specified (should be a integer), means only download issues newer than it"
  (let* ((user/repo-list (split-string user/repo "/"))
         (user (nth 0 user/repo-list))
         (repo (nth 1 user/repo-list))
         (dir (or dir (format "%s%s-%s" (file-name-as-directory issue-downloader-base-dir) user repo)))
         (since (cond ((or (eq t since)
                           (eq nil since))
                       (issue--count-file-in-dir dir))
                      ((stringp since)
                       (string-to-number since))
                      ((numberp since)
                       since)
                      (t 0)))
         (api (gh-issues-api "api"))
         (issues (oref (gh-issues-issue-list api user repo) data))
         (valid-issues (remove-if-not (lambda (issue)
                                        (> (oref issue number) since))
                                      issues)))
    (dolist (issue (reverse valid-issues))
      (let ((title (oref issue title))
            (body (oref issue body)))
        (with-temp-file (format "%s/%s.md" dir (replace-regexp-in-string "[\\/]" "_" title))
          (insert body))))))


;; (issue-downloader-download "CodeFalling/blog")

;;;###autoload
(defun issue-downloader ()
  (interactive)
  (mapc (lambda (x)
          (apply #'issue-downloader-download x))
        (issue-downloader-repos)))

;;; org2issue.el ends here
