(require 'gh)
(defgroup issue-downloader nil
  "download posts in github issue based blog")

(defcustom issue-downloader-base-dir "/tmp"
  ""
  :group 'issue-downloader)

(defcustom issue-downloader-repos nil
  "specify blog repos. It should be a list of \"USER/REPO\" or (\"USER/REPO\" . download-directory)"
  :group 'issue-downloader)

(defun issue--count-file-in-dir (dir)
  "Count file in DIR"
  (length (directory-files dir nil "\.md$")))

(defun issue-downloader-download (user/repo &optional dir since)
  "Download issues of USER/REPO into DIR. If SINCE is specified (should be a integer), means only download issues newer than it"
  (unless (file-exists-p dir)
    (make-directory dir t))
  (let* ((user/repo-list (split-string user/repo "/"))
         (user (nth 0 user/repo-list))
         (repo (nth 1 user/repo-list))
         (dir (or dir (format "%s%s-%s" (file-name-as-directory issue-downloader-base-dir user repo))))
         (since (cond ((eq t since)
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
        (with-temp-file (format "%s/%s.md" dir title)
          (insert body))))))

;; (issue-downloader-download "lujun9972/blog" "/tmp/1" t)

;;;###autoload
(defun issue-downloader ()
  (mapc (lambda (x)
          (apply #'issue-downloader-download x))
        (issue-downloader-repos)))
