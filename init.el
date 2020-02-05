(require 'org)
(require 'org-static-blog)

(setq org-static-blog-publish-title "x25519.net")
(setq org-static-blog-publish-url "https://blog.x25519.net/")
(setq org-static-blog-publish-directory "~/dev/blog.x25519.net/html/")
(setq org-static-blog-posts-directory "~/dev/blog.x25519.net/html/posts/")
(setq org-static-blog-drafts-directory "~/dev/blog.x25519.net/html/drafts/")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-static-blog-index-length 6)
(setq org-static-blog-enable-tags t)

(setq org-static-blog-page-header
"<meta  name=\"author\" content=\"dinosaure\" />
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

(setq org-static-blog-page-preamble
"<div class=\"header\">
  <a href=\"https://blog.x25519.net\">dinosaure's Blog</a>
</div>")

(setq org-static-blog-page-postamble
"<div id=\"archive\">
  <a href=\"archive.html\">archive</a>
</div>")

(defun org-static-blog-post-preamble (post-filename)
  (concat
   "<div class=\"headline\"> <h1 class=\"post-title\">"
   "<a href=\"" (org-static-blog-get-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>" "</h1>\n"
   (org-style-tags post-filename) "</div>"
   "<div class=\"post-date\">" (format-time-string "<%Y-%m-%d>" (org-static-blog-get-date post-filename)) "</div>"))

(defun org-style-tags (post-filename)
  (let ((taglist-content ""))
    (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
      (setq taglist-content (concat "<div class=\"taglist\">"
                                    ":"))
      (dolist (tag (org-static-blog-get-tags post-filename))
        (setq taglist-content (concat taglist-content "<a href=\""
                                      org-static-blog-publish-url
                                      "tag-" (downcase tag) ".html"
                                      "\">" tag "</a>:")))
      (setq taglist-content (concat taglist-content "</div>")))
    taglist-content))

(defun org-static-blog-post-postamble (post-filename)
      "")

(defun org-static-blog-get-post-summary (post-filename)
(concat
         "<div class=\"headline\"> <h2 class=\"post-title\">"
         "<a href=\"" (org-static-blog-get-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>" "</h2>\n"
         (org-style-tags post-filename) "</div>"
         "<div class=\"post-date\">" (format-time-string "<%Y-%m-%d>" (org-static-blog-get-date post-filename)) "</div>"))
