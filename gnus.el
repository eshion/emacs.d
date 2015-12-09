(require 'nnir)

;;@see http://www.emacswiki.org/emacs/GnusGmail#toc1
(setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups 

;; ask encyption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;@see http://gnus.org/manual/gnus_397.html
;(add-to-list 'gnus-secondary-select-methods
;             '(nnimap "qqmail"
;                      (nnimap-address "imap.qq.com")
;                      (nnimap-server-port 993)
;                      (nnimap-stream ssl)
;                      (nnir-search-engine imap)
;                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
;                      ;; press 'E' to expire email
;                      (nnmail-expiry-target "nnimap+qqmail:[QQMail]/Trash")
;                      (nnmail-expiry-wait 90)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "mucfc"
                      (nnimap-address "mail.mucfc.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+mucfc:[Mucfc]/Trash")
                      (nnmail-expiry-wait 90)))
;(add-to-list 'gnus-secondary-select-methods
;             '(nnimap "gmail"
;                      (nnimap-address "imap.gmail.com")
;                      (nnimap-server-port 993)
;                      (nnimap-stream ssl)
;                      (nnir-search-engine imap)
;                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
;                      ;; press 'E' to expire email
;                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
;                      (nnmail-expiry-wait 90)))

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

; NO 'passive
(setq gnus-use-cache t)

;; BBDB: Address list
;(add-to-list 'load-path "/where/you/place/bbdb/")
;(require 'bbdb)
;(bbdb-initialize 'message 'gnus 'sendmail)
;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;(setq bbdb/mail-auto-create-p t
;      bbdb/news-auto-create-p t)
;
;;; auto-complete emacs address using bbdb UI
;(add-hook 'message-mode-hook
;          '(lambda ()
;             (flyspell-mode t)
;             (local-set-key "<TAB>" 'bbdb-complete-name)))

;; Fetch only part of the article if we can.
;; I saw this in someone's .gnus
(setq gnus-read-active-file 'some)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; Personal Information
(setq user-full-name "zhangyusheng"
      user-mail-address "zhangyusheng@mucfc.com")

;; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
;(setq mm-text-html-renderer 'w3m)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "mail.mucfc.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")
;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
(setq gnus-use-correct-string-widths nil)
