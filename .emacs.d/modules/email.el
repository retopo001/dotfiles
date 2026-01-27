;;; email.el --- mu4e email config -*- lexical-binding: t; -*-
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :config
  (setq mu4e-maildir "~/Mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        message-kill-buffer-on-exit t
        mu4e-headers-date-format "%Y-%m-%d"
        mu4e-headers-time-format "%H:%M"
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:from . 22)
                              (:subject)))

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "brianhuaidesign@gmail.com")
                  (user-full-name . "Brian Huai")
                  (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                  (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")))))

  (setq mu4e-context-policy 'pick-first)

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials "~/.authinfo"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls))
