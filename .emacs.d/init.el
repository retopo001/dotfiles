;;; init.el

;; gptel - LLM client
(require 'gptel)

;; Register Claude backend (set ANTHROPIC_API_KEY env var)
(gptel-make-anthropic "Claude" :stream t :key (getenv "ANTHROPIC_API_KEY"))

;; Set Claude as default
(setq gptel-model 'claude-sonnet-4-20250514
      gptel-backend (gptel-make-anthropic "Claude" :stream t :key (getenv "ANTHROPIC_API_KEY")))

;; Use Sonnet as default model
(setq gptel-model 'claude-sonnet-4-20250514)


;; opencode.el - agentic coding tools
(add-to-list 'load-path "~/.emacs.d/opencode.el")
(require 'opencode)
(opencode-setup-coding)

;; Google Search, not DuckDuckGo as default EWW search
(setq eww-search-prefix "https://www.google.com/search?q=")
(global-set-key (kbd "C-c w") 'eww)  ;; quick access


(desktop-save-mode 1)
(setq desktop-save t)              ; always save without asking
(setq desktop-load-locked-desktop t) ; don't ask about locked desktops
(setq desktop-restore-eager 5)     ; restore 5 buffers immediately, rest lazily




;; =============================================================================
;; Devdocs - auto-select docs per language
;; =============================================================================
(add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
(add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
(add-hook 'rust-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("rust"))))
(add-hook 'typescript-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("typescript" "node"))))
(add-hook 'js-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("javascript" "node"))))
(add-hook 'go-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("go"))))
(global-set-key (kbd "C-c d") 'devdocs-lookup)

;; =============================================================================
;; Better Defaults
;; =============================================================================
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-display-line-numbers-mode 1)
(add-hook 'vterm-mode-hook (lambda () (setq-local display-line-numbers nil)))
(column-number-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(setq show-paren-delay 0)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(global-auto-revert-mode 1)

;; =============================================================================
;; Completion
;; =============================================================================
(vertico-mode 1)
(marginalia-mode 1)
(setq completion-styles '(orderless basic))
(global-corfu-mode 1)
(setq corfu-auto t corfu-auto-delay 0.2 corfu-auto-prefix 2)

;; =============================================================================
;; Tree-sitter + file associations
;; =============================================================================
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")))

(when (treesit-available-p)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))

;; =============================================================================
;; Eglot (LSP)
;; =============================================================================
(require 'eglot)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)

;; =============================================================================
;; Which-key
;; =============================================================================
(which-key-mode 1)
(setq which-key-idle-delay 0.5)

;; Update which-key when switching windows/buffers (context follows point)
(dolist (hook '(window-buffer-change-functions
                window-selection-change-functions))
  (add-hook hook (lambda (&rest _) (which-key--update))))

;; =============================================================================
;; Keep backups/autosaves out of working directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-saves" t)



;;; init.el ends here
