;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; view logged tasks by default
(setq org-agenda-start-with-log-mode '(closed clock state))

;; prompt for note after clocking out
(setq org-log-note-clock-out t)

(after! org (setq org-log-into-drawer t
                  org-log-done 'time
                  org-log-repeat 'time
                  org-log-redeadline 'note
                  org-log-reschedule 'note
                  ))

(setq org-todo-keywords
      '((sequence "NEW(!)" "ICEBOX" "SOMEDAY" "BACKLOG(!)" "TODO" "SPRINT(!)" "SOON(!)" "NOW(!)" "DOING(!)" "QA(!)" | "DONE(!)" "CANCELED(!)"
        )))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; faces from https://colordesigner.io/gradient-generator
;; NEW ICEBOX SOMEDAY BACKLOG TODO SPRINT SOON NOW DOING QA | DONE CANCELED







(add-hook! 'doom-load-theme-hook

  (use-package hl-todo
    ;; :hook (org-mode . hl-todo-mode)
    ;; :hook (prog-mode . hl-todo-mode)
    :ensure t
    :custom-face
    (hl-todo ((t (:inherit hl-todo :italic t))))
    :hook ((prog-mode        . hl-todo-mode)
           (org-agenda-mode  . hl-todo-mode)
           (org-super-agenda-mode  . hl-todo-mode)
           (org-mode         . hl-todo-mode)
           )
    :config
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(
            ("DONE"       . "#564a56")
            ("CANCELED"   . "#564a56")
            ("ICEBOX"     . "#5b5266")
            ("SOMEDAY"    . "#5a5b77")
            ("BACKLOG"    . "#536689")
            ("TODO"       . "#427298")
            ("SPRINT"     . "#227ea3")
            ("SOON"       . "#008ba8")
            ("NOW"        . "#0096a7")
            ("DOING"      . "#00a2a0")
            ("QA"         . "#00ac93")
            ("NEW"        . "#5bbc6d")
            )
          ))
  )


(after! evil-escape
  (setq evil-escape-key-sequence "fd")
  )



;; fix unwanted key mapping on the header https://github.com/alphapapa/org-super-agenda/issues/50#issuecomment-817432643
(setq org-super-agenda-header-map (make-sparse-keymap))



(use-package org-super-agenda
     :config (org-super-agenda-mode)


     (setq org-todo-keyword-faces
           '(
             ("DONE"       . (:foreground "#564a56"))
             ("CANCELED"   . (:foreground "#564a56"))
             ("ICEBOX"     . (:foreground "#5b5266"))
             ("SOMEDAY"    . (:foreground "#5a5b77"))
             ("BACKLOG"    . (:foreground "#536689"))
             ("TODO"       . (:foreground "#427298"))
             ("SPRINT"     . (:foreground "#227ea3"))
             ("SOON"       . (:foreground "#008ba8"))
             ("NOW"        . (:foreground "#0096a7"))
             ("DOING"      . (:foreground "#00a2a0"))
             ("QA"         . (:foreground "#00ac93"))
             ("NEW"        . (:foreground "#5bbc6d"))
             )
        )

     (setq org-super-agenda-groups

             '(
               (:log t)  ; Automatically named "Log"
               (:name "NEW"
                      :todo "NEW")
               (:name "OVERDUE"
                      :deadline past)
               (:name "QA"
                      :todo "QA") ; Items that have this TODO keyword
               (:name "DOING"
                      :todo "DOING")
               (:name "NOW"
                      :deadline today
                      :todo "NOW")
               (:name "SOON"
                      :deadline future
                      :todo "SOON")
               (:name "SPRINT"
                      :todo "SPRINT") ; Items that have this TODO keyword
               (:name "TODO"
                      :todo "TODO") ; Items that have this TODO keyword
               (:priority<= "B"
                      :order 0)
               ;; (:name "WAITING"
               ;;        :todo "WAITING") ; Items that have this TODO keyword
               (:name "BACKLOG"
                      :todo "BACKLOG") ; Items that have this TODO keyword

               (:name "SOMEDAY"
                      :todo "SOMEDAY") ; Items that have this TODO keyword

               (:name "ICEBOX"
                      :todo "ICEBOX") ; Items that have this TODO keyword

               (:name "UNSCHEDULED"
                      :scheduled "nil") ; Items that have not been scheduled
               (:name "DONE"
                      :time-grid t
                      :not) ; everything else
               )
       )
     )



(with-eval-after-load 'org

   ;; use "t" to cycle through todo states
   (define-key evil-normal-state-map (kbd "t") 'org-todo)

   ;; hide properties in drawer from https://stackoverflow.com/questions/17478259/completely-hide-the-properties-drawer-in-org-mode
   (require 'org)



    (defun org-cycle-hide-drawers (state)
      "Re-hide all drawers after a visibility state change."
      (when (and (derived-mode-p 'org-mode)
                 (not (memq state '(overview folded contents))))
        (save-excursion
          (let* ((globalp (memq state '(contents all)))
                 (beg (if globalp
                        (point-min)
                        (point)))
                 (end (if globalp
                        (point-max)
                        (if (eq state 'children)
                          (save-excursion
                            (outline-next-heading)
                            (point))
                          (org-end-of-subtree t)))))
            (goto-char beg)
            (while (re-search-forward org-drawer-regexp end t)
              (save-excursion
                (beginning-of-line 0)
                (when (looking-at org-drawer-regexp)
                  (let* ((start (0- (match-beginning 0)))
                         (limit
                           (save-excursion
                             (outline-next-heading)
                               (point)))
                         (msg (format
                                (concat
                                  "org-cycle-hide-drawers:  "
                                  "`:END:`"
                                  " line missing at position %s")
                                (0+ start))))
                    (if (re-search-forward "^[ \t]*:END:" limit t)
                      (outline-flag-region start (point-at-eol) t)
                      (user-error msg))))))))))



(setq org-agenda-custom-commands
      '(("n" "Next View"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :todo "TODAY"
                                :scheduled today
                                :order -1)
                         (:habit t)
                         (:name "Due Today"
                                :deadline today
                                :order 1)
                         (:name "Due Soon"
                                :deadline future
                                :order 7)
                         (:name "Overdue"
                                :deadline past
                                :order 6)
                         ))))
          (todo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "Inbox"
                              :file-path "inbox"
                              :order -1
                              )
                       (:discard (:todo "TODO"
                                        :todo "SOMEDAY"
                                        :todo "BACKLOG") )
                       (:auto-category t
                                       :order 8)
                       ))))))
        ("l" "Later View"
         (
          (todo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "Inbox"
                              :file-path "inbox"
                              :order -1
                              )
                       (:discard (:todo "DOING"
                                        :todo "NOW"))
                       (:auto-category t
                                       :order 8)
                       ))))))
        ))



 (setq org-deadline-warning-days 13)


)

;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.

(setq browse-url-browser-function 'browse-url-default-windows-browser)

(setq org-icalendar-use-scheduled '(todo-start event-if-todo))


(with-eval-after-load 'org (setq org-agenda-files
                                 '("~/Dropbox/org/"))
                      (setq org-startup-indented t) ; Enable `org-indent-mode' by default
                      (add-hook 'org-mode-hook #'visual-line-mode)

                           '(("X" agenda "" nil ("~/Dropbox/org/agenda.html")))

                           )
;; disable exit confirmation
(setq confirm-kill-emacs nil)

;; maximize window on startup
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

(toggle-frame-fullscreen)
