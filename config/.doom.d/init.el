;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company           ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;(spell +flyspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       ;;lsp
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; the accounting system in Emacs
       ;;cc                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;ledger            ; an other accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       org               ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))


 (with-eval-after-load 'org

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


   (use-package org-super-agenda
     :config (org-super-agenda-mode)

     (setq org-super-agenda-groups

             '(
               (:log t)  ; Automatically named "Log"
               (:name "NEW"
                      :todo "NEW")
               (:name "OVERDUE"
                      :deadline past)
               (:name "DOING"
                      :todo "DOING")
               (:name "NOW"
                      :deadline today
                      :todo "NOW")
               (:name "SOON"
                      :deadline future
                      :todo "SOON")
               (:name "QA"
                      :todo "QA") ; Items that have this TODO keyword
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
   )





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
