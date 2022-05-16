;;; package --- schemar emacs early
;;; Commentary:
;;

;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; From straight's documentation:
;; Users of Emacs versions >= 27 will want to add:
(setq package-enable-at-startup nil)

;;; early-init.el ends here
