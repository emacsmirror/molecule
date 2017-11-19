;;; molecule.el --- Simple wrapper for molecule -*- lexical-binding: t -*-

;; Copyright:: Copyright (c) 2017, drymer

;; Author:: drymer <drymer [ AT ] autistici.org>
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1

;; Keywords:: languages terminals
;; Homepage: https://git.daemons.it/drymer/molecule.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or (at
;; your option) any later version.
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; If you find a bug, you may send an e-mail to drymer [ AT ] autistici.org
;; or open an issue at https://git.daemons.it/drymer/molecule.el

;;; Code:
(defgroup molecule nil
  "Wrapper around molecule command."
  :group 'tools)

(defcustom molecule-command "molecule"
  "The molecule command (no shit, Sherlock).  It shouldn't be necessary to cha\
nge it if it's on the PATH."
  :type 'variable
  :group 'molecule)

(defvar molecule-buffer-name-v "*molecule*"
  "Molecule buffer name.  It shouldn't be necessary to modify it.")

(defcustom molecule-debug-v nil
  "If set to t, it will use the --debug flag."
  :type 'variable
  :group 'molecule)

(defvar molecule-version-v nil
  "Molecule version.  Do not modify manually.")

;;;###autoload
(defun molecule-init ()
  "Execute molecule init."
  (interactive)
  (let ((role-name)
	(scenario-name)
	(molecule-parameter)
	(debug))
    ;; Set debug
    (if (eq molecule-debug-v t)
	(setq debug "--debug ")
      (setq debug ""))
    ;; Choose scenario or role
    (setq molecule-parameter
	  (ivy-read "Choose between \"role\" or \"scenario\": " (list
								 "scenario"
								 "role")))
    ;; If scenario, choose scenario-name and set role-name automatically
    (if (string= molecule-parameter "scenario")
	(progn
	  (setq role-name (concat " -r " (file-name-nondirectory
					  (directory-file-name
					   (file-name-directory
					    default-directory)))))
	  (setq scenario-name
		(read-string "Scenario name (leave it empty to use the \"defau\
lt\"): "))
	  (if (string= scenario-name "")
	      (setq scenario-name "default")))
      (setq role-name (concat " -r " (read-string "Role name: "))))
    (setq molecule-parameter (concat molecule-parameter " "))
    (if (boundp 'scenario-name)
	(setq scenario-name "")
      (setq scenario-name (concat " -s" scenario-name)))
    ;; Execute molecule
    (start-process molecule-buffer-name-v molecule-buffer-name-v "sh" "-c"
		   (concat
		    molecule-command
		    " init "
		    debug
		    molecule-parameter
		    scenario-name
		    role-name))
    (message "Molecule finished!")))

(defun molecule-basedir (directory)
  "Molecule function which helps to manage directories names.
Argument DIRECTORY A directory path."
  (file-name-directory (replace-regexp-in-string
			(concat
			 (file-name-nondirectory
			  (directory-file-name
			   (file-name-directory directory)
			   ))
			 "/$")
			""
			directory)))

(defun molecule--wrapper (command)
  "Molecule generic `COMMAND' wrapper."
  (let ((debug)
	(scenario)
	(molecule-dir)
	(scenarios)
	(old-dir))
    ;; Set debug
    (if (eq molecule-debug-v t)
	(setq debug "--debug ")
      (setq debug ""))
    ;; Search the molecule directory until two parent directories
    (if (string= (substring default-directory -6 -1) "tests")
	(progn
	  (setq old-dir default-directory)
	  (setq default-directory (substring default-directory 0 -23)))
      (progn
	(molecule-basedir default-directory)
	(if (not (file-directory-p "molecule"))
	    (progn
	      (setq molecule-dir (molecule-basedir default-directory))
	      (if (not (file-directory-p (concat molecule-dir "molecule")))
		  (progn
		    (setq molecule-dir (molecule-basedir molecule-dir))
		    (if (not (file-directory-p (concat molecule-dir "molecule")))
			(user-error "There's no molecule directory! You should\
 execute M-x molecule-init")
		      (progn
			(setq old-dir default-directory)
			(setq default-dirrectory
			      (concat molecule-dir "molecule"))))))
	      (progn
		(setq old-dir default-directory)
		(setq default-directory molecule-dir)))
	  (setq old-dir default-directory))))
    ;; Exclude . and ..
    (setq scenarios (directory-files (expand-file-name
				      (concat default-directory "molecule"))
				     nil
				     "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
    ;; If there's more than one scenario, give an option to choose them
    (if (> (length scenarios) 1)
	(progn
	  (setq scenarios (cons "all" scenarios))
	  ;; TODO: hacer el ivy-read generico y reusable al menos con ido
	  (setq scenario (concat " -s " (ivy-read "Choose a scenario: "
						  scenarios))))
      (if (= (length scenarios) 1)
	  (setq scenario "")
	(user-error "There's no scenarios! You should execute M-x molecule-ini\
t")))
    (compile (concat molecule-command " " debug command scenario))
    (setq default-directory old-dir)))

;;;###autoload
(defalias 'molecule-check (lambda()
			    "Execute molecule converge."
			    (interactive)
			    (funcall 'molecule--wrapper "check")))

;;;###autoload
(defalias 'molecule-converge (lambda()
			       "Execute molecule converge."
			       (interactive)
			       (funcall 'molecule--wrapper "converge")))

;;;###autoload
(defalias 'molecule-create (lambda()
			     "Execute molecule create."
			     (interactive)
			     (funcall 'molecule--wrapper "create")))

;;;###autoload
(defalias 'molecule-dependency (lambda()
				 "Execute molecule dependency."
				 (interactive)
				 (funcall 'molecule--wrapper "dependency")))

;;;###autoload
(defalias 'molecule-destroy (lambda()
			      "Execute molecule destroy."
			      (interactive)
			      (funcall 'molecule--wrapper "destroy")))

;;;###autoload
(defalias 'molecule-idempotence (lambda()
				  "Execute molecule idempotence."
				  (interactive)
				  (funcall 'molecule--wrapper "idempotence")))

;;;###autoload
(defalias 'molecule-lint (lambda()
			   "Execute molecule lint."
			   (interactive)
			   (funcall 'molecule--wrapper "lint")))

;;;###autoload
(defalias 'molecule-list (lambda()
			   "Execute molecule list."
			   (interactive)
			   (funcall 'molecule--wrapper "list")))

;;;###autoload
(defalias 'molecule-side-effect (lambda()
				  "Execute molecule side."
				  (interactive)
				  (funcall 'molecule--wrapper "side-effect")))

;;;###autoload
(defalias 'molecule-syntax (lambda()
			     "Execute molecule syntax."
			     (interactive)
			     (funcall 'molecule--wrapper "syntax")))

;;;###autoload
(defalias 'molecule-test (lambda()
			   "Execute molecule test."
			   (interactive)
			   (funcall 'molecule--wrapper "test")))

;;;###autoload
(defalias 'molecule-verify (lambda()
			     "Execute molecule verify."
			     (interactive)
			     (funcall 'molecule--wrapper "verify")))


;;;###autoload
(defun molecule-version ()
  "Show molecule and molecule.el version."
  (interactive)
  (let ((output))
    (if (eq molecule-version-v nil)
	(progn
	  (setq output (shell-command-to-string
			(concat molecule-command " --version")))
	  (setq output (concat output "molecule.el v0.1"))
	  (setq molecule-version-v output)))
    (message molecule-version-v)))

;;;###autoload
(defun molecule-debug ()
  "Toggle molecule debug."
  (interactive)
  (if (eq molecule-debug-v nil)
      (setq molecule-debug-v t)
    (setq molecule-debug-v nil)))

;;;###autoload
(defvar molecule-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x m c") #'molecule-check)
    (define-key map (kbd "C-x m o") #'molecule-converge)
    (define-key map (kbd "C-x m r") #'molecule-create)
    (define-key map (kbd "C-x m d") #'molecule-dependency)
    (define-key map (kbd "C-x m e") #'molecule-destroy)
    (define-key map (kbd "C-x m i") #'molecule-idempotence)
    (define-key map (kbd "C-x m n") #'molecule-init)
    (define-key map (kbd "C-x m l") #'molecule-lint)
    (define-key map (kbd "C-x m s") #'molecule-list)
    (define-key map (kbd "C-x m f") #'molecule-side-effect)
    (define-key map (kbd "C-x m y") #'molecule-syntax)
    (define-key map (kbd "C-x m t") #'molecule-test)
    (define-key map (kbd "C-x m v") #'molecule-verify)
    (define-key map (kbd "C-x m b") #'molecule-debug)
    map)
  "Keymap for all `molecule-mode' functions.")

;;;###autoload
(define-minor-mode molecule-mode
  "Minor mode for molecule wrapper.

When called interactively, toggle `molecule-mode'.  With
prefix ARG, enable `molecule-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `molecule-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`molecule-mode'.  Otherwise behave as if called interactively.

In `molecule-mode' provide the following keybindings for
molecule testing:

\\{molecule-mode-map}"
  :init-value nil
  :keymap molecule-mode-map
  :lighter " ADoc"
  :group 'molecule.el
  :require 'molecule.el)

(provide 'molecule)

;;; molecule.el ends here
