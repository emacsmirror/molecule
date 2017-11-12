;;; molecule.el --- Simple wrapper for molecule -*- lexical-binding: t -*-

;; Copyright:: Copyright (c) 2017, drymer

;; Author:: drymer <drymer [ AT ] autistici.org>
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1

;; Keywords:: languages terminals
;; X-URL:: https://git.daemons.it/drymer/molecule.el

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
;;; or open an issue at https://git.daemons.it/drymer/molecule.el

;;; Code:

(defgroup molecule nil
  "Wrapper around molecule command."
  :group 'tools)

(defcustom molecule-command "molecule"
  "The molecule command (no shit, Sherlock).  It shouldn't be necessary to chan\
ee it if it's on the PATH."
  :type 'variable
  :group 'molecule)

(defvar molecule-buffer-name-v "*molecule*"
  "Molecule buffer name.  It shouldn't be necessary to modify it.")

(defcustom molecule-debug nil
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
    (if (eq molecule-debug t)
	(setq debug "--debug ")
      (setq debug ""))
    ;; Choose scenario or role
    (setq molecule-parameter
	  (ivy-read "Choose between \"role\" or \"scenario\": " (list "scenario"
								      "role")))
    ;; If scenario, choose scenario-name and set role-name automatically
    (if (string= molecule-parameter "scenario")
	(progn
	  (setq role-name (concat " -r " (file-name-nondirectory
					  (directory-file-name
					   (file-name-directory
					    default-directory)))))
	  (setq scenario-name
		(read-string "Scenario name (leave it empty to use the \"default\"): "))
	  (if (string= scenario-name "")
	      (setq scenario-name "default")))
      (setq role-name (concat " -r " (read-string "Role name: "))))
    (setq molecule-parameter (concat molecule-parameter " "))
    (if (boundp 'scenario-name)
	(setq scenario-name "")
      (setq scenario-name (concat " -s" scenario-name)))
    ;; Execute molecule
    (start-process molecule-buffer-name-v molecule-buffer-name-v "sh" "-c"
		   (concat molecule-command " init " debug molecule-parameter scenario-name role-name))
    (message "Molecule finished!")))

(defun molecule--wrapper (command)
  "Molecule generic `COMMAND' wrapper."
  (let ((debug)
	(bname)
	(scenario)
	(molecule-dir)
	(scenarios)
	(old-dir)
	(rel-path)
	(parent-dir (file-name-nondirectory (directory-file-name
					      (file-name-directory
					       default-directory))))
	;; See if it's a typical ansible directory
	(ansible-dirs '("defaults" "handlers" "vars" "meta" "tests" "tasks")))
    ;; Set debug
    (if (eq molecule-debug t)
	(setq debug "--debug ")
      (setq debug ""))
    ;; See if parent-dir is member of ansible-dirs
    (if (member parent-dir ansible-dirs)
	(progn
	  (setq rel-path "../")
	  (setq molecule-dir "../molecule"))
      (progn
	(setq rel-path "")
	(setq molecule-dir "molecule")))
    ;; Execute only if the molecule directory exists
    (if (file-exists-p molecule-dir)
	(progn
	  ;; Exclude . and ..
	  (setq scenarios (directory-files (expand-file-name molecule-dir) nil
					   "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
	  ;; If there's more than one scenario, give an option to choose them
	  (if (> (length scenarios) 1)
	      (progn
		(setq scenarios (cons "all" scenarios))
		(message scenarios)
		;; TODO: hacer el ivy-read generico y reusable al menos con ido
		(setq scenario (concat " -s " (ivy-read "Choose a scenario: "
							scenarios)))))
	  (setq default-directory (concat default-directory rel-path))
	  (compile (concat molecule-command " " debug command scenario))
	  (setq default-directory old-dir))
      (message "There's no scenarios! You should execute M-x molecule-init"))))

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
	  (setq output (shell-command-to-string (concat molecule-command " --version")))
	  (setq output (concat output "molecule.el v0.1"))
	  (setq molecule-version-v result))
      (message molecule-version-v))))

(provide 'molecule)

;;; molecule.el ends here
