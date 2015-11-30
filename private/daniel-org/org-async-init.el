(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org)
(require 'ox)
(require 'ox-beamer)
(require 'ox-latex)
(require 'cl)
(require 'org-capture)

(setq org-export-async-debug nil)
