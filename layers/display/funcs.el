;;; All-the-icons-ivy Advice

(when (configuration-layer/package-usedp 'all-the-icons-ivy)
  (defun ivy-file-transformer-fixed-for-files (s)
    "Gets file icon for string, fixing bug for folders and windows box."
    (format "%s\t%s"
            (if (and linux? (s-ends-with? "/" s))
                (propertize "\t" 'display "" 'face 'all-the-icons-silver)
              (propertize "\t" 'display (all-the-icons-icon-for-file s)))
            s)))
