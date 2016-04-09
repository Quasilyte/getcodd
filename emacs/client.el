(defun gcdd-snippet-once (lang query)
  (shell-command "rm ../1 && mkfifo ../1")
  (switch-to-buffer-other-window "*gcdd*")
  (erase-buffer)
  (let ((result (shell-command-to-string
                 (format "../bin/send_once \"../1\" \"%s\""
                         (format "1;query;snippet;%s;%s"
                                 (capitalize lang)
                                 query)))))
    (let ((result (replace-regexp-in-string "{#delimeter#}"
                                            "<br><br><br>"
                                            result)))
      (cond ((> 4 (length result))
             (message "nothing found!"))
            (t (insert (substring result 1 (- (length result) 2)))
               (shr-render-buffer (current-buffer))
               (eval
                (car
                 (read-from-string
                  (format "(%s-mode)" lang))))
               (indent-region (point-min) (point-max)))))))
    
(gcdd-snippet-once "go" "null object")

    
