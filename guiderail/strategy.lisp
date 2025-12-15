(ql:quickload "dexador")
(ql:quickload "yason")

; (load "mvlet.lisp")

(defparameter *endpoint* "http://localhost:8000/query")
(defparameter *read-timeout* 10)
(defparameter *connect-timeout* 10)

(defun call-llm (prompt)
  (format t "Calling LLM w/prompt ***********~%~a~%***************~%" prompt)
  (dexador:post *endpoint*
                :headers '(("Content-Type" . "application/json")
                           ("Accept" . "application/json"))
                :content
                (yason:with-output-to-string* ()
                  (yason:with-object ()
                    (yason:encode-object-element "prompt" prompt)
                    (yason:encode-object-element "include_backend_output" t)))
                :read-timeout *read-timeout*
                :connect-timeout *connect-timeout*))

(defun construct-prompt (pos-sforms neg-sforms)
  (format t "Constructing prompt...~%")
  (with-output-to-string (s)
    (format s "~a~%-----------------------------------~%"
	    (unparse (current-theory)
			:string t
			:char-width *default-char-width*))
    (print-object *ps* s)
    ))

(defun extract-answer (text)
  (format t "Extracting answer...~%")
  (let* ((open "<answer>")
         (close "</answer>")
         (start (search open text))
         (end (and start (search close text :start2 (+ start (length open))))))
    (when (and start end)
      (subseq text (+ start (length open)) end))))

(defun llm-random-test-main (fnums)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (s-forms (select-seq (s-forms goalsequent) fnums))
	     (pos-sforms (pos-s-forms s-forms))
	     (neg-sforms (neg-s-forms s-forms))
	     (prompt (construct-prompt pos-sforms neg-sforms)))
	(multiple-value-bind (resp-str)
	    (handler-case (call-llm prompt)
	      (usocket:invalid-argument-error (c) (format t "~%~%Invalid server: ~a~%Full message: ~a~%~%" endpoint c)))
	  (format t "~%resp-str: ~a~%" resp-str)
	  (let* ((response (gethash "llm_output" (yason:parse resp-str))))
	    (format t "~%Response:~%%%%%%%%%%%%%%%%% ~a%%%%%%%%%%%%%%%%" (extract-answer response)))))))

(addrule 'llm-random-test () ((fnums *)
			      (endpoint "http://localhost:8000/query")
			      (read-timeout 10)
			      (connect-timeout 10))
	 (progn (setq *endpoint* endpoint)
		(setf *read-timeout* read-timeout)
		(setf *connect-timeout* connect-timeout)
		(llm-random-test-main fnums))
	 "Fuzzing the current sequent with a local LLM" 
	 "~%Calling LLM,")


(defun eval-instance-main (ex instance)
  (let* ((inst (typecheck (pc-parse instance 'modname)))
	 (expr (if inst (subst-mod-params ex inst) ex))
	 (cl-eval (eval (pvs2cl expr)))
	 (clval (cl2pvs cl-eval (type expr))))
    (format "clval: ~a~%" clval)))
