;;; agent-sessions-pr-test.el --- Tests for dashboard PR state  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-sessions)
(require 'agent-sessions-test)

(defmacro agent-sessions-pr-test--isolated (&rest body)
  "Run BODY with fresh PR state and no PR timers or processes."
  (declare (indent 0) (debug t))
  `(let ((agent-sessions--pr-states (make-hash-table :test 'equal))
         (agent-sessions--pr-urls nil)
         (agent-sessions--pr-process nil)
         (agent-sessions--pr-poll-timer nil)
         (agent-sessions--pr-kick-timer nil))
     ,@body))

(defun agent-sessions-pr-test--node (&rest props)
  "A GraphQL pullRequest node with PROPS and checks state :checks."
  (let ((checks (plist-get props :checks)))
    (append (list :isDraft (plist-get props :isDraft)
                  :state (or (plist-get props :state) "OPEN")
                  :reviewDecision (plist-get props :reviewDecision)
                  :reviewRequests (list :totalCount
                                        (or (plist-get props :requests) 0))
                  :isInMergeQueue (plist-get props :isInMergeQueue)
                  :commits (list :nodes
                                 (list (list :commit
                                             (list :statusCheckRollup
                                                   (and checks
                                                        (list :state checks))))))))))

(defun agent-sessions-pr-test--faces (text)
  "The distinct `font-lock-face' values on TEXT, in order of appearance."
  (let (faces)
    (dotimes (i (length text))
      (let ((face (get-text-property i 'font-lock-face text)))
        (when (and face (not (member face faces)))
          (push face faces))))
    (nreverse faces)))

(ert-deftest agent-sessions-pr-state-from-node-orders-lifecycle ()
  (let ((cases
         `(((:state "MERGED" :isDraft t :checks "FAILURE") merged)
           ((:state "CLOSED" :checks "PENDING") closed)
           ((:isDraft t :isInMergeQueue t :checks "PENDING") draft)
           ((:isInMergeQueue t :checks "PENDING") queue)
           ((:checks "PENDING") checks)
           ((:checks "EXPECTED") checks)
           ((:checks "FAILURE") failed)
           ((:checks "ERROR") failed)
           ((:checks "SUCCESS") open)
           (() open))))
    (pcase-dolist (`(,props ,life) cases)
      (should (eq life (plist-get (agent-sessions--pr-state-from-node
                                   (apply #'agent-sessions-pr-test--node
                                          props))
                                  :life))))))

(ert-deftest agent-sessions-pr-state-from-node-reads-review ()
  (pcase-dolist (`(,decision ,requests ,review)
                 '(("APPROVED" 0 approved)
                   ("CHANGES_REQUESTED" 0 changes-requested)
                   ;; A decision outranks reviewers still listed as requested.
                   ("APPROVED" 2 approved)
                   ("CHANGES_REQUESTED" 1 changes-requested)
                   ("REVIEW_REQUIRED" 1 requested)
                   (nil 1 requested)
                   ("REVIEW_REQUIRED" 0 nil)
                   (nil 0 nil)))
    (should (eq review
                (plist-get (agent-sessions--pr-state-from-node
                            (agent-sessions-pr-test--node
                             :reviewDecision decision :requests requests))
                           :review)))))

(ert-deftest agent-sessions-pr-url-parts-accepts-only-github-prs ()
  (should (equal '("example-org" "example-repo" 26425)
                 (agent-sessions--pr-url-parts
                  "https://github.com/example-org/example-repo/pull/26425")))
  (should (equal '("o" "r" 7)
                 (agent-sessions--pr-url-parts
                  "https://github.com/o/r/pull/7/files")))
  (should-not (agent-sessions--pr-url-parts
               "https://github.com/o/r/issues/7"))
  (should-not (agent-sessions--pr-url-parts
               "https://gitlab.com/o/r/pull/7"))
  (should-not (agent-sessions--pr-url-parts
               "https://github.com/o\"x/r/pull/7")))

(ert-deftest agent-sessions-pr-query-aliases-each-url-in-order ()
  (let ((query (agent-sessions--pr-query
                '("https://github.com/a/b/pull/1"
                  "https://github.com/c/d/pull/2"))))
    (should (string-match-p
             "p0:repository(owner:\"a\",name:\"b\"){pullRequest(number:1)"
             query))
    (should (string-match-p
             "p1:repository(owner:\"c\",name:\"d\"){pullRequest(number:2)"
             query))
    (should (string-match-p "reviewRequests(first:0){totalCount}" query))))

(ert-deftest agent-sessions-pr-apply-response-keeps-good-prs-beside-errors ()
  (agent-sessions-pr-test--isolated
    (let ((good "https://github.com/a/b/pull/1")
          (bad "https://github.com/a/nope/pull/2")
          (silent "https://github.com/a/b/pull/3"))
      (agent-sessions--pr-apply-response
       (list good bad silent)
       (list :data (list :p0 (list :pullRequest
                                   (agent-sessions-pr-test--node
                                    :reviewDecision "APPROVED"
                                    :checks "PENDING"))
                         :p1 nil
                         :p2 nil)
             :errors (list (list :path '("p1")
                                 :message "Could not resolve to a Repository"))))
      (should (equal '(:life checks :review approved)
                     (gethash good agent-sessions--pr-states)))
      (should (equal '(:error "Could not resolve to a Repository")
                     (gethash bad agent-sessions--pr-states)))
      (should (equal '(:error "PR not found")
                     (gethash silent agent-sessions--pr-states))))))

(ert-deftest agent-sessions-pr-failure-keeps-the-last-good-state ()
  (agent-sessions-pr-test--isolated
    (let ((known "https://github.com/a/b/pull/1")
          (failed "https://github.com/a/b/pull/2")
          (new "https://github.com/a/b/pull/3"))
      (puthash known '(:life open :review nil) agent-sessions--pr-states)
      (puthash failed '(:error "old") agent-sessions--pr-states)
      (agent-sessions--pr-record-failure (list known failed new) "offline")
      (should (equal '(:life open :review nil)
                     (gethash known agent-sessions--pr-states)))
      (should (equal '(:error "offline")
                     (gethash failed agent-sessions--pr-states)))
      (should (equal '(:error "offline")
                     (gethash new agent-sessions--pr-states))))))

(ert-deftest agent-sessions-pr-glyphs-and-words-show-each-state ()
  (agent-sessions-pr-test--isolated
    (let ((url "https://github.com/a/b/pull/1"))
      (should (equal "…" (agent-sessions--pr-glyphs url)))
      (should (equal "fetching…" (agent-sessions--pr-words url)))
      (puthash url '(:life checks :review approved) agent-sessions--pr-states)
      (let ((glyphs (agent-sessions--pr-glyphs url))
            (words (agent-sessions--pr-words url)))
        (should (equal "◔ ✓" glyphs))
        (should (equal '(agent-sessions-pr-checks agent-sessions-pr-approved)
                       (agent-sessions-pr-test--faces glyphs)))
        (should (equal "checks running · approved" words)))
      (puthash url '(:life failed :review changes-requested)
               agent-sessions--pr-states)
      (should (equal "◔ ✗" (agent-sessions--pr-glyphs url)))
      (should (equal '(agent-sessions-pr-failed
                       agent-sessions-pr-changes-requested)
                     (agent-sessions-pr-test--faces
                      (agent-sessions--pr-glyphs url))))
      (should (equal "checks failed · changes requested"
                     (agent-sessions--pr-words url)))
      (puthash url '(:life open :review requested) agent-sessions--pr-states)
      (should (equal "○ ◇" (agent-sessions--pr-glyphs url)))
      (should (equal '(agent-sessions-pr-open
                       agent-sessions-pr-review-requested)
                     (agent-sessions-pr-test--faces
                      (agent-sessions--pr-glyphs url))))
      (should (equal "open · reviewer assigned"
                     (agent-sessions--pr-words url)))
      (puthash url '(:life merged :review nil) agent-sessions--pr-states)
      (should (equal "◉" (agent-sessions--pr-glyphs url)))
      (should (equal "merged" (agent-sessions--pr-words url)))
      (puthash url '(:error "Bad credentials") agent-sessions--pr-states)
      (should (equal "?" (agent-sessions--pr-glyphs url)))
      (should (equal "Bad credentials" (agent-sessions--pr-words url))))
    (should (equal "" (agent-sessions--pr-glyphs "https://example.com/pr/1")))
    (should (equal "not a GitHub PR URL"
                   (agent-sessions--pr-words "https://example.com/pr/1")))))

(ert-deftest agent-sessions-pr-single-link-speaks-for-the-session ()
  (agent-sessions-test--with-temp-db
    (let ((one (list :agent-session-id "thread-one"))
          (two (list :agent-session-id "thread-two")))
      (agent-sessions--share-link
       one "linear" "https://linear.app/issue/ABC-1" "ABC-1")
      (agent-sessions--share-link
       one "pr" "https://github.com/a/b/pull/1" "PR 1")
      (should (equal "https://github.com/a/b/pull/1"
                     (plist-get (agent-sessions--session-pr-link one) :url)))
      (agent-sessions--share-link
       two "pr" "https://github.com/a/b/pull/2" "PR 2")
      (agent-sessions--share-link
       two "pr" "https://github.com/a/b/pull/3" "PR 3")
      (should-not (agent-sessions--session-pr-link two))
      (should (equal '("https://github.com/a/b/pull/1"
                       "https://github.com/a/b/pull/2"
                       "https://github.com/a/b/pull/3")
                     (agent-sessions--pr-urls-of
                      (list (list :session one) (list :session two)
                            (list :session one))))))))

(ert-deftest agent-sessions-pr-row-puts-glyphs-after-state ()
  (agent-sessions-test--with-temp-db
    (agent-sessions-pr-test--isolated
      (let ((session (list :agent-session-id "thread-one"))
            (url "https://github.com/a/b/pull/1"))
        (agent-sessions--share-link session "pr" url "PR 1")
        (puthash url '(:life queue :review approved) agent-sessions--pr-states)
        (with-temp-buffer
          (setq agent-sessions--pr-column-width
                agent-sessions--pr-marks-width)
          (let* ((marks (agent-sessions--pr-marks session))
                 (row (agent-sessions--row-text
                       "    " "REVIEW" "Title\n"
                       'agent-sessions-needs-attention marks))
                 (start (+ 4 (agent-sessions--state-column-width))))
            (should (equal "◕ ✓  " marks))
            (should (equal "◕ ✓  Title\n" (substring row start)))
            ;; The glyph keeps its colour and the row keeps its highlight.
            (should (equal '(agent-sessions-pr-queue
                             agent-sessions-needs-attention)
                           (get-text-property start 'font-lock-face row)))
            (should (eq 'agent-sessions-needs-attention
                        (get-text-property (1+ start) 'font-lock-face row)))))
        (with-temp-buffer
          (setq agent-sessions--pr-column-width 0)
          (should (equal "" (agent-sessions--pr-marks session))))))))

(ert-deftest agent-sessions-pr-link-row-ends-with-words ()
  (agent-sessions-test--with-temp-db
    (agent-sessions-pr-test--isolated
      (let ((session (list :agent-session-id "thread-one"))
            (pr "https://github.com/a/b/pull/1"))
        (agent-sessions--share-link
         session "linear" "https://linear.app/issue/ABC-1" "ABC-1")
        (agent-sessions--share-link session "pr" pr "PR #1")
        (puthash pr '(:life open :review changes-requested)
                 agent-sessions--pr-states)
        (with-temp-buffer
          (agent-sessions-dashboard-mode)
          (let ((inhibit-read-only t)
                (agent-sessions--row-status nil))
            (agent-sessions--insert-links session "  ")
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (string-suffix-p "ABC-1" (car lines)))
              (should (string-suffix-p "PR #1   open · changes requested"
                                       (cadr lines))))))))))

(defun agent-sessions-pr-test--fake-gh (stdout stderr status)
  "Write a fake `gh' printing STDOUT and STDERR, exiting STATUS; return its path."
  (let ((script (make-temp-file "fake-gh-")))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              (format "printf '%%s' %s\n" (shell-quote-argument stdout))
              (format "printf '%%s\\n' %s >&2\n" (shell-quote-argument stderr))
              (format "exit %d\n" status)))
    (set-file-modes script #o755)
    script))

(defun agent-sessions-pr-test--fetch-with (gh urls)
  "Fetch URLS through the fake GH and wait for it to finish."
  (setq agent-sessions--pr-urls urls)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name &rest _) (and (equal name "gh") gh)))
            ((symbol-function 'agent-sessions--refresh-if-visible) #'ignore))
    (agent-sessions--pr-fetch)
    (let ((proc agent-sessions--pr-process))
      (should proc)
      (while (process-live-p proc)
        (accept-process-output proc 0.1))
      ;; The sentinel runs after the process dies; give it its turn.
      (let ((deadline (+ (float-time) 5)))
        (while (and agent-sessions--pr-process (< (float-time) deadline))
          (accept-process-output nil 0.05))))))

(ert-deftest agent-sessions-pr-fetch-reads-partial-answers-from-gh ()
  (agent-sessions-pr-test--isolated
    (let* ((good "https://github.com/a/b/pull/1")
           (bad "https://github.com/a/nope/pull/2")
           (gh (agent-sessions-pr-test--fake-gh
                (concat "{\"data\":{\"p0\":{\"pullRequest\":{\"isDraft\":true,"
                        "\"state\":\"OPEN\",\"reviewDecision\":null,"
                        "\"isInMergeQueue\":false,\"commits\":{\"nodes\":[]}}},"
                        "\"p1\":null},\"errors\":[{\"path\":[\"p1\"],"
                        "\"message\":\"Could not resolve\"}]}")
                "gh: Could not resolve" 1)))
      (unwind-protect
          (progn
            (agent-sessions-pr-test--fetch-with gh (list good bad))
            (should (equal '(:life draft :review nil)
                           (gethash good agent-sessions--pr-states)))
            (should (equal '(:error "Could not resolve")
                           (gethash bad agent-sessions--pr-states))))
        (delete-file gh)))))

(ert-deftest agent-sessions-pr-fetch-reports-a-whole-failure ()
  (agent-sessions-pr-test--isolated
    (let* ((known "https://github.com/a/b/pull/1")
           (new "https://github.com/a/b/pull/2")
           (gh (agent-sessions-pr-test--fake-gh
                "" "gh: error connecting to api.github.com\nmore detail" 1)))
      (puthash known '(:life open :review nil) agent-sessions--pr-states)
      (unwind-protect
          (progn
            (agent-sessions-pr-test--fetch-with gh (list known new))
            (should (equal '(:life open :review nil)
                           (gethash known agent-sessions--pr-states)))
            (should (equal '(:error "error connecting to api.github.com")
                           (gethash new agent-sessions--pr-states))))
        (delete-file gh)))))

(ert-deftest agent-sessions-pr-fetch-skips-final-prs-unless-forced ()
  (agent-sessions-pr-test--isolated
    (let ((merged "https://github.com/a/b/pull/1")
          (started nil))
      (puthash merged '(:life merged :review approved)
               agent-sessions--pr-states)
      (setq agent-sessions--pr-urls (list merged "https://example.com/x"))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (setq started args) nil))
                ((symbol-function 'process-put) #'ignore)
                ((symbol-function 'set-process-sentinel) #'ignore)
                ((symbol-function 'run-with-timer) #'ignore))
        (agent-sessions--pr-fetch)
        (should-not started)
        (agent-sessions--pr-fetch t)
        (should started)
        (let ((command (plist-get started :command)))
          (delete-file (nth 5 command))
          (should (string-match-p "pullRequest(number:1)" (nth 4 command))))))))

(ert-deftest agent-sessions-pr-fetch-without-gh-records-why ()
  (agent-sessions-pr-test--isolated
    (let ((url "https://github.com/a/b/pull/1"))
      (setq agent-sessions--pr-urls (list url))
      (cl-letf (((symbol-function 'executable-find) #'ignore)
                ((symbol-function 'agent-sessions--refresh-if-visible)
                 #'ignore))
        (agent-sessions--pr-fetch))
      (should (equal '(:error "gh not found")
                     (gethash url agent-sessions--pr-states))))))

(defmacro agent-sessions-pr-test--with-dashboard (&rest body)
  "Run BODY while a buffer stands in for the dashboard."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let ((agent-sessions-buffer-name (buffer-name)))
       ,@body)))

(ert-deftest agent-sessions-pr-note-shown-arms-timers-only ()
  (agent-sessions-pr-test--isolated
    (agent-sessions-pr-test--with-dashboard
      (let ((agent-sessions-pr-poll-interval 180)
            timers)
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (secs repeat fn &rest _)
                     (push (list secs repeat fn) timers)
                     (list 'timer secs)))
                  ((symbol-function 'make-process)
                   (lambda (&rest _)
                     (error "The render must not start a process"))))
          (agent-sessions--pr-note-shown nil)
          (should-not timers)
          (agent-sessions--pr-note-shown '("https://github.com/a/b/pull/1"))
          ;; The poll, one-shot, and a near-immediate fetch for the new PR.
          (should (equal '((0.5 nil) (180 nil))
                         (mapcar #'butlast timers)))
          (setq timers nil)
          (agent-sessions--pr-note-shown '("https://github.com/a/b/pull/1"))
          ;; Both already armed.
          (should-not timers))))))

(ert-deftest agent-sessions-pr-poll-delay-follows-motion ()
  (agent-sessions-pr-test--isolated
    (let ((agent-sessions-pr-poll-interval 180)
          (agent-sessions-pr-poll-interval-active 20)
          (a "https://github.com/a/b/pull/1")
          (b "https://github.com/a/b/pull/2"))
      (setq agent-sessions--pr-urls (list a b))
      ;; Nothing known yet: the first fetch is the kick's job, not the poll's.
      (should (= 180 (agent-sessions--pr-poll-delay)))
      (puthash a '(:life open :review approved) agent-sessions--pr-states)
      (puthash b '(:error "Bad credentials") agent-sessions--pr-states)
      (should (= 180 (agent-sessions--pr-poll-delay)))
      (dolist (life '(checks queue))
        (puthash b (list :life life :review nil) agent-sessions--pr-states)
        (should (= 20 (agent-sessions--pr-poll-delay))))
      (dolist (life '(failed draft merged))
        (puthash b (list :life life :review nil) agent-sessions--pr-states)
        (should (= 180 (agent-sessions--pr-poll-delay))))
      ;; A PR in motion that is no longer on the dashboard does not count.
      (puthash b '(:life checks :review nil) agent-sessions--pr-states)
      (setq agent-sessions--pr-urls (list a))
      (should (= 180 (agent-sessions--pr-poll-delay))))))

(ert-deftest agent-sessions-pr-poll-rearms-when-no-fetch-starts ()
  (agent-sessions-pr-test--isolated
    (agent-sessions-pr-test--with-dashboard
      (let ((agent-sessions-pr-poll-interval 180)
            (url "https://github.com/a/b/pull/1")
            timers)
        (puthash url '(:life merged :review nil) agent-sessions--pr-states)
        (setq agent-sessions--pr-urls (list url))
        (cl-letf (((symbol-function 'run-with-timer)
                   (lambda (secs &rest _) (push secs timers) (list 'timer secs)))
                  ((symbol-function 'cancel-timer) #'ignore))
          ;; Only a merged PR: no fetch starts, so the poll re-arms itself.
          (agent-sessions--pr-poll)
          (should (equal '(180) timers))
          (should agent-sessions--pr-poll-timer))))))

(ert-deftest agent-sessions-pr-poll-stops-without-a-dashboard ()
  (agent-sessions-pr-test--isolated
    (let ((agent-sessions-buffer-name " *no such dashboard*")
          started)
      (setq agent-sessions--pr-urls '("https://github.com/a/b/pull/1"))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq started t)))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (setq started t))))
        (agent-sessions--pr-poll)
        (should-not started)
        (should-not agent-sessions--pr-poll-timer)))))

(ert-deftest agent-sessions-pr-finish-times-next-poll-by-new-state ()
  (agent-sessions-pr-test--isolated
    (agent-sessions-pr-test--with-dashboard
      (let* ((agent-sessions-pr-poll-interval 180)
             (agent-sessions-pr-poll-interval-active 20)
             (url "https://github.com/a/b/pull/1")
             (gh (agent-sessions-pr-test--fake-gh
                  (concat "{\"data\":{\"p0\":{\"pullRequest\":{\"isDraft\":false,"
                          "\"state\":\"OPEN\",\"reviewDecision\":null,"
                          "\"isInMergeQueue\":true,\"commits\":{\"nodes\":[]}}}}}")
                  "" 0))
             delays)
        (unwind-protect
            (cl-letf (((symbol-function 'run-with-timer)
                       (lambda (secs repeat fn &rest args)
                         (unless (eq fn #'agent-sessions--pr-timeout)
                           (push secs delays))
                         (list 'timer secs repeat fn args)))
                      ((symbol-function 'cancel-timer) #'ignore))
              (agent-sessions-pr-test--fetch-with gh (list url))
              (should (equal '(:life queue :review nil)
                             (gethash url agent-sessions--pr-states)))
              (should (equal '(20) delays)))
          (delete-file gh))))))

(provide 'agent-sessions-pr-test)
;;; agent-sessions-pr-test.el ends here
