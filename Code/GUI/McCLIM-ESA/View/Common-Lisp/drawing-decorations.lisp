(cl:in-package #:second-climacs-clim-view-common-lisp)

;;; Region

(defun draw-region (context point mark first-line last-line max-column
                    &key (kind :primary))
  (multiple-value-bind (from to) (if (cluffer:cursor< point mark)
                                     (values point mark)
                                     (values mark point))
    (multiple-value-bind (start-line start-column end-line end-column)
        (filter-area
         (cluffer:line-number from) (cluffer:cursor-position from)
         (cluffer:line-number to)   (cluffer:cursor-position to)
         first-line last-line)
      ;; END-COLUMN is `nil' when the line which contains TO is
      ;; after LAST-LINE.
      (let ((end-column (or end-column max-column))
            (ink        (ecase kind
                          (:primary   clim:+light-blue+)
                          (:secondary clim:+light-gray+))))
        (draw-multiple-line-rectangle
         context start-line start-column end-line end-column max-column
         :ink ink :include-descent t)))))

(defun draw-regions (context buffer first-line last-line max-column)
  (let ((primary-site (text.editing:site buffer)))
    (text.editing:map-sites
     (lambda (site)
       (when (text.editing:mark-active-p site)
         (let ((point (text.editing:point site))
               (mark  (text.editing:mark site))
               (kind  (if (eq site primary-site) :primary :secondary)))
           (draw-region context point mark first-line last-line max-column
                        :kind kind))))
     buffer)))

;;; Cursor

(defun draw-cursor* (stream x y &key (role    :point)
                                     (kind    :primary)
                                     (ascent  (error "todo"))
                                     (descent (error "todo")))
  (let ((ink (ecase kind
               (:primary   clim:+blue+)
               (:secondary clim:+steel-blue+)
               (:other     clim:+gray50+))))
    (ecase role
      (:point
       (let* ((width  (* 1/6 (+ ascent descent)))
              (offset (* -1/3 width))
              (offset (+ offset (- (min 0 (+ x offset))))))
         (clim:draw-rectangle* stream
                               (+ x offset)       (- y ascent)
                               (+ x offset width) (+ y descent)
                               :ink ink)))
      (:active-mark
       (draw-triangle-marker* stream x y :ink ink :filled t))
      (:inactive-mark
       (draw-triangle-marker* stream x y :ink ink :filled nil)))))

(defun draw-cursor (context line column &rest args &key role kind)
  (declare (ignore role kind))
  (multiple-value-bind (x y) (text-position context line column
                                            :include-ascent t)
    (let ((ascent  (ascent context))
          (descent (descent context)))
      (apply #'draw-cursor* (stream* context) x y
             :ascent ascent :descent descent args))))

(defun draw-cursors (context buffer first-line last-line)
  (labels ((maybe-draw-cursor (cursor role kind)
             (let ((cursor-line-number (cluffer:line-number cursor)))
               (when (<= first-line cursor-line-number last-line)
                 (let ((cursor-column-number (cluffer:cursor-position cursor)))
                   (draw-cursor context cursor-line-number cursor-column-number
                                :role role :kind kind)))))
           (draw-site (site kind)
             (let ((point (text.editing:point site))
                   (mark  (text.editing:mark site)))
               (maybe-draw-cursor point :point kind)
               (when mark
                 (let ((role (if (text.editing:mark-active-p site)
                                 :active-mark
                                 :inactive-mark)))
                   (maybe-draw-cursor mark role kind)))
               (mapc (alexandria:rcurry
                      #'maybe-draw-cursor :inactive-mark :other)
                     (text.editing::mark-stack site)))))
    (let ((primary-site (text.editing:site buffer)))
      (text.editing:map-sites
       (lambda (site)
         (draw-site site (if (eq site primary-site) :primary :secondary)))
       buffer))))

;;; Errors

(defun draw-error-wads (context drawn-error-wads)
  (let* ((error-wads     (nreverse drawn-error-wads))
         (line-clusters  (cluster-error-wads
                          (lambda (line end-column
                                   previous-line previous-end-column)
                            (declare (ignore end-column previous-end-column))
                            (= previous-line line))
                          error-wads))
         (exact-clusters (cluster-error-wads
                          (lambda (line end-column
                                   previous-line previous-end-column)
                            (and (= previous-line line)
                                 (= end-column previous-end-column)))
                          error-wads)))
    (draw-gutter-indicators context line-clusters)
    (draw-error-wad-decorations context exact-clusters)
    (draw-error-annotations context exact-clusters)))

;;; Error clustering

(defun cluster-error-wads (predicate error-wads)
  (let ((clusters '())
        (cluster  '()))
    (flet ((flush ()
             (unless (null cluster)
               (push (nreverse cluster) clusters))
             (setf cluster '())))
      (loop :for previous-line       = nil :then line
            :for previous-end-column = nil :then end-column
            :for wad                 :in error-wads
            :for line                = (ip:absolute-start-line-number wad)
            :for end-column          = (ip:end-column wad)
            :when (not (or (null cluster)
                           (null previous-line)
                           (funcall predicate
                                    line end-column
                                    previous-line previous-end-column)))
            :do (flush)
            :do (push wad cluster)
            :finally (flush)))
    (nreverse clusters)))

;;; Gutter indicates

(defun draw-gutter-indicators (context error-wad-clusters)
  ;; Draw error indicator in gutter.
  ;; TODO this is a temporary solution. It would be better to inform
  ;; the gutter pane about errors and let it handle the drawing
  ;; itself.
  (let* ((pane   (stream* context))
         (gutter (second-climacs-clim-base::left-gutter pane))
         (width  (character-width context))
         (height (line-height context))
         (ascent (ascent context)))
    (loop :for cluster    :in error-wad-clusters
          :for count      =   (length cluster)
          :for wad        =   (first cluster)
          :for start-line =   (ip:absolute-start-line-number wad)
          :for y          =   (* start-line height)
          :do (clim:with-translation (gutter 0 y)
                (clim:draw-rectangle* gutter 0 0 (+ width 1) height
                                      :ink clim:+red+)
                (when (> count 1)
                  (let ((label (princ-to-string count)))
                    (clim:draw-text* gutter label 1 ascent
                                     :text-size :smaller
                                     :align-y   :baseline)))))))

;;; Wad decorations
;;;
;;; Red underlines in the buffer content.

(defun draw-error-wad-decorations (context error-wad-clusters)
  (mapc (alexandria:curry #'draw-error-wad-cluster-decoration context)
        error-wad-clusters))

(defun draw-error-wad-cluster-decoration (context error-wad-cluster)
  (let* ((start-line   (ip:absolute-start-line-number
                        (first error-wad-cluster)))
         (start-column (reduce #'min error-wad-cluster :key #'ip:start-column))
         (end-column   (reduce #'max error-wad-cluster :key #'ip:end-column)))
    (draw-error-decoration context start-line start-column end-column)))

(defun draw-error-decoration (context line start-column end-column)
  (if (= end-column start-column)
      (draw-triangle-marker context line start-column :ink clim:+red+)
      (draw-underline-marker context line start-column end-column :ink clim:+red+)))

;;; Error annotations
;;;
;;; Popups that appear when the cursor is position in an error wad.

(defun draw-error-annotations (context error-wad-clusters)
  (let* ((clim-view    (clim:stream-default-view (stream* context)))
         (climacs-view (clim-base:climacs-view clim-view))
         (site         (base:site climacs-view))
         (point        (edit:point site)))
    (loop :with cursor-line   =   (cluffer:line-number point)
          :with cursor-column =   (cluffer:cursor-position point)
          :for cluster        :in error-wad-clusters
          :for wad            =   (first cluster)
          :for start-line     =   (ip:absolute-start-line-number wad)
          :when (and (= start-line cursor-line)
                     (<= (ip:start-column wad)
                         cursor-column
                         (ip:end-column wad)))
            :do (draw-error-cluster-annotation
                 context start-line cursor-column cluster))))

(defun draw-error-cluster-annotation (context line column error-wad-cluster)
  (let ((conditions (mapcar #'ip::condition* error-wad-cluster)))
    (draw-error-conditions-annotation context line column conditions)))

;;; Use "tints" so that the background and border of the annotation
;;; work for different pane backgrounds.
(defvar *pink-tint* (clim:compose-in clim:+pink+ (clim:make-opacity .5)))
(defvar *salmon-tint* (clim:compose-in clim:+salmon+ (clim:make-opacity .5)))

(defun draw-error-conditions-annotation (context line column conditions)
  (draw-annotation context line column
                   (lambda (stream width)
                     (format-error-list stream conditions :width width))
                   :background *pink-tint* :border *salmon-tint*))

(defun format-error-list (stream conditions &key width)
  (clim:formatting-item-list (stream :n-columns 1)
    (loop :for condition :in conditions
          :do (clim:formatting-cell (stream)
                (when (> (length conditions) 1)
                  (write-string "• " stream))
                (clim:with-output-as-presentation
                    (stream condition 'clim:expression :single-box t)
                  (let ((x (clim:stream-cursor-position stream)))
                    (clime:with-temporary-margins
                        (stream :left `(:absolute ,x) :right `(:absolute ,width))
                      (clim:with-end-of-line-action (stream :wrap*)
                        (princ condition stream)))))))))
