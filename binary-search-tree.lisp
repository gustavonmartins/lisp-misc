(defstruct node
  key
  left
  right)

(defmethod setkey ((n node) newkey)
  (setf (node-key n) newkey))

(defmethod setleft ((n node) (child node))
  (setf (node-left n) child))

(defmethod setright ((n node) (child node))
  (setf (node-right n) child))

(defmethod addkey ((n node) newkey)
  (cond((eq (node-key n) nil) (setf (node-key n) newkey) (princ 'set))
       ((< newkey (node-key n)) (cond ((eq (node-left n) nil) (setleft n (make-node :key newkey)))
				      (t (addkey (node-left n) newkey))
				      ))
       ((> newkey (node-key n)) (cond ((eq (node-right n) nil) (setright n (make-node :key newkey)))
				      (t (addkey (node-right n) newkey))
				      ))
       ((= newkey (node-key n)) (princ 'repeated))
       )
  )

(defmethod mappath-struct ((fn function) (tree node) endkey)
  (funcall fn tree)
  (cond ((< endkey (node-key tree)) (princ 'go-left) (cond ((not (eq (node-left tree) nil)) (mappath-struct fn (node-left tree) endkey))
							   (t (princ '-deadend)))
	 )
	((> endkey (node-key tree)) (princ 'go-right) (cond ((not (eq (node-right tree) nil)) (mappath-struct fn (node-right tree) endkey))
							    (t (princ '-deadend)))
	 )
	((= endkey (node-key tree)) (princ 'stop-found))
	)
  )

(defmethod mappath ((fn function) (tree node) endkey)
  (mappath-struct (lambda (n) (funcall fn (node-key n))) tree endkey)
    )

(defmethod maptree-struct ((fn function) (tree node))
  (unless (eq (node-left tree) nil) (maptree-struct fn (node-left tree)))
  (funcall fn tree)
  (unless (eq (node-right tree) nil) (maptree-struct fn (node-right tree)))
  )

(defmethod maptree ((fn function) (tree node))
  (maptree-struct (lambda (n) (funcall fn (node-key n))) tree)
    )

(defmethod addkeys-from-list ((tree node) list)
(mapcar (lambda (x) (addkey tree x)) list)
  )
