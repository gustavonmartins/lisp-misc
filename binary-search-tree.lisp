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
