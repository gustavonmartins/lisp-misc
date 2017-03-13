(load "queue.lisp")
(setf *print-circle* t)

(defstruct graph
  (vertexlist nil :type list)
  (vertexamount 0)
  (edgesamount 0)
  )

(defstruct vertex
  name
  (sucessors nil :type list)
  (enqueued-or-visited nil :type boolean)
  (in-degree 0 :type integer)
  (out-degree 0 :type integer)
  )

(defmethod addvertex ((g graph) vertexname) ; undesired behaviour with repeated items
  (push (make-vertex :name vertexname) (graph-vertexlist g))
  (incf (graph-vertexamount g))
  )

(defmethod addvertex ((g graph) (l list))
  (mapcar (lambda (x)
	    (addvertex g x))
	  l)
  )

(defmethod removevertex ((g graph) vertexname)
  (setf (graph-vertexlist g) (remove-if (lambda (x)
					  (if (and (eq (vertex-name x) vertexname)
						   (eq (vertex-in-degree x) 0))
					      (progn (princ 'found)
						     (mapcar (lambda (y)                        ;Actions on sucessors
							       (decf (vertex-in-degree y)))
							     (vertex-sucessors x)
							     )
						     (princ 'todo-actions-on-precedents)	;Actions on precedents
						     (decf (graph-vertexamount g))             ;Actions on graph vertex
						     (decf (graph-edgesamount g) (vertex-out-degree x)) ; Actions on graph edges
						     t)
					      (progn (princ 'not-found-or-indeg-incompatible)
						     nil)))
					(graph-vertexlist g)))
  )

(defmethod addedge ((g graph) first-vertex-name second-vertex-name) ; undesired behaviour with repeated items
  (let (
	(first-vertex (find first-vertex-name (graph-vertexlist g) :key 'vertex-name)) 
	(second-vertex (find second-vertex-name (graph-vertexlist g) :key 'vertex-name)))
    (unless (or (null first-vertex) (null second-vertex))
      (push second-vertex (vertex-sucessors first-vertex))
      (incf (vertex-in-degree second-vertex))
      (incf (vertex-out-degree first-vertex))
      (incf (graph-edgesamount g))
      ))
  )


(defmethod mapsucessors-bfs ((g graph) (apply-fn function) starting-vertex-name) ; Problem: cant reach nodes not connected to initial node!
  (mapcar (lambda (x)
	    (setf (vertex-enqueued-or-visited x) nil))
	  (graph-vertexlist g));mark all vertices as not-visited
  
  (let* (
	 (starting-vertex (find starting-vertex-name (graph-vertexlist g) :key 'vertex-name))
	 (tovisit-list (make-fifo))
	 (mark-as-visited (lambda (x)
			    (setf (vertex-enqueued-or-visited x) t)))
	 )
    (enqueue tovisit-list starting-vertex mark-as-visited)
    (enqueue tovisit-list (vertex-sucessors starting-vertex) mark-as-visited)
    
    (mapcar (lambda (x) ;take each enqueued vertex and analyse it
	      (funcall apply-fn x)
	      (mapcar (lambda (y) ; have a look at sucessors to the current vertex add it to the queue list (unless it has been added before)
			(unless (vertex-enqueued-or-visited y)
			  (setf (vertex-enqueued-or-visited y) t)
			  (enqueue tovisit-list y)
			  ))
		      (vertex-sucessors x))
	      )
	    (fifo-aslist tovisit-list))
    )
  )

(defparameter test (make-graph))
(addvertex test (list 'A 'B 'C 'D 'E 'F 'G 'H 'I))

(addedge test 'A 'B)
(addedge test 'A 'C)
(addedge test 'A 'D)
(addedge test 'A 'E)

(addedge test 'B 'F)
(addedge test 'F 'H)
(addedge test 'D 'G)
(addedge test 'G 'I)

(mapsucessors-bfs test (lambda (x) (princ (vertex-name x))) 'A)
