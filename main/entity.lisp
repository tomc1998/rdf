(in-package :rdf)

(defparameter *entity-list* ())

;; Basically a passthrough to the corm function, but adds the entity meta-data
;; to the *entity-list*
(defmacro defentity (name slots &key parents override)
  "Define an entity with the given name. This macro creates a class with the
  entity of that name, and creates the appropriate corresponding persistence
  storage. You can create a new entity with the make-<name> function.

  Slots can be given to type the data for storage in a database. Each slot is a
  list, the first item being the name of the slot, the second being the type in
  database storage, and the following items being data regarding other storage
  modifiers.

  A list of parent entities & names for these can be given to create a
  many-to-one relationship between this entity and the given entities. For
  example, one may have a 'user' entity to represent users on a website. Users
  might be able to create posts, which are amalgamated into a central newsfeed.
  There would therefore be a many-to-one relationship between posts and users -
  users would 'own' many posts. Therefore, when defining the post entity, one
  would specify the user entity as a parent.
  This list is given as a plist, with the keyword being the alias for the
  relationship. In the above example, one may want to alias the post - user
  relationship as 'author', as the post is authored by a given user. You would
  then pass (:author user) as the parent list. This will then generate a
  corresponding SQL table column called 'parent_author_id', and can also be
  referenced in complex select queries.

  An autoincrement ID is automatically added to table definitions.

  Example usage:

  ;; Define a user entity
  (defentity user
      ;; Slots
      ((first-name \"VARCHAR (256)\" :not-null)
      (last-name \"VARCHAR (256)\" :not-null))
      ;; Parents (none)
      ())
  ;; Define a child 'post' entity
  (defentity post
      ;; Slots
      (body \"VARCHAR (2048)\" :not-null)
      ;; Parents
      (user))

  An optional 'override' argument can be set to T to drop the SQL table before
  re-creating it."
  (push name *entity-list*)
  `(corm:defentity ,name ,slots :parents ,parents :override ,override))

(defun single-entity-from-json (entity data)
  (let* ((e (make-instance entity))
         ;; Get all this entity's slots
         (slots (mapcar #'sb-mop:slot-definition-name
                        (sb-mop:class-direct-slots (class-of e)))))
    ;; Loop over all the slots & insert the appropriate data where possible
    (loop for s in slots do
       ;; Get the value of the data & set it to the instance
         (let ((val (getf data (intern (string s) :keyword))))
           (setf (slot-value e s) val)))
    ;; Return the entity
    e))

(defun entity-from-json (entity-parse-clue data)
  "Given an entity name and some parsed json data (i.e. json data in lisp form)
parse that into and instance of the given entity class"
  ;; First, strip [] from the entity if it's around
  (let* ((is-array nil)
        (entity (if (listp entity-parse-clue)
                    (progn (setf is-array t)
                           (car entity-parse-clue))
                    entity-parse-clue)))
    (if is-array
        ;; Parse a whole list
        (loop for e-data in data collect (single-entity-from-json entity e-data))
        ;; Just a single instance, parse the instance
        (single-entity-from-json entity data))))

(defun entity-to-json (e)
  "Given an entity instance, return a list which can be serialised to the appropriate JSON"
  (let* ((res ())
         ;; Get all this entity's slots
         (slots (mapcar #'sb-mop:slot-definition-name
                        (sb-mop:class-direct-slots (class-of e)))))
    ;; Loop over all the slots & extract the appropriate data
    (loop for s in slots do
         (setf res (append res (list (intern (string s) :keyword) (slot-value e s)))))
    ;; Return the serialised value
    res))
