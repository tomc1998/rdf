(in-package :rdf)

(defparameter *entity-list* ())

;; Basically a passthrough to the corm function, but adds the entity meta-data
;; to the *entity-list*
(defmacro defentity (name slots &optional parents override)
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
  `(corm:defentity ,name ,slots ,parents ,override))

(defun entity-from-json (entity data)
  "Given an entity name and some parsed json data (i.e. json data in lisp form)
parse that into and instance of the given entity class"
  ;; Create an instance of the entity
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
